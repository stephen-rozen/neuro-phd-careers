# R/00_qc.R — data quality checks on raw inputs

# Takes the ASSEMBLED `people` and `companies` tibbles, not file paths.
#
# It used to re-read data/LinkedIn people profiles.csv and
# data/LinkedIn company information.csv directly, which meant it was measuring
# the delivered 1,000-row company SAMPLE while the pipeline joined the
# 11,935-row pulled table. That is why it reported company_rows = 1000 and
# current_company_id_join_rate = 0.000 long after the company pull took the
# real join to 99.4% — a stale QC input masquerading as a broken join. Reading the targets keeps QC pointed at what the pipeline uses.
run_qc_raw <- function(people, companies, experience_long = NULL) {
  ppl <- people
  co  <- companies

  parses <- function(x) {
    vapply(x, function(s) {
      if (is.na(s) || s == "") return(FALSE)
      ok <- tryCatch({ jsonlite::fromJSON(s); TRUE }, error = function(e) FALSE)
      ok
    }, logical(1))
  }

  edu_ok <- parses(ppl$education)
  exp_ok <- parses(ppl$experience)

  out <- tibble::tibble(
    metric = c(
      "people_rows", "people_distinct_ids", "people_duplicate_ids",
      "education_parse_rate", "experience_parse_rate",
      "country_codes_distinct",
      "company_rows", "company_distinct_ids",
      "people_with_current_company_id",
      "current_company_id_join_rate"
    ),
    value = c(
      nrow(ppl),
      dplyr::n_distinct(ppl$id),
      sum(duplicated(ppl$id)),
      round(mean(edu_ok), 3),
      round(mean(exp_ok), 3),
      dplyr::n_distinct(ppl$country_code),
      nrow(co),
      dplyr::n_distinct(co$id),
      sum(!is.na(ppl$current_company_company_id)),
      round(mean(ppl$current_company_company_id %in% co$id, na.rm = TRUE), 3)
    )
  )

  # Experience dates. `experience_parse_rate` above only says the JSON parsed —
  # it says nothing about whether the entries carry usable dates, and a payload
  # with no date keys at all parses perfectly while being useless for
  # first_sector. These three make that visible in QC instead of only in an
  # ad-hoc script (2026-07-28): 27,218 collected people had experience rows
  # with no date string anywhere.
  if (!is.null(experience_long) && nrow(experience_long)) {
    ex <- experience_long
    per <- ex |>
      dplyr::group_by(id) |>
      dplyr::summarise(n_date_str = sum(!is.na(start_date_raw)), .groups = "drop")
    out <- dplyr::bind_rows(out, tibble::tibble(
      metric = c("experience_rows",
                 "start_date_string_rate",
                 "start_date_parse_rate",
                 "start_date_year_only_share",
                 "people_with_experience_rows",
                 "people_with_no_dated_experience"),
      value = c(
        nrow(ex),
        round(mean(!is.na(ex$start_date_raw)), 3),
        # of the rows that HAVE a date string, how many yield a date
        round(sum(!is.na(ex$start_date)) / max(sum(!is.na(ex$start_date_raw)), 1L), 3),
        round(mean(ex$start_date_precision %in% "year", na.rm = TRUE), 3),
        nrow(per),
        sum(per$n_date_str == 0)
      )))
  }
  out
}

# Turn the raw QC metrics into an issues frame — empty when every check passes.
# Thresholds are deliberately coarse: they exist to catch a broken ingest, not
# to grade data quality.
qc_issues <- function(qc) {
  get <- function(m) {
    v <- qc$value[qc$metric == m]
    if (length(v)) v[[1]] else NA_real_
  }
  checks <- tibble::tribble(
    ~issue,                              ~value,                                 ~bad,
    "duplicate_people_ids",              get("people_duplicate_ids"),            get("people_duplicate_ids") > 0,
    "education_parse_rate_below_0.95",   get("education_parse_rate"),            get("education_parse_rate") < 0.95,
    "experience_parse_rate_below_0.95",  get("experience_parse_rate"),           get("experience_parse_rate") < 0.95,
    # NOT a coverage check. The company pull is deliberately SCOPED (Tier-A
    # people's current employers, moving to Arm V) so most of the 94,860
    # collected people's employers were never bought -- 0.177 over the whole
    # universe is by design, not a defect, and a threshold like 0.5 just fires
    # on the scoping. What this check is actually for is the failure it missed
    # for months: the company table being empty, stale or the wrong file.
    "current_company_id_join_rate_zero", get("current_company_id_join_rate"), get("current_company_id_join_rate") <= 0,
    # Catches a NEW unparseable date format arriving in the full corpus. The
    # bare-year fallback took this to 1.000 on the sample; anything below 0.99
    # means a format .parse_li_date() doesn't know about.
    "start_date_parse_rate_below_0.99",  get("start_date_parse_rate"),  get("start_date_parse_rate") < 0.99
  )
  checks |>
    dplyr::filter(!is.na(bad) & bad) |>
    dplyr::select(issue, value)
}

# Pipeline completion check: tar_make() reached this target error-free, every
# required figure exists, and the QC issues frame is empty OR every remaining
# issue is documented in output/tables/qc_known_issues.csv.
verify_done <- function(figures_dir, tables_dir, qc) {
  # fig_geo_flows.png removed 2026-07-29 — see R/05_plots.R for why. It was a
  # stub PNG, so listing it here made the stopping condition pass on a filename
  # rather than on a figure.
  required_figs <- c(
    "fig_filter_funnel.png", "fig_cohort_size.png",
    "fig_first_sector_by_cohort.png", "fig_current_sector_by_cohort.png",
    "fig_first_subsector_by_cohort.png", "fig_current_subsector_by_cohort.png",
    "fig_time_to_industry_km.png",
    "fig_neurotech_share.png", "fig_top_employers.png",
    # Added 2026-07-30: first sector split by dissertation subfield.
    "fig_first_sector_by_subfield.png",
    # Added 2026-07-29: the prestige covariate finally has a consumer, and the
    # postdoc-duration figure is the lead analytical addition.
    "fig_prestige_by_sector.png", "fig_postdoc_duration.png",
    # Added 2026-07-30 with the NIH funding feature (R/24_grants.R). Its
    # denominator is the NeuroTree-matched subset of Arm V, not Arm V.
    "fig_grant_by_cohort.png",
    # Added 2026-07-31. AI/ML is title-matched, not industries-matched — see
    # METHODS.md; postdoc-first-job needed no new classification.
    "fig_ai_ml_share_by_cohort.png", "fig_postdoc_first_job_share.png"
  )
  present <- fs::dir_ls(figures_dir, glob = "*.png") |> fs::path_file()
  missing <- setdiff(required_figs, present)
  if (length(missing)) {
    stop("Pipeline incomplete. Missing figures: ",
         paste(missing, collapse = ", "))
  }

  issues <- qc_issues(qc)
  if (nrow(issues)) {
    known_path <- fs::path(tables_dir, "qc_known_issues.csv")
    known <- if (fs::file_exists(known_path)) {
      readr::read_csv(known_path, show_col_types = FALSE)$issue
    } else {
      character()
    }
    undocumented <- setdiff(issues$issue, known)
    if (length(undocumented)) {
      stop("QC issues not documented in ", known_path, ": ",
           paste(undocumented, collapse = ", "))
    }
    message("✓ ", nrow(issues), " QC issue(s), all documented in qc_known_issues.csv.")
  } else {
    message("✓ QC clean.")
  }

  message("✓ All ", length(required_figs), " figures present.")
  invisible(TRUE)
}
