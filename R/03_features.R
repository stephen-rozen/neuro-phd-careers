# R/03_features.R — derive per-person features
#
# The PhD-year derivation is the gnarly part. Multiple fallback strategies,
# and we record which one fired in `phd_year_source` for QC.

PHD_TITLE_PATTERNS <- c(
  "ph\\.?d (candidate|student|researcher)",
  "doctoral (candidate|student|researcher)",
  "graduate (student|researcher)",
  "predoctoral"
)

# A PhD spanning more years than this is implausible — usually a stale "present"
# end date (e.g. "2005–present" yields the current year) rather than a real
# completion. Drop such an end_year so it can't produce a bogus phd_year; the
# person falls through to a later strategy or NA.
MAX_PHD_SPAN <- 10L

derive_phd_year <- function(education_long, experience_long) {
  # Strategy 1: end_year on a PhD education entry. Ignore end_years that imply an
  # implausibly long span over the same row's start_year.
  s1 <- education_long |>
    dplyr::filter(degree_norm == "phd", !is.na(end_year),
                  is.na(start_year) | end_year - start_year <= MAX_PHD_SPAN) |>
    dplyr::group_by(id) |>
    dplyr::summarise(phd_year = max(end_year, na.rm = TRUE),
                     phd_year_source = "education_end_year",
                     .groups = "drop")

  # Strategy 2: end_date of PhD-titled experience
  is_phd_title <- function(x) {
    x <- stringr::str_to_lower(dplyr::coalesce(x, ""))
    purrr::map_lgl(x, ~ any(stringr::str_detect(.x, PHD_TITLE_PATTERNS)))
  }
  s2 <- experience_long |>
    dplyr::filter(is_phd_title(title_norm), !is.na(end_year)) |>
    dplyr::group_by(id) |>
    dplyr::summarise(phd_year = max(end_year, na.rm = TRUE),
                     phd_year_source = "experience_phd_title_end",
                     .groups = "drop")

  # Strategy 3: start of the earliest post-PhD-looking role.
  # Heuristic: first experience with "postdoc", "research scientist", "scientist",
  # or any non-academic title that starts after the earliest PhD-title role.
  # The !is.na(start_date) filters are load-bearing: min(na.rm = TRUE) over an
  # all-NA group returns Inf with a warning, and an Inf "date" then flows into
  # the comparison below. It was harmless only by luck (Inf > Inf is FALSE, so
  # those rows dropped out anyway) and it emitted 200 warnings per run. Filter
  # the empty groups out instead of letting them produce sentinel values.
  earliest_phd_start <- experience_long |>
    dplyr::filter(is_phd_title(title_norm), !is.na(start_date)) |>
    dplyr::group_by(id) |>
    dplyr::summarise(min_phd_start = min(start_date), .groups = "drop")

  s3 <- experience_long |>
    dplyr::filter(stringr::str_detect(title_norm,
      "postdoc|post-doctoral|research scientist|^scientist|associate scientist"),
      !is.na(start_date)) |>
    dplyr::group_by(id) |>
    dplyr::summarise(post_phd_start = min(start_date), .groups = "drop") |>
    dplyr::inner_join(earliest_phd_start, by = "id") |>
    dplyr::filter(post_phd_start > min_phd_start) |>
    dplyr::transmute(id, phd_year = lubridate::year(post_phd_start),
                     phd_year_source = "first_postdoc_or_scientist")

  # Layer them: s1 wins, else s2, else s3.
  dplyr::bind_rows(s1, s2, s3) |>
    dplyr::group_by(id) |>
    dplyr::slice(1) |>
    dplyr::ungroup()
}

# Cohort bins. The breaks now come from R/06_figure_settings.R (fixed 1975-2025,
# 2-year width) rather than from the data.
#
# WHY THIS MOVED (2026-07-29): breaks used to be derived from the observed
# min/max of whatever vector was passed in. On Arm V that is 1963-2026, giving
# 31 cohorts — 31 stacked-bar segments and 31 survival strata, some with a
# single person — and, worse, bins that SHIFTED whenever the sample shifted, so
# no two runs were comparable. Years outside the window return NA and are
# excluded rather than squeezed into an end bin.
.cohort_bins <- function(y) cohort_bin(y)

# `people` is OPTIONAL and defaults to NULL so the ProQuest and NeuroTree stores,
# which also call this, keep their measured behaviour unchanged. When supplied,
# the person's CURRENT country rides along from the profile's top-level
# `country_code` (99.95% populated on Arm V, clean ISO-2, 60 distinct values).
#
# NOTE ON SEMANTICS: country_code is where the PROFILE says the person is now,
# not the country of their current employer, and not a derived field. It is the
# "where did they end up" half of a mobility question; the "where did they train"
# half is phd_country, which lives in the ProQuest store's pq_persons and is not
# on the inclusion seam.
build_features <- function(analytic_ids, education_long, experience_long,
                           people = NULL) {
  ids <- unique(analytic_ids)

  # One row per person, even when no PhD year is derivable (NA flows through).
  phd_year <- derive_phd_year(education_long, experience_long)
  base <- tibble::tibble(id = ids) |>
    dplyr::left_join(phd_year, by = "id")

  # PhD institution: prefer the PhD education row; fall back to the person's
  # first education institution when no clean PhD row exists (~8% of collected).
  inst_phd <- education_long |>
    dplyr::filter(degree_norm == "phd", id %in% ids) |>
    dplyr::group_by(id) |>
    dplyr::summarise(inst_phd = dplyr::first(institution), .groups = "drop")
  inst_any <- education_long |>
    dplyr::filter(id %in% ids) |>
    dplyr::group_by(id) |>
    dplyr::summarise(inst_any = dplyr::first(institution), .groups = "drop")
  phd_inst <- inst_phd |>
    dplyr::full_join(inst_any, by = "id") |>
    dplyr::transmute(id, phd_institution = dplyr::coalesce(inst_phd, inst_any))

  current_year <- as.integer(format(Sys.Date(), "%Y"))

  out <- base |>
    dplyr::left_join(phd_inst, by = "id") |>
    dplyr::mutate(
      cohort          = .cohort_bins(phd_year),
      years_since_phd = current_year - phd_year
    )

  if (!is.null(people) && "country_code" %in% names(people)) {
    out <- out |>
      dplyr::left_join(
        people |>
          dplyr::select(id, current_country = country_code) |>
          dplyr::distinct(id, .keep_all = TRUE),
        by = "id")
  } else {
    out$current_country <- NA_character_
  }
  out
}

# One backfill year per person from their matched ProQuest dissertations: the
# closest match's degree_year wins; ties → earliest year. pq_matched already
# carries degree_year (it is `dissertations` left-joined to the match table).
# Shared by the ProQuest-store backfill below and the inclusion-table seam
# (R/12_inclusion.R::write_inclusion_table), so both apply the same rule.
pq_backfill_years <- function(pq_matched) {
  pq_matched |>
    dplyr::filter(match_type != "unmatched", !is.na(matched_id),
                  !is.na(degree_year)) |>
    dplyr::group_by(matched_id) |>
    dplyr::slice_min(name_dist + 0.001 * degree_year, n = 1, with_ties = FALSE) |>
    dplyr::ungroup() |>
    dplyr::transmute(id = matched_id, pq_phd_year = as.integer(degree_year))
}

# Apply a (id, pq_phd_year) year to a feature table, recomputing cohort and
# years_since_phd. `source_label` names the provenance written to
# phd_year_source.
#
# `override = TRUE` makes the supplied year WIN wherever it exists;
# `override = FALSE` (the default) only fills gaps.
#
# ProQuest is the gold standard. For Arm V it is called with
# override = TRUE, because every Arm-V member by definition has a matched
# dissertation carrying a real awarded-degree year, and the LinkedIn-derived
# year is a heuristic over free text. Evidence for preferring it: on the 2,037
# Arm-V people who had both, the two agreed exactly 79.0% of the time and 100%
# within +/-1 year — so the override is low-risk where they overlap — while the
# LinkedIn side is the one that produces outright errors, dating 162 people who
# are STILL DOING THEIR PhD to a completed 2026 degree.
#
# The NeuroTree grad-year seam still uses override = FALSE: it is a weaker
# signal than a dissertation record and belongs in the gap-filling role.
.apply_phd_year_backfill <- function(features, pq_year,
                                     source_label = "proquest_degree_year",
                                     override = FALSE) {
  current_year <- as.integer(format(Sys.Date(), "%Y"))
  features |>
    dplyr::left_join(pq_year |> dplyr::select(id, bf_year = pq_phd_year),
                     by = "id") |>
    dplyr::mutate(
      applied         = if (override) !is.na(bf_year)
                        else is.na(phd_year) & !is.na(bf_year),
      phd_year        = dplyr::if_else(applied, bf_year, phd_year),
      phd_year_source = dplyr::if_else(applied, source_label, phd_year_source),
      cohort          = .cohort_bins(phd_year),
      years_since_phd = current_year - phd_year
    ) |>
    dplyr::select(-bf_year, -applied)
}

# Cap phd_year at the analysis window (PHD_YEAR_MAX, R/06_figure_settings.R).
#
# A year beyond the last complete year is an in-progress degree, not a
# completed one, and it arrives two ways: an education row carrying an EXPECTED
# end year, and — the one that actually bit — a still-current "PhD Candidate"
# experience row, whose end_date is set to today by parse_experience() and then
# read as a completion year by derive_phd_year() strategy 2. Measured 2026-07-29
# before this cap: 244 Arm-V people dated to 2026, 162 of them with a live
# PhD-student row.
#
# The person is DROPPED from the analytic sample rather than having phd_year
# NA'd, because "we don't know when they finished" and "they haven't finished"
# are different states and only the first belongs in a cohort analysis.
drop_incomplete_phd_years <- function(features, max_year = PHD_YEAR_MAX) {
  n_before <- nrow(features)
  out <- features |> dplyr::filter(!is.na(phd_year), phd_year <= max_year)
  if (n_before > nrow(out)) {
    message(sprintf("phd_year cap at %d: dropped %d of %d people.",
                    max_year, n_before - nrow(out), n_before))
  }
  out
}

# Backfill phd_year from a matched ProQuest dissertation's degree_year for people
# who have no LinkedIn-derivable year. Runs AFTER matching (no cycle with the
# year-based match path). Recomputes cohort / years_since_phd and tags the source
# as "proquest_degree_year".
backfill_phd_year <- function(features, pq_matched, dissertations) {
  .apply_phd_year_backfill(features, pq_backfill_years(pq_matched))
}
