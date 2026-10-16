# R/01_parse.R — parse JSON-encoded array columns into long tibbles
#
# Education and experience arrive as JSON strings inside CSV cells. We explode
# them into long tibbles keyed on `id` so downstream code can dplyr them.

parse_people <- function(people_csv) {
  readr::read_csv(people_csv, show_col_types = FALSE) |>
    dplyr::select(
      id, name, first_name, last_name, url, city, country_code, position, about,
      current_company_company_id, current_company_name,
      educations_details, education, experience,
      followers, connections, location
    )
}

# Safely parse a JSON string into a tibble; return NULL on failure.
.json_to_tbl <- function(s) {
  if (is.na(s) || identical(s, "")) return(NULL)
  out <- tryCatch(jsonlite::fromJSON(s, simplifyDataFrame = TRUE),
                  error = function(e) NULL)
  if (is.null(out) || !length(out)) return(NULL)
  if (is.list(out) && !is.data.frame(out)) out <- tibble::as_tibble(out)
  tibble::as_tibble(out)
}

# ---- education ----

.normalize_degree <- function(x) {
  x <- stringr::str_to_lower(dplyr::coalesce(x, ""))
  dplyr::case_when(
    # POSTDOC IS NOT A DEGREE, and it has to be caught before the PhD branch
    # because "doctoral" is a substring of "postdoctoral" (fixed 2026-07-29).
    # Measured before the fix: 1,795 education rows across 1,591 people whose
    # degree_raw was "Postdoctoral Fellow" or similar normalized to "phd". The
    # damage was not cosmetic — 198 people reached Tier A on the neuro text of a
    # POSTDOC row (so their actual doctorate may not have been neuro), 322
    # people's derive_phd_year() strategy 1 read a postdoc end year as a PhD
    # completion year, and 6 Arm-V people's phd_institution (and hence their
    # program-ranking covariate) was their postdoc institution.
    stringr::str_detect(x, "post-?doc")                                     ~ "postdoc",
    # PhD first so MD-PhD strings ("Doctor of Medicine (MD), PhD") count as PhD.
    # The widened PhD alternatives below do NOT match "doctor of medicine", so a
    # pure MD still falls through to the md branch. Curly + straight apostrophes
    # and several non-English doctorates are covered; professional doctorates
    # (EdD, PsyD, PharmD, JD, DC) correctly fall through to "other".
    stringr::str_detect(x,
      paste0("ph\\.?d|doctor of philosophy|dphil|doctorat|doctoral|",
             "doctor['’]?s degree|doctorado|dottorato|doutorado|doktor")) ~ "phd",
    stringr::str_detect(x, "\\bm\\.?d\\b|doctor of medicine")              ~ "md",
    stringr::str_detect(x, "master|m\\.?sc|m\\.?s\\.|mba|m\\.?eng|m\\.?a\\.") ~ "masters",
    stringr::str_detect(x, "bachelor|b\\.?sc|b\\.?s\\.|b\\.?a\\.|undergrad") ~ "bachelors",
    x == "" ~ NA_character_,
    TRUE ~ "other"
  )
}

.normalize_field <- function(x) {
  x <- stringr::str_to_lower(dplyr::coalesce(x, ""))
  dplyr::case_when(
    # "cognit" alone is NOT core neuro (cognitive psychology / cognitive science /
    # "Cognition and Development" are feeder/Tier-B; see METHODS.md). Genuine
    # "cognitive neuroscience" still lands here via "neuro".
    stringr::str_detect(x, "neuro|brain")        ~ "neuroscience_related",
    stringr::str_detect(x, "psychol")            ~ "psychology",
    stringr::str_detect(x, "biolog|biomed")      ~ "biology_biomed",
    stringr::str_detect(x, "comput|engineer|math|physic") ~ "quant",
    x == "" ~ NA_character_,
    TRUE ~ "other"
  )
}

# Guarantee the named columns exist (as NA character) before they're referenced.
# The BrightData FILTER pull always carries degree/field/title, so this never
# mattered; a SCRAPER (collect-by-URL) pull of thin or stale profiles can return
# education entries with none of them, and the bare column reference then errors
# out mid-parse. Measured 2026-07-22 on a 123-URL scrape: 21 profiles hidden or
# private, and the survivors' education entries frequently omit `degree`/`field`.
.ensure_cols <- function(df, cols) {
  missing <- setdiff(cols, names(df))
  if (length(missing)) df[missing] <- NA_character_
  df
}

# Force a column to character. A JSON column whose values are ALL null parses to
# a LOGICAL NA vector, not a character one, and bind_rows() then refuses to
# combine it with the character column from the other branch ("Can't combine
# <logical> and <character>"). Hit for real on the 2026-07-28 probe, whose 40
# records came back schema-complete with every value null. Cheap insurance on
# every text field rather than only the one that happened to blow up.
.chr <- function(x) if (is.character(x)) x else as.character(x)

# Parse a LinkedIn date string. `lubridate::my()` covers the "Mar 2024" form,
# but LinkedIn also emits a BARE YEAR when the member left the month blank, and
# my() silently returns NA for those.
#
# Measured 2026-07-28 on the rebuilt core store: 55,391 of 381,454 experience
# rows carried a start_date_raw that my() dropped, and 100% of them were bare
# years ("2010", "2008", ...). Same on the end side — 43,233 of 43,254 unparsed
# end_date values. Not a long tail of exotic formats: ONE format, all of it.
#
# The cost of dropping them was not mainly coverage (+118 people gain a
# first_sector in Arm V) but CORRECTNESS: 279 of the 4,073 Arm-V people who
# already had a first_sector get a different one, and the flips run
# systematically toward "Academia — postdoc" (faculty -> postdoc 54,
# Other -> postdoc 34, staff scientist -> postdoc 17). Postdoc start dates are
# disproportionately recorded as bare years, so the unfixed parser was hiding
# the postdoc and promoting the NEXT job to "first post-PhD position".
#
# A bare year is anchored, not invented — `anchor_month` says which end of the
# year to assume. Jan for a start (earliest it could have begun), Dec for an end
# (latest it could have run). The choice is near-inert for ordering: anchoring
# start at Jan vs Dec changes first_sector for only 15 of 4,191 people, because
# the ties it breaks are same-year ones. `*_precision` records which rows were
# anchored so downstream work can tell a real month from an assumed one.
.parse_li_date <- function(x, anchor_month = 1L) {
  out <- suppressWarnings(lubridate::my(x))
  yr  <- suppressWarnings(
    as.integer(stringr::str_match(x, "^\\s*(\\d{4})\\s*$")[, 2]))
  fill <- is.na(out) & !is.na(yr)
  out[fill] <- lubridate::make_date(yr[fill], anchor_month, 1L)
  out
}

# TRUE where .parse_li_date() had to anchor a bare year rather than read a month.
.is_year_only <- function(x) {
  !is.na(x) & stringr::str_detect(dplyr::coalesce(x, ""), "^\\s*\\d{4}\\s*$")
}

parse_education <- function(people) {
  people |>
    dplyr::select(id, education) |>
    dplyr::mutate(parsed = purrr::map(education, .json_to_tbl)) |>
    dplyr::select(-education) |>
    tidyr::unnest(parsed, keep_empty = FALSE) |>
    janitor::clean_names() |>
    .ensure_cols(c("degree", "field", "title", "start_year", "end_year")) |>
    dplyr::mutate(
      degree_raw  = degree,
      degree_norm = .normalize_degree(degree),
      field_raw   = field,
      field_norm  = .normalize_field(field),
      institution = title,
      start_year  = suppressWarnings(as.integer(start_year)),
      end_year    = suppressWarnings(as.integer(end_year))
    ) |>
    dplyr::select(id, degree_raw, degree_norm, field_raw, field_norm,
                  institution, start_year, end_year)
}

# ---- experience ----

# Redaction shapes in a logged-out ("public card") payload. LinkedIn serves the
# top experience card in full and replaces every entry below it with either an
# asterisk mask ("****** ** ********", punctuation and spacing preserved) or a
# bare media URL where the company name should be. Measured 2026-07-31 over the
# 3,311 date-less Arm-V profiles: entry 1 is a real company 100.0% of the time,
# entries 2+ only 0.6% (49.8% masked, 11.2% media URL, 38.4% empty). Detecting
# these is what lets the top entry be used as the current job without the
# padding rows being mistaken for a job history — see join_sector_features().
.is_masked_company <- function(x) {
  !is.na(x) &
    (stringr::str_detect(x, "^[*[:space:],.()&/'-]+$") |
       stringr::str_detect(x, stringr::fixed("licdn.com")))
}

parse_experience <- function(people) {
  raw <- people |>
    dplyr::select(id, experience) |>
    dplyr::mutate(parsed = purrr::map(experience, .json_to_tbl)) |>
    dplyr::select(-experience) |>
    tidyr::unnest(parsed, keep_empty = FALSE) |>
    janitor::clean_names() |>
    # Position in the person's experience ARRAY, captured before the parent/sub
    # split below reorders everything. `.json_to_tbl()` keeps array order, and
    # LinkedIn emits the array most-recent-first, so this is the only place the
    # ordering is still recoverable. Without it the `bind_rows(parent, sub)`
    # further down puts every sub-position after every parent row and the
    # array's meaning is gone.
    dplyr::mutate(entry_idx = dplyr::row_number(), .by = id) |>
    .ensure_cols(c("company", "company_id", "title", "start_date", "end_date",
                   "location", "industry"))

  # Explode `positions` sub-array (multiple titles at one company). An entry
  # with sub-positions is represented ONLY by its exploded rows — keeping the
  # company-level parent row too double-counted the company, and the parent's
  # NA end_date made every such company read as a current job. The parent's
  # industry is carried onto the sub rows so no signal is lost.
  if ("positions" %in% names(raw)) {
    has_pos <- purrr::map_lgl(raw$positions, function(p) {
      if (is.null(p)) return(FALSE)
      if (is.data.frame(p)) return(nrow(p) > 0)
      length(p) > 0
    })
    sub <- raw |>
      dplyr::filter(has_pos) |>
      dplyr::mutate(
        positions = purrr::map(positions, tibble::as_tibble),
        parent_industry = if ("industry" %in% names(raw)) industry else NA_character_
      ) |>
      dplyr::select(id, entry_idx, parent_company = company,
                    parent_company_id = company_id,
                    parent_industry, positions) |>
      tidyr::unnest(positions, names_sep = "_") |>
      janitor::clean_names() |>
      # Sub-positions are themselves most-recent-first within their company.
      dplyr::mutate(sub_idx = dplyr::row_number(), .by = c(id, entry_idx)) |>
      # The sub-array's own columns need the same guarantee the parent gets.
      # A `positions` entry that carries none of these is not hypothetical: the
      # 2026-07-28 re-fetch probe returned 40 profiles whose positions sub-array
      # had no `title` at all, and the bare reference below errored the whole
      # parse out. It survived on the full corpus only because SOME profile
      # supplied every column -- a "runs on the sample, dies on the full data"
      # failure.
      .ensure_cols(c("positions_title", "positions_start_date",
                     "positions_end_date", "positions_location")) |>
      dplyr::transmute(
        id, entry_idx, sub_idx,
        company_raw  = .chr(parent_company),
        company_id   = .chr(parent_company_id),
        title_raw    = .chr(positions_title),
        start_date_raw = .chr(positions_start_date),
        end_date_raw   = .chr(positions_end_date),
        location     = .chr(positions_location),
        industry_raw = .chr(parent_industry)
      )
    parent <- raw |>
      dplyr::filter(!has_pos) |>
      dplyr::transmute(
        id, entry_idx, sub_idx = 1L,
        company_raw  = .chr(company),
        company_id   = .chr(company_id),
        title_raw    = .chr(title),
        start_date_raw = if ("start_date" %in% names(raw)) .chr(start_date) else NA_character_,
        end_date_raw   = if ("end_date" %in% names(raw)) .chr(end_date) else NA_character_,
        location     = if ("location" %in% names(raw)) .chr(location) else NA_character_,
        industry_raw = if ("industry" %in% names(raw)) .chr(industry) else NA_character_
      )
    out <- dplyr::bind_rows(parent, sub)
  } else {
    out <- raw |>
      dplyr::transmute(
        id, entry_idx, sub_idx = 1L,
        company_raw = .chr(company), company_id = .chr(company_id),
        title_raw = .chr(title),
        start_date_raw = .chr(start_date), end_date_raw = .chr(end_date),
        location = .chr(location), industry_raw = .chr(industry)
      )
  }

  out |>
    # Restore the array order the parent/sub split destroyed. `exp_order == 1`
    # is the top card on the person's profile.
    dplyr::arrange(id, entry_idx, sub_idx) |>
    dplyr::mutate(exp_order = dplyr::row_number(), .by = id) |>
    dplyr::mutate(
      # Bare years are anchored to Jan on a start and Dec on an end, so a
      # year-only stint spans the widest interval consistent with the string
      # rather than collapsing to a point. See .parse_li_date().
      start_date = .parse_li_date(start_date_raw, 1L),
      # A missing end date means "ongoing" ONLY if the row carries date
      # information at all. 27,218 collected profiles have experience rows with
      # NO date strings whatsoever (see METHODS.md, "Date-less profiles"), and
      # the old `is.na(end_date_raw)` test read every one of them as a current
      # job: 47.5% of all is_current rows came from those profiles, and 54.5% of
      # people with any current job had MORE THAN ONE. It also moved the
      # top-employer ranking materially on the strength of date-less rows alone.
      # An absent end date is only evidence of "ongoing" against a present
      # start date; with neither, the row says nothing and must not claim to.
      is_current = (is.na(end_date_raw) |
                      stringr::str_detect(dplyr::coalesce(end_date_raw, ""), "Present")) &
                   !(is.na(start_date_raw) & is.na(end_date_raw)),
      # Tied to is_current so a date-less row no longer asserts "ran until
      # today". That mattered beyond the figures: derive_phd_year()'s strategy 2
      # takes max(end_year) over PhD-titled experience, so a date-less
      # "PhD student" row used to contribute end_year = the CURRENT year.
      end_date   = dplyr::if_else(is_current, Sys.Date(),
                                  .parse_li_date(end_date_raw, 12L)),
      start_year = lubridate::year(start_date),
      end_year   = lubridate::year(end_date),
      start_date_precision = dplyr::case_when(
        is.na(start_date)              ~ NA_character_,
        .is_year_only(start_date_raw)  ~ "year",
        TRUE                           ~ "month"
      ),
      end_date_precision = dplyr::case_when(
        is_current                     ~ "current",
        is.na(end_date)                ~ NA_character_,
        .is_year_only(end_date_raw)    ~ "year",
        TRUE                           ~ "month"
      ),
      title_norm = stringr::str_to_lower(title_raw),
      # A padding row from a logged-out payload — no real employer, only a mask
      # or a media URL. Never a job. See .is_masked_company().
      is_redacted_row = .is_masked_company(company_raw) | is.na(company_raw)
    )
}
