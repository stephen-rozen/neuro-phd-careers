# R/22_ucot.R — UCOT-Exp2 workforce sector + career type
#
# The citable spine for sector reporting. See METHODS.md, "Sector
# classification", for the crosswalk rationale.
#
# Taxonomy: Unified Career Outcomes Taxonomy, experimentally validated version
# (UCOT-Exp2). Stayart et al. (2020) F1000Research 9:8,
# doi:10.12688/f1000research.21641.1. Coding key with the official numeric
# codes: data/dictionaries/ucot_exp2_coding_key.csv (OSF 5cguj, CC-BY).
#
# Deliberately does NOT live in R/sector_map.R. That file owns the
# project-local `industry` axis (LinkedIn `industries` -> our subsectors); this
# file owns the two standard axes derived from it. Keeping them apart is also
# what lets the subsector regexes be edited without touching the taxonomy.
#
# All functions are pure: vector in, vector out, no side effects.

# The six workforce sectors and six career types, in coding-key order. Codes
# are UCOT's own, so a reader can join our output to the published key.
UCOT_SECTOR_LEVELS <- c("Academia", "Government", "For-Profit",
                        "Nonprofit", "Other", "Unknown")

UCOT_CAREER_TYPE_LEVELS <- c("Primarily Research", "Primarily Teaching",
                             "Science-related", "Not Related to Science",
                             "Further Training or Education", "Unknown")

# Employers whose staff scientists are GOVERNMENT, not Academia. Project
# decision (METHODS.md, "Sector classification"): intramural government
# scientists are a separate cell from university staff scientists. Under UCOT
# this needs no new category -- `Group leader or Principal Investigator` is
# defined as leading a team "in any research environment", i.e. it is
# sector-blind, so the split lives here on the sector axis.
#
# Matched against the EMPLOYER name, never against `industries`: a LinkedIn
# industry string cannot distinguish an NIH lab from a university lab.
GOV_INTRAMURAL_PATTERNS <- paste(
  "\\bnih\\b", "national institutes? of health",
  "\\bnin(ds|ds)\\b", "\\bnida\\b", "\\bnimh\\b", "\\bninds\\b",
  "\\bnia\\b", "\\bnci\\b", "\\bniehs\\b", "\\bnhgri\\b", "\\bniaid\\b",
  "national institute of",
  "\\bnsf\\b", "national science foundation",
  "\\bfda\\b", "food and drug administration",
  "\\bcdc\\b", "centers for disease control",
  "veterans affairs", "\\bva medical", "\\bvamc\\b",
  "walter reed", "\\bnist\\b", "\\bnasa\\b",
  # DOE national labs that employ neuro/bio PhDs
  "national laboratory", "national labs?\\b",
  sep = "|"
)

# Clinical training. The project's decision -- residency counts as ACADEMIA --
# is also the standard: CNGLS defines `Further training or education` as
# "Temporary training position including postdoctoral appointments, medical
# residencies & graduate school", and its Academia sector covers the
# university medical centers that employ residents.
CLINICAL_TRAINING_PATTERNS <- paste(
  "\\bresident\\b", "\\bresidency\\b", "\\bhouse ?staff\\b",
  "clinical fellow", "\\bfellowship in\\b", "\\bpgy-?[0-9]",
  sep = "|"
)

POSTDOC_PATTERNS <- "postdoc|post[- ]?doctoral"

TEACHING_TITLE_PATTERNS <- paste(
  "lecturer", "instructor", "\\bteaching\\b", "professor of practice",
  "adjunct", "\\bpreceptor\\b",
  sep = "|"
)

RESEARCH_TITLE_PATTERNS <- paste(
  "scientist", "\\bresearch\\b", "principal investigator", "\\bp\\.?i\\.?\\b",
  "group leader", "investigator", "\\br ?& ?d\\b", "laborator",
  sep = "|"
)

# Roles that support science without conducting it -- UCOT `Science-related`.
SCIENCE_RELATED_PATTERNS <- paste(
  "medical science liaison", "\\bmsl\\b", "regulatory", "clinical operations",
  "clinical research", "product manager", "program officer", "program director",
  "consultant", "consulting", "patent", "medical writer", "science writer",
  "medical affairs", "\\bbioinformatic", "data scientist", "science policy",
  "technology transfer", "grants? manager", "publication",
  sep = "|"
)

.lower <- function(x) stringr::str_to_lower(dplyr::coalesce(x, ""))

is_gov_intramural_employer <- function(company_name) {
  stringr::str_detect(.lower(company_name), GOV_INTRAMURAL_PATTERNS)
}

is_clinical_training_title <- function(title) {
  t <- .lower(title)
  # "Fellow" alone is also a senior INDUSTRY rank ("Distinguished Fellow"), so
  # only the explicitly clinical//training spellings above count. A bare
  # "Research Fellow" is caught by the postdoc branch instead.
  stringr::str_detect(t, CLINICAL_TRAINING_PATTERNS)
}

#' Map our project-local subsector to a UCOT-Exp2 workforce sector.
#'
#' Returns a character vector of `UCOT_SECTOR_LEVELS`. Ambiguous cells resolve
#' on the employer name where they can and fall to "Unknown" where they cannot
#' -- never to a guess. Pair with `ucot_sector_basis()` for the audit trail.
#'
#' @param sector Our subsector (`subsector_lookup_table$subsector` values) —
#'   named `sector` for parity with `ucot_career_type()`'s signature, not
#'   because it takes the four-level `sector` column.
#' @param title Raw job title. Used only for clinical-training rescue.
#' @param company_name Employer name. Used only to split the genuinely
#'   ambiguous buckets.
ucot_workforce_sector <- function(sector, title = NA_character_,
                                  company_name = NA_character_) {
  s <- dplyr::coalesce(sector, NA_character_)
  gov <- is_gov_intramural_employer(company_name)
  clin <- is_clinical_training_title(title)

  out <- dplyr::case_when(
    # Clinical training outranks the industry read: a resident at a hospital is
    # Academia, not Healthcare delivery. Checked first for that reason.
    clin                                        ~ "Academia",
    # An intramural government employer outranks the academic read, which is
    # the whole point of the separate-subsector decision.
    gov                                         ~ "Government",
    stringr::str_detect(s, "^Academia")         ~ "Academia",
    stringr::str_detect(s, "^Government —")     ~ "Government",
    s == "Defense"                              ~ "For-Profit",
    # Genuinely two UCOT sectors merged into one of our buckets. If the
    # employer name did not resolve it above, we do not know.
    s == "Government/Nonprofit"                 ~ "Unknown",
    # US community hospitals are plurality nonprofit (~58%), so this is the
    # least-wrong default, but it IS a default -- flagged in the basis column
    # and stated as a limitation in METHODS.md.
    s == "Healthcare delivery"                  ~ "Nonprofit",
    # K-12 is explicitly NOT Academia under UCOT; sector follows school
    # ownership, which `industries` does not carry.
    s == "Education (non-university)"           ~ "Unknown",
    s == "Other"                                ~ "Other",
    is.na(s)                                    ~ "Unknown",
    # Everything remaining is a commercial industry bucket.
    TRUE                                        ~ "For-Profit"
  )
  factor(out, levels = UCOT_SECTOR_LEVELS)
}

#' Why `ucot_workforce_sector()` returned what it did (QC column).
#'
#' Mirrors the `phd_year_source` / `neuro_basis` convention: every assignment
#' records its evidence so a reviewer can audit the defaults separately from
#' the determinate calls.
ucot_sector_basis <- function(sector, title = NA_character_,
                              company_name = NA_character_) {
  s <- dplyr::coalesce(sector, NA_character_)
  dplyr::case_when(
    is_clinical_training_title(title)        ~ "clinical_training_title",
    is_gov_intramural_employer(company_name) ~ "gov_intramural_employer",
    stringr::str_detect(s, "^Academia")      ~ "academic_subsector",
    stringr::str_detect(s, "^Government —")  ~ "gov_subsector",
    s == "Government/Nonprofit"              ~ "gov_nonprofit_unresolved",
    s == "Healthcare delivery"               ~ "assumed_nonprofit_hospital",
    s == "Education (non-university)"        ~ "k12_ownership_unknown",
    s == "Other"                             ~ "other_subsector",
    is.na(s)                                 ~ "no_subsector",
    TRUE                                     ~ "commercial_subsector"
  )
}

#' Map subsector + title to a UCOT-Exp2 career type.
#'
#' This axis is a function of TITLE, not of `industries` -- an industry string
#' cannot tell a bench scientist from a VP of business development at the same
#' company. Expect this to be the noisier of the two axes; an inter-rater
#' (kappa) validation should precede any publication of it.
ucot_career_type <- function(sector, title = NA_character_) {
  s <- dplyr::coalesce(sector, NA_character_)
  t <- .lower(title)

  out <- dplyr::case_when(
    stringr::str_detect(t, POSTDOC_PATTERNS)      ~ "Further Training or Education",
    is_clinical_training_title(t)                 ~ "Further Training or Education",
    s == "Academia — postdoc"                     ~ "Further Training or Education",
    # UCOT-Exp2 splits faculty into `Group leader or PI` vs `Teaching faculty
    # or staff`; the career-type axis carries the same distinction.
    stringr::str_detect(t, TEACHING_TITLE_PATTERNS) ~ "Primarily Teaching",
    s == "Education (non-university)"             ~ "Primarily Teaching",
    stringr::str_detect(s, "^Academia")           ~ "Primarily Research",
    stringr::str_detect(s, "^Government —")       ~ "Primarily Research",
    stringr::str_detect(t, RESEARCH_TITLE_PATTERNS) ~ "Primarily Research",
    stringr::str_detect(t, SCIENCE_RELATED_PATTERNS) ~ "Science-related",
    s == "Healthcare delivery"                    ~ "Science-related",
    s %in% c("Consulting", "Finance")             ~ "Science-related",
    s == "Other" | is.na(s)                       ~ "Unknown",
    t == ""                                       ~ "Unknown",
    TRUE                                          ~ "Not Related to Science"
  )
  factor(out, levels = UCOT_CAREER_TYPE_LEVELS)
}

#' Attach both UCOT axes plus the sector basis to a sectored experience table.
#'
#' Expects the columns `sector_map.R`'s classifier produces (`subsector`,
#' `title_raw`) plus an employer name. `company_col` is optional so this still
#' runs on tables that have not been joined to `companies`.
#'
#' TAKES `subsector`, NOT `sector` (fixed on wiring, 2026-07-30). Every
#' `case_when` branch in `ucot_workforce_sector()` / `ucot_career_type()`
#' matches subsector-level strings ("Defense", "Healthcare delivery",
#' "Government — intramural research", ...) — our four-level `sector` collapses
#' those distinctions away before this function ever saw them, so calling it
#' with `sector` silently mis-sorted every Government row into "For-Profit"
#' (the catch-all), because "Government" does not match the `"^Government —"`
#' pattern the branch actually checks. Caught before this was ever consumed by
#' a figure.
attach_ucot <- function(sectored_exp, company_col = "company_name") {
  nm <- if (company_col %in% names(sectored_exp)) {
    sectored_exp[[company_col]]
  } else {
    rep(NA_character_, nrow(sectored_exp))
  }

  sectored_exp |>
    dplyr::mutate(
      ucot_sector       = ucot_workforce_sector(subsector, title_raw, nm),
      ucot_sector_basis = ucot_sector_basis(subsector, title_raw, nm),
      ucot_career_type  = ucot_career_type(subsector, title_raw)
    )
}
