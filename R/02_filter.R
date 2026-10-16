# R/02_filter.R — funnel the sample down to neuro PhDs
#
# Each stage records a count + the IDs that survived, so the funnel itself
# becomes a deliverable (filter_funnel.csv).

# Bare "neural" is deliberately absent. It matched
# "artificial neural networks" — 113 education rows, e.g. "Aircraft fatigue
# monitoring systems using artificial neural networks" — and carried 19 Arm-V
# people into neuro-doctoral status on an only-"neural" match. The specific
# compounds below keep the genuine programs ("Computation and Neural Systems",
# NYU's "Neural Science", neural engineering) without the ML false positives.
# Mirrors SELFID_NEURO_PATTERNS in R/12_inclusion.R, which already avoided bare
# "neural" for exactly this reason — the two lists were inconsistent.
NEURO_PATTERNS <- c(
  "neurosci", "neurobio", "neuro-?engineer",
  "neural (science|systems|circuit|engineer)",
  "computational neuroscience",
  "systems neuroscience", "behavioral neuroscience",
  # core ProQuest neuro subjects the field parser otherwise routes to "psychology"
  # (cognitive psychology is handled separately and tagged — see EDU_COGPSYCH_PATTERNS)
  "physiological psychology", "psychobiolog", "cognitive neuro",
  "behavioral neuro", "neuropsych"
)

# Scans field_raw + degree_raw + institution: LinkedIn frequently puts the field
# in the degree string ("PHD in Cognitive Psychology") or the program/institution
# name ("Interdepartmental Neuroscience PhD Program"), not in field_raw.
is_neuro <- function(field_raw, field_norm, institution = NULL, degree_raw = NULL) {
  text <- stringr::str_to_lower(paste(dplyr::coalesce(field_raw, ""),
                                      dplyr::coalesce(degree_raw, ""),
                                      dplyr::coalesce(institution, "")))
  # Hard veto. A machine-learning degree can still reach field_norm
  # "neuroscience_related" through .normalize_field()'s "neuro|brain" rule
  # ("Neural Networks" does not, but "Neuro-fuzzy systems" would), so the veto
  # has to sit outside the pattern list rather than inside it.
  ml_only <- stringr::str_detect(text, "neural network") &
    !stringr::str_detect(text, "neurosci|neurobio|neurolog|neuropsych")
  (field_norm %in% c("neuroscience_related") |
     purrr::map_lgl(text, ~ any(stringr::str_detect(.x, NEURO_PATTERNS)))) &
    !ml_only
}

# A row is a doctorate when the degree parsed to PhD, OR (when it didn't parse at
# all) the degree/program text itself names a doctorate. The is.na guard means a
# row that parsed to MD/masters/other is NOT rescued — only genuinely unparsed
# rows (e.g. "Interdepartmental Neuroscience PhD Program" with a blank degree).
DOCTORAL_TEXT_RE <- "ph\\.?d|d\\.?phil|doctor|doctoral|\\bsc\\.?d\\b"
is_doctoral_row <- function(degree_norm, degree_raw = NULL, institution = NULL) {
  txt <- stringr::str_to_lower(paste(dplyr::coalesce(degree_raw, ""),
                                     dplyr::coalesce(institution, "")))
  (!is.na(degree_norm) & degree_norm == "phd") |
    (is.na(degree_norm) & stringr::str_detect(txt, DOCTORAL_TEXT_RE))
}

# Cognitive psychology (ProQuest 0633) in education: counts (Tier A) but is
# tagged, mirroring the ProQuest cog-psych rule. Kept OUT of NEURO_PATTERNS so it
# never silently reads as core neuro.
EDU_COGPSYCH_PATTERNS <- c("cognitive psychology")
is_edu_cogpsych <- function(field_raw, degree_raw = NULL) {
  txt <- stringr::str_to_lower(paste(dplyr::coalesce(field_raw, ""),
                                     dplyr::coalesce(degree_raw, "")))
  purrr::map_lgl(txt, ~ any(stringr::str_detect(.x, EDU_COGPSYCH_PATTERNS)))
}

build_filter_funnel <- function(people, education_long, experience_long) {
  s0 <- tibble::tibble(stage = "start", id = people$id)

  s1_ids <- education_long |>
    dplyr::filter(degree_norm == "phd", is_neuro(field_raw, field_norm, institution)) |>
    dplyr::pull(id) |> unique()
  s1 <- tibble::tibble(stage = "has_neuro_phd", id = s1_ids)

  phd_year <- derive_phd_year(education_long, experience_long)
  s2_ids <- phd_year |> dplyr::filter(!is.na(phd_year)) |> dplyr::pull(id) |> intersect(s1_ids)
  s2 <- tibble::tibble(stage = "phd_year_derivable", id = s2_ids)

  s3_ids <- experience_long |>
    dplyr::filter(!is.na(start_date)) |>
    dplyr::pull(id) |> unique() |> intersect(s2_ids)
  s3 <- tibble::tibble(stage = "has_dated_experience", id = s3_ids)

  final <- tibble::tibble(stage = "final", id = s3_ids)

  dplyr::bind_rows(s0, s1, s2, s3, final) |>
    dplyr::group_by(stage) |>
    dplyr::mutate(n = dplyr::n()) |>
    dplyr::ungroup()
}
