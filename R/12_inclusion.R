# R/12_inclusion.R — tiered, OR-of-evidence inclusion for "neuro PhD"
#
# Supersedes the single education-field gate (R/02_filter.R) as the inclusion
# definition. Collection is deliberately wide — BrightData pulls any PhD with a
# neuro/feeder signal anywhere on the profile — and this function decides who
# actually counts.
#
# Evidence tiers:
#   A (include outright): neuro in education  OR  a matched ProQuest dissertation
#                         classed neuro (by the direct name matcher OR via the
#                         NeuroTree dissertation-title bridge).
#   B (adjudicate):       self-ID neuro in headline/about alone, neuro only in
#                         experience, a feeder field (biomedical engineering,
#                         bioengineering, physiology, ...) in education, OR a
#                         NeuroTree neuro-tree affiliation with no dissertation
#                         corroboration. Counted ONLY if ProQuest corroborates
#                         (which lifts the person into Tier A via pq_neuro);
#                         otherwise held for human review — NOT counted.
#   C (excluded):         no neuro signal anywhere.
#
# ProQuest and NeuroTree are positive-evidence paths and Tier-B adjudicators —
# NOT gates. A non-match on either never removes a Tier-A person, so overall
# recall stays pinned to LinkedIn while their precision is used only where
# LinkedIn can't decide.
#
# WHY NEUROTREE MEMBERSHIP IS ONLY TIER B (measured 2026-07-22; worth
# re-measuring before relaxing). `people.majorarea == "neuro"` looked like
# advisor-attested evidence of a neuroscience PhD. It isn't: tree membership
# PROPAGATES DOWN THE ADVISOR LINEAGE. An advisor whose majorarea lists both
# "neuro" and an unrelated field passes "neuro" to trainees whose own `area`
# and dissertations lie entirely in that other field. Of 1,127 people a
# bare-grad-row Tier-A rule would have added, only 42 had ANY independent neuro
# evidence (ProQuest subject, dissertation-title text, or NeuroTree `area` text) —
# 96% were false positives.
#
# So the Tier-A path is nt_pq_neuro ONLY: the person's dissertation was reached
# through the NeuroTree title bridge and pq_is_neuro() classed it neuro. That is
# the ProQuest taxonomy deciding — NeuroTree
# supplies a new route to the dissertation, not a new definition of neuro.

# PhD years beyond this are expected (not completed) degrees — excluded from the
# sample. The current calendar year: a degree dated after today hasn't happened.
FUTURE_PHD_CUTOFF <- as.integer(format(Sys.Date(), "%Y"))

# Feeder fields: real neuro pipelines but mostly non-neuro on their own, so they
# need ProQuest corroboration. Mirrors NEURO_FEEDER_CODES in R/07_proquest.R.
FEEDER_FIELD_PATTERNS <- c(
  "biomedical engineer", "bioengineer", "biomedical scien", "biomedical",
  "physiolog", "pharmacolog", "molecular biolog", "biophysic", "bioinformatic"
)

# Tight self-ID pattern for the auto-include tier. Deliberately avoids bare
# "neural" so it doesn't fire on "neural network" (ML headlines).
SELFID_NEURO_PATTERNS <- c(
  "neurosci", "neuroscientist", "neurobiolog", "neurolog",
  "computational neuro", "cognitive neuro", "behavioral neuro",
  "neural circuit", "brain scien", "neuroimag", "neural engineer"
)

# Looser pattern for the experience tier — which is ProQuest-adjudicated anyway,
# so a little noise here is harmless.
EXP_NEURO_PATTERNS <- c(
  "neurosci", "neurobiolog", "neurolog", "neuro lab", "neuro depart",
  "brain ", "cortex", "hippocamp", "neural circuit", "neuroimag",
  "neuropharm", "neurotech", "neuralink"
)

is_feeder <- function(field_raw, field_norm = NULL) {
  text <- stringr::str_to_lower(dplyr::coalesce(field_raw, ""))
  purrr::map_lgl(text, ~ any(stringr::str_detect(.x, FEEDER_FIELD_PATTERNS)))
}

.detect_any <- function(text, patterns) {
  text <- stringr::str_to_lower(dplyr::coalesce(text, ""))
  purrr::map_lgl(text, ~ any(stringr::str_detect(.x, patterns)))
}

# Broad PhD base over whatever was collected: EVERY collected person. BrightData
# already gated collection to PhD-indicative degree substrings, so the universe
# is all collected profiles — not just those with a clean normalized PhD row or a
# derivable year (~11% lack one, and ~2k of those still carry a neuro signal).
# Tiered inclusion narrows from here; Tier C drops anyone with no neuro signal.
# (cf. R/02_filter.R, which pre-gates to education-neuro.)
broad_phd_base <- function(people, education_long, experience_long) {
  unique(people$id)
}

build_inclusion <- function(features, education_long, experience_long,
                            people, pq_raw, pq_matched, nt_link = NULL) {
  ids <- unique(features$id)

  # Doctoral education rows: degree parsed to PhD, or (if unparsed) the degree/
  # program text names a doctorate. Neuro evidence scans field_raw + degree_raw +
  # institution, so a neuro signal in the degree string or program name counts.
  edu_doc <- education_long |>
    dplyr::mutate(.doc = is_doctoral_row(degree_norm, degree_raw, institution))

  edu_neuro_ids <- edu_doc |>
    dplyr::filter(.doc, is_neuro(field_raw, field_norm, institution, degree_raw)) |>
    dplyr::pull(id) |> unique()

  edu_cogpsych_ids <- edu_doc |>
    dplyr::filter(.doc, is_edu_cogpsych(field_raw, degree_raw)) |>
    dplyr::pull(id) |> unique()

  edu_feeder_ids <- edu_doc |>
    dplyr::filter(.doc, is_feeder(field_raw, field_norm)) |>
    dplyr::pull(id) |> unique()

  selfid_ids <- people |>
    dplyr::filter(.detect_any(paste(dplyr::coalesce(position, ""),
                                    dplyr::coalesce(about, "")),
                              SELFID_NEURO_PATTERNS)) |>
    dplyr::pull(id) |> unique()

  exp_neuro_ids <- experience_long |>
    dplyr::filter(.detect_any(paste(dplyr::coalesce(title_norm, ""),
                                    dplyr::coalesce(company_raw, "")),
                              EXP_NEURO_PATTERNS)) |>
    dplyr::pull(id) |> unique()

  # Classify with the full audit trail, then join to matched links.
  # ev = one row per (matched person, neuro diss).
  matched_links <- pq_matched |>
    dplyr::filter(match_type != "unmatched", !is.na(matched_id)) |>
    dplyr::select(pq_id, matched_id)
  ev <- pq_classify_neuro(pq_raw) |>
    dplyr::filter(pq_neuro) |>
    dplyr::select(pq_id, neuro_basis, cogpsych_any) |>
    dplyr::inner_join(matched_links, by = "pq_id")

  pq_neuro_ids <- unique(ev$matched_id)
  pq_cogpsych_ids <- ev |>
    dplyr::filter(cogpsych_any) |>
    dplyr::pull(matched_id) |> unique()

  # NeuroTree evidence (optional seam — absent/stub link table is a no-op, which
  # keeps this a pure ADDITION to the OR: nobody can be dropped by it).
  nt_affil_ids <- character(0)
  nt_pq_neuro_ids <- character(0)
  if (!is.null(nt_link) && nrow(nt_link) > 0) {
    nt_affil_ids <- nt_link |>
      dplyr::filter(dplyr::coalesce(nt_neuro_grad, FALSE) |
                      dplyr::coalesce(nt_neuro_postdoc, FALSE)) |>
      dplyr::pull(id) |> unique()
    if ("nt_pq_neuro" %in% names(nt_link)) {
      nt_pq_neuro_ids <- nt_link |>
        dplyr::filter(dplyr::coalesce(nt_pq_neuro, FALSE)) |>
        dplyr::pull(id) |> unique()
    }
  }

  # phd_year > current year = an in-progress PhD listing an EXPECTED completion
  # date (LinkedIn education_end_year). Those people aren't PhD holders yet, so
  # they're excluded from the sample regardless of neuro evidence.
  future_ids <- features |>
    dplyr::filter(!is.na(phd_year), phd_year > FUTURE_PHD_CUTOFF) |>
    dplyr::pull(id) |> unique()

  tibble::tibble(id = ids) |>
    dplyr::mutate(
      edu_neuro    = id %in% edu_neuro_ids,
      edu_cogpsych = id %in% edu_cogpsych_ids,
      selfid_neuro = id %in% selfid_ids,
      pq_neuro     = id %in% pq_neuro_ids,
      pq_cogpsych  = id %in% pq_cogpsych_ids,
      exp_neuro    = id %in% exp_neuro_ids,
      edu_feeder   = id %in% edu_feeder_ids,
      nt_affil     = id %in% nt_affil_ids,
      nt_pq_neuro  = id %in% nt_pq_neuro_ids,
      future_phd   = id %in% future_ids,
      # Cog psych (edu or PQ) is now Tier B — tagged for audit.
      cogpsych_only = (edu_cogpsych | pq_cogpsych) &
        !edu_neuro & !selfid_neuro,
      # Counted ONLY because the NeuroTree title bridge reached a neuro
      # dissertation the direct matcher missed. Tagged so the recall gain from
      # the bridge is measurable rather than implicit.
      nt_only = nt_pq_neuro & !edu_neuro & !pq_neuro,
      tier = dplyr::case_when(
        future_phd                                  ~ "C_excluded",
        edu_neuro | pq_neuro | nt_pq_neuro          ~ "A_include",
        selfid_neuro | exp_neuro |
          edu_feeder | edu_cogpsych | nt_affil       ~ "B_adjudicate",
        TRUE                                        ~ "C_excluded"
      ),
      include = tier == "A_include",
      reason = dplyr::case_when(
        future_phd             ~ "future_phd_year",
        edu_neuro              ~ "education_neuro",
        pq_neuro               ~ "proquest_neuro",
        nt_pq_neuro            ~ "neurotree_bridged_proquest_neuro",
        selfid_neuro           ~ "selfid_neuro",
        edu_cogpsych           ~ "education_cogpsych",
        nt_affil               ~ "neurotree_affiliation_only",
        tier == "B_adjudicate" ~ "pending_proquest_or_review",
        TRUE                   ~ "no_neuro_signal"
      )
    )
}

inclusion_summary <- function(inclusion) {
  inclusion |>
    dplyr::count(tier, include, reason, sort = TRUE) |>
    dplyr::mutate(pct = n / sum(n) * 100)
}

# ---- main-store seam ----
#
# The tiered inclusion is computed in the ProQuest store but defines the core
# pipeline's analytic sample. The seam is a single CSV (mirrors the Glassdoor
# enrichment seam): the ProQuest store writes it, _targets.R consumes it via a
# format="file" target, so a re-adjudication rebuilds the main store's sample
# automatically.

INCLUSION_TABLE_PATH <- "output/tables/inclusion_table.csv"

# Best-matched dissertation per person, carrying the ProQuest docview URL and
# subject terms alongside the LinkedIn profile URL so a reviewer can open both
# sides of a match from the inclusion table alone. Mirrors pq_backfill_years'
# selection rule (closest match wins, ties -> earliest year) but keeps pq_url/
# subject_all instead of collapsing to just the year.
pq_best_match_detail <- function(pq_matched) {
  pq_matched |>
    dplyr::filter(match_type != "unmatched", !is.na(matched_id),
                  !is.na(degree_year)) |>
    dplyr::group_by(matched_id) |>
    dplyr::slice_min(name_dist + 0.001 * degree_year, n = 1, with_ties = FALSE) |>
    dplyr::ungroup() |>
    dplyr::transmute(id = matched_id, pq_url, pq_subject_terms = subject_all)
}

# pq_phd_year rides along on the seam: the degree_year of each person's
# best-matched dissertation (pq_backfill_years, R/03_features.R), so the main
# store can backfill phd_year for people LinkedIn can't date without touching
# the ProQuest store. linkedin_url/pq_url/pq_subject_terms ride along too, so
# each row can be opened on both sides without a separate lookup.
#
# The nt_* publication columns come from the authorPub export via R/23_pubs.R.
# `nt_n_pubs` is a THRESHOLDED count (the cut is in `nt_pub_threshold`), never a
# raw candidate-row count, and `nt_citations_sum` is a lower bound — see the
# header of R/23_pubs.R before publishing anything built on them.
NT_SEAM_COLS <- c("nt_pid", "nt_url", "nt_advisor_name", "nt_dissertation_title",
                  "nt_phd_year", "nt_n_pubs", "nt_n_pubs_human_confirmed",
                  "nt_citations_sum", "nt_pub_threshold")

# NIH funding history (R/24_grants.R) rides the same seam, joined nt_pid ->
# person. Read these THREE-STATE:
#
#   nt_grant_known == FALSE  -> no NeuroTree pid, so funding is UNKNOWN. The
#                               counts and flags are NA, not zero.
#   nt_grant_known == TRUE, nt_n_grants == 0
#                            -> we looked this person up in peopleGrant and found
#                               no award clearing NT_GRANT_SCORE_MIN. A real zero,
#                               subject to the name-matcher's recall.
#   nt_grant_known == TRUE, nt_n_grants > 0  -> funded.
#
# Never let a zero and an unknown collapse: only 42.8% of Arm V has an nt_pid at
# all (measured 2026-07-29), so treating NA as 0 would report most of the sample
# as unfunded on no evidence.
NT_GRANT_SEAM_COLS <- c("nt_grant_known", "nt_n_grants", "nt_ever_pi",
                        "nt_first_grant_year", "nt_last_grant_year",
                        "nt_ever_r01", "nt_ever_training_award")

write_inclusion_table <- function(inclusion, people, pq_matched = NULL,
                                  nt_link = NULL, nt_grants = NULL,
                                  path = INCLUSION_TABLE_PATH) {
  out <- inclusion |>
    dplyr::select(id, tier, include, reason) |>
    dplyr::left_join(
      people |> dplyr::select(id, linkedin_url = url) |> dplyr::distinct(),
      by = "id")
  if (!is.null(pq_matched)) {
    out <- out |>
      dplyr::left_join(pq_backfill_years(pq_matched), by = "id") |>
      dplyr::left_join(pq_best_match_detail(pq_matched), by = "id")
  } else {
    out$pq_phd_year <- NA_integer_
    out$pq_url <- NA_character_
    out$pq_subject_terms <- NA_character_
  }
  # NeuroTree payload rides the same seam: advisor lineage, PhD era and the
  # dissertation title reach the main store without it touching either the
  # NeuroTree or ProQuest store.
  if (!is.null(nt_link) && nrow(nt_link) > 0) {
    nt <- dplyr::distinct(nt_link, id, .keep_all = TRUE)
    # A neurotree_link.csv written before the publication targets existed has no
    # nt_n_pubs columns; fill them so the seam's shape never depends on which
    # version of the NeuroTree store last ran.
    for (col in c("nt_n_pubs", "nt_n_pubs_human_confirmed")) {
      if (!col %in% names(nt)) nt[[col]] <- NA_integer_
    }
    for (col in c("nt_citations_sum", "nt_pub_threshold")) {
      if (!col %in% names(nt)) nt[[col]] <- NA_real_
    }
    out <- out |>
      dplyr::left_join(
        nt |>
          dplyr::transmute(id, nt_pid, nt_url, nt_advisor_name,
                           nt_dissertation_title,
                           nt_phd_year = as.integer(nt_year),
                           nt_n_pubs = as.integer(nt_n_pubs),
                           nt_n_pubs_human_confirmed =
                             as.integer(nt_n_pubs_human_confirmed),
                           nt_citations_sum = as.double(nt_citations_sum),
                           nt_pub_threshold = as.double(nt_pub_threshold)),
        by = "id")
  } else {
    out[NT_SEAM_COLS] <- list(NA_character_, NA_character_, NA_character_,
                              NA_character_, NA_integer_, NA_integer_,
                              NA_integer_, NA_real_, NA_real_)
  }
  # NIH funding history. `nt_grant_known` is set from nt_pid, so the zero/unknown
  # distinction above survives the join: a linked person with no matching grant
  # row coalesces to 0/FALSE, an unlinked person stays NA.
  if (!is.null(nt_grants) && nrow(nt_grants) > 0 && "nt_pid" %in% names(out)) {
    out <- out |>
      dplyr::left_join(
        nt_grants |>
          dplyr::distinct(nt_pid, .keep_all = TRUE) |>
          dplyr::select(nt_pid, dplyr::all_of(setdiff(NT_GRANT_SEAM_COLS,
                                                      "nt_grant_known"))),
        by = "nt_pid") |>
      dplyr::mutate(
        nt_grant_known = !is.na(nt_pid),
        dplyr::across(c(nt_n_grants),
                      ~ dplyr::if_else(nt_grant_known,
                                       as.integer(dplyr::coalesce(., 0L)),
                                       NA_integer_)),
        dplyr::across(c(nt_ever_pi, nt_ever_r01, nt_ever_training_award),
                      ~ dplyr::if_else(nt_grant_known,
                                       dplyr::coalesce(., FALSE), NA))) |>
      dplyr::relocate(dplyr::all_of(NT_GRANT_SEAM_COLS), .after = dplyr::last_col())
  } else {
    out[NT_GRANT_SEAM_COLS] <- list(NA, NA_integer_, NA, NA_integer_,
                                    NA_integer_, NA, NA)
  }
  readr::write_csv(out, path)
  path
}

# Guarantee the seam file exists so the core pipeline's format="file" target is
# satisfied before the ProQuest store has ever run. Writes an empty, shaped CSV
# only when absent — never clobbers a real adjudication.
ensure_inclusion_stub <- function(path = INCLUSION_TABLE_PATH) {
  if (!file.exists(path)) {
    readr::write_csv(
      tibble::tibble(id = character(), tier = character(),
                     include = logical(), reason = character(),
                     linkedin_url = character(), pq_phd_year = integer(),
                     pq_url = character(), pq_subject_terms = character(),
                     nt_pid = character(), nt_url = character(),
                     nt_advisor_name = character(),
                     nt_dissertation_title = character(),
                     nt_phd_year = integer(),
                     nt_grant_known = logical(), nt_n_grants = integer(),
                     nt_ever_pi = logical(), nt_first_grant_year = integer(),
                     nt_last_grant_year = integer(), nt_ever_r01 = logical(),
                     nt_ever_training_award = logical(),
                     nt_n_pubs = integer(),
                     nt_n_pubs_human_confirmed = integer(),
                     nt_citations_sum = double(),
                     nt_pub_threshold = double()),
      path)
  }
  path
}

.read_inclusion_table <- function(inclusion_csv) {
  readr::read_csv(inclusion_csv, show_col_types = FALSE,
                  col_types = readr::cols(
                    id = readr::col_character(),
                    tier = readr::col_character(),
                    include = readr::col_logical(),
                    reason = readr::col_character(),
                    linkedin_url = readr::col_character(),
                    pq_phd_year = readr::col_integer(),
                    pq_url = readr::col_character(),
                    pq_subject_terms = readr::col_character(),
                    nt_pid = readr::col_character(),
                    nt_url = readr::col_character(),
                    nt_advisor_name = readr::col_character(),
                    nt_dissertation_title = readr::col_character(),
                    nt_phd_year = readr::col_integer(),
                    nt_grant_known = readr::col_logical(),
                    nt_n_grants = readr::col_integer(),
                    nt_ever_pi = readr::col_logical(),
                    nt_first_grant_year = readr::col_integer(),
                    nt_last_grant_year = readr::col_integer(),
                    nt_ever_r01 = readr::col_logical(),
                    nt_ever_training_award = readr::col_logical(),
                    nt_n_pubs = readr::col_integer(),
                    nt_n_pubs_human_confirmed = readr::col_integer(),
                    nt_citations_sum = readr::col_double(),
                    nt_pub_threshold = readr::col_double()))
}

# Main-store PhD-year policy, applied to the analytic sample.
#
# Three steps, in order:
#   1. ProQuest degree_year OVERRIDES the LinkedIn-derived year wherever it
#      exists. It is the gold standard: an awarded-degree record beats a
#      heuristic over LinkedIn free text. This is a change from the old
#      behaviour, which only used ProQuest to fill gaps.
#   2. NeuroTree's grad-row year fills whatever ProQuest still can't date —
#      gap-filling only, since it is the weaker signal.
#   3. Cap at PHD_YEAR_MAX, dropping in-progress degrees.
#
# Renamed from backfill_phd_year_from_seam(): it no longer only backfills.
# No-op on steps 1-2 while the seam is the stub; step 3 still applies.
apply_phd_year_policy <- function(features, inclusion_csv) {
  inc <- .read_inclusion_table(inclusion_csv)
  pq_year <- inc |>
    dplyr::filter(!is.na(pq_phd_year)) |>
    dplyr::select(id, pq_phd_year)
  if (nrow(pq_year) > 0) {
    features <- .apply_phd_year_backfill(features, pq_year,
                                         "proquest_degree_year",
                                         override = TRUE)
  }
  if ("nt_phd_year" %in% names(inc)) {
    nt_year <- inc |>
      dplyr::filter(!is.na(nt_phd_year)) |>
      dplyr::transmute(id, pq_phd_year = nt_phd_year)
    if (nrow(nt_year) > 0) {
      features <- .apply_phd_year_backfill(features, nt_year,
                                           "neurotree_grad_year")
    }
  }
  drop_incomplete_phd_years(features)
}

# Analytic sample for the core pipeline: Tier-A included people from the
# inclusion table, restricted to ids present in the current people pull (a
# stale table can't inject ghosts). Falls back to the legacy education-only
# funnel while the seam file is still the empty stub.
analytic_ids_from_inclusion <- function(inclusion_csv, filter_funnel, people) {
  inc <- .read_inclusion_table(inclusion_csv)
  ids <- intersect(inc$id[inc$include], unique(people$id))
  if (length(ids) > 0) return(ids)
  message("inclusion_table.csv empty — falling back to the education-only funnel sample.")
  filter_funnel |> dplyr::filter(stage == "final") |> dplyr::pull(id)
}

# ARM V — the sample every figure and table is built on (METHODS.md,
# "Analytic sample").
#
# Arm V is Tier A AND matched to a ProQuest dissertation (revised 2026-07-29).
# That is the whole rule. The earlier definition also
# required `pq_neuro` and `phd_is_us`; those conditions are gone, not merely
# unenforced, and this function is the definition of record.
#
# `pq_url` is non-NA on exactly the people pq_best_match_detail() found a
# dissertation for, so the existing seam already carries everything this needs —
# no pq_neuro / phd_is_us column has to be added to write_inclusion_table().
#
# The revision grew the sample by about 5%: people who reached Tier A via
# education while their matched dissertation is not neuro-classed, plus people
# whose phd_is_us is not TRUE — mostly institutions ROR could not place, since
# phd_is_us is three-state (TRUE / FALSE / NA), not genuinely foreign PhDs.
#
# Consequence worth knowing when publishing: Arm V no longer carries an explicit
# US condition, so it is US-dominated by ProQuest's corpus coverage (non-US
# match rate 0.4%) rather than by construction. Do not call it a US sample.
#
# `phd_year` coverage is UNAFFECTED and stays 100% by construction: pq_url is
# non-NA only where pq_best_match_detail() found a dissertation with a non-NA
# degree_year, so every Arm-V member still triggers the backfill. That remains a
# definitional artifact, not a data-quality signal — never compare Arm V to a
# non-matched group on phd_year completeness.
#
# Deliberately NO stub fallback, unlike analytic_ids_from_inclusion() above. An
# empty seam has to be loud here: quietly substituting the education-only funnel
# would publish figures built on ~94k collected people while every label on them
# says Arm V. Green-offline is the right default for a QC artifact and the wrong
# one for the headline sample.
armv_ids_from_inclusion <- function(inclusion_csv, people) {
  inc <- .read_inclusion_table(inclusion_csv)
  ids <- inc |>
    dplyr::filter(.data$tier == "A_include", !is.na(.data$pq_url)) |>
    dplyr::pull("id") |>
    intersect(unique(people$id))
  if (length(ids) == 0) {
    stop("Arm V is empty — output/tables/inclusion_table.csv is the stub, or ",
         "carries no pq_url. Run the ProQuest store before the core store.",
         call. = FALSE)
  }
  message(sprintf("Arm V analytic sample: %d people", length(ids)))
  ids
}

# Tier-B people awaiting ProQuest corroboration or a human pass — not counted
# until adjudicated. Joins the LinkedIn name + profile URL so each can be opened.
write_inclusion_review <- function(inclusion, people, path) {
  inclusion |>
    dplyr::filter(tier == "B_adjudicate") |>
    dplyr::left_join(
      people |> dplyr::select(id, name, url) |> dplyr::distinct(),
      by = "id") |>
    dplyr::relocate(name, url, .after = id) |>
    readr::write_csv(path)
  path
}
