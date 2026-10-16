# R/22_rankings.R — neuroscience PhD program rankings attached to the person-level
# table as a PhD-institution prestige covariate.
#
# TWO SOURCES, BOTH GLOBAL RANK ONLY. Everything else each
# file carries (research.com's national_rank / scholars / sum_publications /
# sum_d_index / profile_url) is deliberately dropped.
#
#   researchcom_world_rank_2026  — research.com "Best Neuroscience Universities in
#     the World", 2026 (5th ed). 799 schools, 51 countries, rank 1-799. Genuinely
#     global, and the source that carries the analysis: it reaches 89.2% of Arm V.
#   usnews_global_rank_2020 / usnews_global_score_2020 — US News. 106 schools.
#     The `rank` column is a GLOBAL rank (1-397) and the file holds only the US
#     subset of it, which is why the ranks are sparse. Score is 0-100, monotone
#     with rank. Reaches 74.7% of Arm V but adds only ~1pp beyond research.com;
#     90 of its 106 schools are already in research.com.
#
# ABSENCE IS NOT A LOW RANK. A school with no row here may be unranked OR may be
# ranked under a spelling the matcher could not reach. Both sources therefore
# ship a `*_ranked` flag, and the score/rank stays NA rather than being imputed.
# US News absence is additionally uninformative for non-US institutions, which
# the file excludes by construction.
#
# MATCHING — two tiers, no fuzzy distance anywhere.
#   1. exact `inst_norm` (norm_institution(), the same key the ProQuest matcher
#      and the ROR crosswalk block on).
#   2. UNIQUE bidirectional token-subset. Ranking files spell out campus and
#      system suffixes that LinkedIn omits ("Purdue University West Lafayette"
#      vs "Purdue University"; "University at Buffalo, State University of New
#      York" vs "University at Buffalo") and occasionally the reverse
#      ("University of Missouri" vs "University of Missouri-Columbia"). A match
#      requires one token set to contain the other AND exactly one candidate to
#      qualify. Measured 2026-07-29: ror_core_sorted() does NOT solve this --
#      the suffix tokens survive the generic-vocabulary strip -- which is why
#      this rule exists rather than reusing the ROR core ladder.
#
# The uniqueness requirement is what makes tier 2 safe. A bare "University of
# California" is a token subset of every UC campus, so it qualifies against
# several rows and is dropped rather than being handed an arbitrary one.
#
# COLLISIONS WITHIN A RANKING FILE are resolved before matching, because
# norm_institution() is lossy in ways that matter here: Boston University (42)
# and Boston College (392) both reduce to `boston` at norm AND at core level.
# A true duplicate of one school (research.com lists Würzburg twice, once with
# the umlaut and once without) keeps its better rank; genuinely different schools
# sharing a key are dropped to NA. Assigning one school's rank to another is the
# one failure this must not commit. Everything dropped lands in the review CSV.

USNEWS_RANKINGS_PATH      <- "data/usnews_neuro_rankings_2020.csv"
RESEARCHCOM_RANKINGS_PATH <- "data/research_com_neuroscience_university_rankings_2026.csv"
RANKINGS_REVIEW_PATH      <- "output/tables/rankings_match_review.csv"

# Institutions norm_institution() cannot separate, because it strips the very
# org-type word that distinguishes them. `boston` is the measured case: Boston
# University (research.com 42) and Boston College (392) both reduce to it, so
# the collision gate dropped BOTH, costing 156 Arm-V people a rank they should
# have had. Matched on the RAW string before normalization, and applied to both
# sides of the join, so the two schools land on distinct keys.
#
# Deliberately NOT a change to norm_institution(): that function is the blocking
# key for the ProQuest matcher and the ROR crosswalk, and moving it would
# re-measure linkage results this has nothing to do with. Scoped to rankings.
# Longest/most-specific pattern first — "boston university school of medicine"
# must not fall through to a bare-"boston" rule.
INST_DISAMBIG <- c(
  "\\bboston college\\b"    = "boston_college",
  "\\bboston university\\b" = "boston_university"
)

# norm_institution(), with the disambiguation overrides applied.
.inst_key <- function(x) {
  k <- norm_institution(x)
  lo <- tolower(dplyr::coalesce(as.character(x), ""))
  for (i in seq_along(INST_DISAMBIG)) {
    hit <- stringr::str_detect(lo, names(INST_DISAMBIG)[i])
    k[hit] <- unname(INST_DISAMBIG[i])
  }
  k
}

.rank_tokset <- function(x) {
  lapply(strsplit(dplyr::coalesce(x, ""), " +"), function(t) sort(unique(t[nzchar(t)])))
}

# One row per inst_norm. Same-institution duplicates collapse to the better
# (lower) rank; different institutions sharing a key are dropped and reported.
.resolve_rank_collisions <- function(tbl, source_label) {
  tbl <- tbl |> dplyr::filter(!is.na(inst_norm), nzchar(inst_norm))
  dropped <- tibble::tibble(source = character(), inst_norm = character(),
                            university = character(), rank = numeric(),
                            reason = character())

  grouped <- tbl |> dplyr::group_by(inst_norm)
  keep <- grouped |>
    dplyr::filter(dplyr::n() == 1) |>
    dplyr::ungroup()

  multi <- grouped |> dplyr::filter(dplyr::n() > 1) |> dplyr::ungroup()
  if (nrow(multi)) {
    for (k in unique(multi$inst_norm)) {
      g <- multi[multi$inst_norm == k, ]
      # Same school spelled two ways -> identical normalized university string.
      same <- length(unique(.norm_uni(g$university))) == 1
      if (same) {
        keep <- dplyr::bind_rows(keep, g[which.min(g$rank), ])
        dropped <- dplyr::bind_rows(dropped, tibble::tibble(
          source = source_label, inst_norm = k, university = g$university,
          rank = g$rank, reason = "duplicate_same_school_kept_best_rank"))
      } else {
        dropped <- dplyr::bind_rows(dropped, tibble::tibble(
          source = source_label, inst_norm = k, university = g$university,
          rank = g$rank, reason = "key_collision_distinct_schools_dropped"))
      }
    }
  }
  list(tbl = keep, dropped = dropped)
}

# Loose comparison used only to decide "is this the same school twice".
.norm_uni <- function(x) {
  x |> tolower() |>
    stringi::stri_trans_general("Latin-ASCII") |>
    stringr::str_replace_all("[^a-z0-9]+", " ") |>
    stringr::str_squish()
}

read_usnews_rankings <- function(path = USNEWS_RANKINGS_PATH) {
  readr::read_csv(path, show_col_types = FALSE, progress = FALSE) |>
    dplyr::transmute(
      university,
      rank  = as.numeric(rank),
      score = as.numeric(score),
      inst_norm = .inst_key(university))
}

read_researchcom_rankings <- function(path = RESEARCHCOM_RANKINGS_PATH) {
  readr::read_csv(path, show_col_types = FALSE, progress = FALSE) |>
    dplyr::transmute(
      university,
      rank    = as.numeric(world_rank),
      country = country,
      inst_norm = .inst_key(university))
}

# Tier-2 residual match. Returns a lookup keyed on the QUERY inst_norm.
.token_subset_match <- function(query_keys, rank_tbl) {
  if (!length(query_keys)) {
    return(tibble::tibble(inst_norm = character(), matched_inst_norm = character()))
  }
  q_tok <- .rank_tokset(query_keys)
  r_tok <- .rank_tokset(rank_tbl$inst_norm)

  out <- vapply(seq_along(query_keys), function(i) {
    qt <- q_tok[[i]]
    if (!length(qt)) return(NA_character_)
    ok <- vapply(r_tok, function(rt) {
      length(rt) > 0 && (all(qt %in% rt) || all(rt %in% qt))
    }, logical(1))
    if (sum(ok) == 1L) rank_tbl$inst_norm[which(ok)] else NA_character_
  }, character(1))

  tibble::tibble(inst_norm = query_keys, matched_inst_norm = out) |>
    dplyr::filter(!is.na(matched_inst_norm))
}

# Resolve one ranking source against a vector of institution keys.
# Returns inst_norm -> (rank, [score], match_tier).
.match_one_source <- function(inst_keys, rank_tbl, source_label, has_score = FALSE) {
  res <- .resolve_rank_collisions(rank_tbl, source_label)
  rt  <- res$tbl

  keys <- unique(inst_keys[!is.na(inst_keys) & nzchar(inst_keys)])
  exact <- tibble::tibble(inst_norm = keys[keys %in% rt$inst_norm]) |>
    dplyr::mutate(matched_inst_norm = inst_norm, match_tier = "exact")

  residual <- setdiff(keys, exact$inst_norm)
  subset_m <- .token_subset_match(residual, rt) |>
    dplyr::mutate(match_tier = "token_subset")

  lut <- dplyr::bind_rows(exact, subset_m) |>
    dplyr::left_join(
      rt |> dplyr::select(matched_inst_norm = inst_norm, university,
                          rank, dplyr::any_of("score")),
      by = "matched_inst_norm")

  list(lut = lut, dropped = res$dropped,
       unmatched = setdiff(keys, lut$inst_norm))
}

# Attach both sources to the person-level feature table.
attach_rankings <- function(features,
                            usnews_path      = USNEWS_RANKINGS_PATH,
                            researchcom_path = RESEARCHCOM_RANKINGS_PATH,
                            review_path      = RANKINGS_REVIEW_PATH) {
  feat <- features |>
    dplyr::mutate(.inst_norm = .inst_key(phd_institution))

  un <- .match_one_source(feat$.inst_norm, read_usnews_rankings(usnews_path),
                          "usnews_2020", has_score = TRUE)
  rc <- .match_one_source(feat$.inst_norm, read_researchcom_rankings(researchcom_path),
                          "researchcom_2026")

  out <- feat |>
    dplyr::left_join(
      un$lut |> dplyr::transmute(.inst_norm = inst_norm,
                                 usnews_global_rank_2020  = rank,
                                 usnews_global_score_2020 = score,
                                 usnews_match_tier        = match_tier),
      by = ".inst_norm") |>
    dplyr::left_join(
      rc$lut |> dplyr::transmute(.inst_norm = inst_norm,
                                 researchcom_world_rank_2026 = rank,
                                 researchcom_match_tier      = match_tier),
      by = ".inst_norm") |>
    dplyr::mutate(
      usnews_ranked      = !is.na(usnews_global_rank_2020),
      researchcom_ranked = !is.na(researchcom_world_rank_2026)) |>
    dplyr::select(-.inst_norm)

  write_rankings_review(feat, un, rc, review_path)
  out
}

# Audit artifact: every collision decision, and every Arm-V institution neither
# source could reach, worst-first by how many people it costs.
write_rankings_review <- function(feat, un, rc, path) {
  people_per_inst <- feat |>
    dplyr::filter(!is.na(.inst_norm), nzchar(.inst_norm)) |>
    dplyr::count(.inst_norm, phd_institution, name = "n_people")

  unmatched <- people_per_inst |>
    dplyr::mutate(
      in_usnews      = !.inst_norm %in% un$lut$inst_norm,
      in_researchcom = !.inst_norm %in% rc$lut$inst_norm) |>
    dplyr::filter(in_usnews & in_researchcom) |>
    dplyr::transmute(source = "both", inst_norm = .inst_norm,
                     university = phd_institution, rank = NA_real_,
                     n_people, reason = "no_match_in_either_source") |>
    dplyr::arrange(dplyr::desc(n_people))

  dplyr::bind_rows(un$dropped, rc$dropped, unmatched) |>
    readr::write_csv(path)
  path
}
