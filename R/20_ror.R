# R/20_ror.R — ROR institution crosswalk, a SUPPLEMENT to norm_institution
#
# Context (measured 2026-07-27: ROR is NOT a drop-in replacement for
# `norm_institution`): a first ROR build resolved raw
# institution strings to ROR ids and blocked on the id. As an exact-variant
# matcher it LOST 7.0% of the education rows `norm_institution` already reaches
# (Cambridge, Penn State, Northwestern, Columbia, Purdue, UCLA, Harvard Medical
# School...) while adding 0.1%. Two things were wrong with that shape and both
# are fixed here:
#
#   1. It resolved the RAW string. ROR has no "Harvard Medical School" org at
#      all, and no "Purdue University" display name — but `norm_institution`
#      already folds those into "harvard" / "purdue" via its sub-unit strip.
#      This crosswalk is therefore keyed on **inst_norm**, not the raw string:
#      resolve every raw spelling, then let the whole inst_norm group inherit
#      the winning id. norm_institution's hand-curated work is kept and ROR is
#      layered on top of it.
#   2. Key collisions (6,211 of them) were DROPPED, which is what actually lost
#      Purdue and Northwestern — both sides of the join had the same string and
#      neither resolved. For a BLOCKING key an arbitrary-but-deterministic
#      choice among collisions is fine, because both sides go through this same
#      table and land on the same id. Collisions are now resolved, never dropped.
#
# The resulting block key is `coalesce(ror_id, inst_norm)` (`ror_block_key()`),
# and it is **lossless by construction**: two strings that share an inst_norm
# share a crosswalk group, hence share a block key, so anything that blocks
# today still blocks. ROR can only ADD — by giving two DIFFERENT inst_norm
# groups the same id ("UMass Boston" / "University of Massachusetts Boston").
#
# That "can only add" guarantee has one price: two unrelated groups landing on
# one id merge into a single block. Every such merge is written to
# `output/tables/ror_crosswalk_merge_review.csv`, worst-first, and gated by
# `.ror_merge_ok()` below.
#
# Source data: data/fetch_ror.R (Zenodo, CC0). The crosswalk itself is built by
# `build_ror_crosswalk.R` at the repo root and consumed by the pipeline as a
# static file — never a per-run parse of the 285 MB dump.
#
# COUNTRY (added 2026-07-28). The analysis focuses on people whose PhD
# institution is in the US, WITHOUT discarding the
# non-US people already collected. ROR is the only country source in the
# pipeline — LinkedIn education rows carry no country, and ProQuest's country
# parentheticals are stripped by `.clean_pq_institution()` before we ever see
# them — so this crosswalk now also emits `country`.
#
# It LABELS, it does not filter. Nothing is dropped anywhere in this file.
#
# Resolution stays country-BLIND on purpose. Preferring a US org when a key is
# ambiguous would manufacture the very US-ness the label is meant to measure;
# instead the country is read off whatever org the country-blind ladder picked,
# and genuinely ambiguous groups are flagged `country_conflict` for audit rather
# than guessed at. See `.ror_group_country()`.

ROR_DIR                <- "data/ror/extracted"
ROR_CROSSWALK_PATH      <- "output/tables/ror_institution_crosswalk.csv"
ROR_MERGE_REVIEW_PATH   <- "output/tables/ror_crosswalk_merge_review.csv"
ROR_COUNTRY_REVIEW_PATH <- "output/tables/ror_country_conflict_review.csv"

# Jaro-Winkler ceiling for the fuzzy layer. Tighter than the name matcher's
# NAME_MAX (0.12): an institution is a blocking key, so a false accept pools two
# schools' candidate sets, whereas a false reject only costs the ROR supplement.
ROR_FUZZY_MAX <- 0.10

# Fuzzy is not applied below this many characters of core key. Short cores are
# acronyms and initialisms ("ucla", "mit", "ucl", "nyu"), where one edit is a
# different institution and Jaro-Winkler is worthless. Six keeps distinctive
# one-word schools ("purdue", "rutgers") in and every acronym in the corpus out.
ROR_FUZZY_MIN_CHARS <- 6

# Share of an inst_norm group's ROR-resolved weight that the plurality country
# must hold for the group to carry a country label at all. Below this the group
# is genuinely split and is reported as unknown. See `.ror_group_country()` for
# why an "any disagreement is a conflict" rule does not work.
ROR_COUNTRY_MIN_SHARE <- 0.80

# Country codes that count as "US" for the analytic sample. Puerto Rico is a US
# territory and its universities are US institutions for this analysis (added
# 2026-07-28); UPR Rio Piedras, UPR Medical Sciences,
# Ponce Health Sciences and Carlos Albizu are all in the ProQuest corpus. The
# other inhabited territories (GU, VI, AS, MP) are the same legal category — add
# them here if they ever appear in the data.
ROR_US_COUNTRIES <- c("US", "PR")

# Resolution tiers trusted to assign a COUNTRY. Blocking can afford a loose
# guess (a wrong block key mostly just fails to match); a country label defines
# the analytic sample, so it gets the stricter bar. Measured 2026-07-28: every
# observed country misassignment came from the fuzzy tier — "Weill Medical
# College of Cornell University" -> Weill Cornell Qatar (QA, 1,351 rows),
# "United States International University" -> USIU-Africa (KE),
# "Teachers College, Columbia University" -> an Indian org, "Medical College of
# Georgia" -> a Spanish one. All four are fuzzy; none is fixable by hierarchy,
# because the offending orgs have no ROR parent. Exact and core only.
ROR_COUNTRY_TIERS <- c("exact", "core")

# Generic organizational vocabulary, dropped to form the "core" key. Extends
# INST_STOPWORDS (R/07_proquest.R) with the multilingual equivalents ROR carries
# in its `label` translations, plus the pure connectives ("de", "di", "von").
# Deliberately EXCLUDES anything that can distinguish one school from another:
# "state", "polytechnic", "technology", "medical", "national" are kept, and so
# are articles that belong to place names ("Los" Angeles, "Las" Vegas) and bare
# single letters (Texas "A" and "M").
ROR_GENERIC_TOKENS <- unique(c(
  INST_STOPWORDS,
  "universitet", "universitetet", "universiteit", "uniwersytet", "univerzita",
  "univerza", "yliopisto", "universitas", "universitatea", "universiteti",
  "ecole", "escuela", "escola", "hochschule", "fachhochschule", "facultad",
  "faculdade", "faculte", "fakultat", "colegio", "istituto", "universidad",
  "universidade", "universite", "universitaet",
  "de", "del", "der", "des", "di", "du", "van", "von"
))

# A comma inside a ROR name column is ambiguous: it separates multiple values
# ("Purdue University Calumet, Purdue University North Central") but also occurs
# INSIDE a single name ("University of California, Los Angeles" — which is the
# exact string ProQuest writes, so splitting it would be a real loss). Split only
# when EVERY comma-piece looks like an institution in its own right.
ROR_ORG_WORD_RE <- paste0(
  "(?i)\\b(universit|college|institut|school|academ|centre|center|hospital",
  "|laborator|faculty|facult|escuela|escola|hochschule|polytechnic|seminary",
  "|conservator|akadem|universidad|universidade|ecole)"
)

# ---- normalization -----------------------------------------------------------

# Light normalization, deliberately NOT norm_institution: this is the key ROR
# variants are indexed under, and folding it through norm_institution's aliases
# would just re-test the thing ROR is meant to supplement.
ror_key <- function(x) {
  x <- stringr::str_to_lower(dplyr::coalesce(x, ""))
  x <- stringi::stri_trans_general(x, "Latin-ASCII")
  x <- stringr::str_replace_all(x, "\\(.*?\\)", " ")
  x <- stringr::str_replace_all(x, "&", " and ")
  x <- stringr::str_replace_all(x, "[[:punct:]]", " ")
  x <- stringr::str_squish(x)
  stringr::str_replace(x, "^the ", "")
}

# Order-preserving core: the generic organizational vocabulary removed. Keeps
# word order so the fuzzy layer's prefix block and Jaro-Winkler's prefix bonus
# both stay meaningful ("penn state university" -> "penn state").
ror_core <- function(key) {
  purrr::map_chr(stringr::str_split(dplyr::coalesce(key, ""), " "), function(t) {
    t <- t[t != "" & !(t %in% ROR_GENERIC_TOKENS)]
    paste(t, collapse = " ")
  })
}

# Order-independent core, for the second exact layer. This is what lets
# "Penn State University" reach ROR's "Penn State" alias without any fuzz.
ror_core_sorted <- function(key) {
  purrr::map_chr(stringr::str_split(dplyr::coalesce(key, ""), " "), function(t) {
    t <- t[t != "" & !(t %in% ROR_GENERIC_TOKENS)]
    paste(sort(unique(t)), collapse = " ")
  })
}

# ---- ROR dump -> name variants ----------------------------------------------

ror_csv_path <- function(dir = ROR_DIR) {
  hits <- fs::dir_ls(dir, glob = "*ror-data.csv", fail = FALSE)
  if (length(hits) == 0) {
    stop("No ROR CSV under ", dir, " — run: Rscript data/fetch_ror.R", call. = FALSE)
  }
  as.character(sort(hits, decreasing = TRUE)[[1]])
}

read_ror_orgs <- function(path = ror_csv_path()) {
  readr::read_csv(path, show_col_types = FALSE,
                  col_types = readr::cols(.default = readr::col_character())) |>
    dplyr::filter(status == "active")
}

# "en: Foo; fr: Bar" -> c("Foo", "Bar"); then the conditional comma split.
.ror_split_values <- function(cell, split_commas = FALSE) {
  parts <- stringr::str_split(dplyr::coalesce(cell, ""), ";\\s*")[[1]]
  parts <- stringr::str_squish(stringr::str_replace(parts, "^[a-z_]{2,14}:\\s*", ""))
  parts <- parts[parts != ""]
  if (length(parts) == 0) return(character(0))
  out <- purrr::map(parts, function(p) {
    if (!stringr::str_detect(p, ",")) return(p)
    pieces <- stringr::str_squish(stringr::str_split(p, ",\\s*")[[1]])
    pieces <- pieces[pieces != ""]
    # Acronyms never contain a comma, so their commas are always separators.
    # For names, split only when every piece is independently institution-like.
    if (split_commas || all(stringr::str_detect(pieces, ROR_ORG_WORD_RE))) pieces else p
  })
  unique(unlist(out))
}

# Long table of every name a ROR org goes by, with the precedence used to settle
# key collisions. Lower `pref` wins: a display name beats a translated label,
# which beats an alias, which beats an acronym.
ROR_NAME_COLS <- c(
  ror_display = "names.types.ror_display",
  label       = "names.types.label",
  alias       = "names.types.alias",
  acronym     = "names.types.acronym"
)
ROR_NAME_PREF <- c(ror_display = 1L, label = 2L, alias = 3L, acronym = 4L)

build_ror_variants <- function(orgs) {
  types <- stringr::str_split(dplyr::coalesce(orgs$types, ""), ";\\s*")
  meta <- tibble::tibble(
    ror_id = orgs$id,
    is_edu = purrr::map_lgl(types, ~ "education" %in% .x),
    # Prominence proxy for collision tiebreaks: ROR carries no popularity
    # measure, so use the curation footprint — a funder role, external
    # identifiers, a Wikipedia page. The US Northwestern outranks the Philippine
    # one on all three. No country is read (see the header note).
    is_funder = purrr::map_lgl(types, ~ "funder" %in% .x),
    # First location's ISO-2 code. Multi-campus orgs list several; the first is
    # the headquarters, which is the one that answers "where is this degree
    # from". Never used to CHOOSE an org — only read off the one chosen.
    country = stringr::str_squish(stringr::str_split_i(
      dplyr::coalesce(orgs$locations.geonames_details.country_code, ""), ";", 1)),
    n_ext = stringr::str_count(dplyr::coalesce(orgs$external_ids.type.grid.all, ""), ";") +
      stringr::str_count(dplyr::coalesce(orgs$external_ids.type.isni.all, ""), ";") +
      as.integer(!is.na(orgs$external_ids.type.wikidata.preferred)) +
      as.integer(!is.na(orgs$external_ids.type.fundref.preferred)),
    has_wiki = !is.na(orgs$links.type.wikipedia) & orgs$links.type.wikipedia != ""
  )

  purrr::imap_dfr(ROR_NAME_COLS, function(col, kind) {
    vals <- purrr::map(orgs[[col]], .ror_split_values,
                       split_commas = identical(kind, "acronym"))
    tibble::tibble(
      ror_id  = rep(orgs$id, lengths(vals)),
      variant = unlist(vals),
      kind    = kind
    )
  }) |>
    dplyr::mutate(
      pref = unname(ROR_NAME_PREF[kind]),
      key  = ror_key(variant)
    ) |>
    dplyr::filter(key != "") |>
    dplyr::left_join(meta, by = "ror_id") |>
    dplyr::mutate(
      core        = ror_core(key),
      core_sorted = ror_core_sorted(key)
    ) |>
    dplyr::distinct(ror_id, key, kind, .keep_all = TRUE)
}

.ror_collapse <- function(variants, key_col) {
  variants |>
    dplyr::rename(.k = dplyr::all_of(key_col)) |>
    dplyr::filter(.k != "") |>
    dplyr::group_by(.k) |>
    dplyr::mutate(n_orgs = dplyr::n_distinct(ror_id)) |>
    .ror_pick_grouped() |>
    dplyr::ungroup() |>
    dplyr::select(key = .k, ror_id, kind, n_orgs)
}

# Deterministic winner among the orgs sharing one key. Precedence, in order:
# name kind (display > label > alias > acronym), education type, funder role,
# external-identifier count, a Wikipedia page, then the ROR id itself so the
# result never depends on row order. Collisions are RESOLVED, not dropped —
# dropping them is what lost Purdue and Northwestern in the first build.
.ror_pick_grouped <- function(g) {
  g |>
    dplyr::arrange(pref, dplyr::desc(is_edu), dplyr::desc(is_funder),
                   dplyr::desc(n_ext), dplyr::desc(has_wiki), ror_id,
                   .by_group = TRUE) |>
    dplyr::slice(1)
}

# ROR ships hierarchy in `relationships` as "child: <ids>; parent: <ids>;
# related: <ids>". Only `parent:` is read: one edge per (child, parent).
build_ror_parents <- function(orgs) {
  par <- stringr::str_extract(dplyr::coalesce(orgs$relationships, ""),
                              "parent:[^;]*")
  ids <- stringr::str_extract_all(dplyr::coalesce(par, ""),
                                  "https://ror\\.org/[0-9a-z]+")
  tibble::tibble(ror_id = rep(orgs$id, lengths(ids)),
                 parent_id = unlist(ids)) |>
    dplyr::distinct()
}

# Walk an org up to its topmost ancestor. Used ONLY for the country label — a
# branch campus should answer with its parent's country, and the parent of a
# campus is the institution an unqualified name most likely means. Deliberately
# NOT applied to the block key: folding "UC Berkeley" up to a UC system org
# would pool every campus into one block, which is exactly the SUNY failure the
# merge gate exists to prevent.
#
# Bounded depth also makes a cyclic or self-referential edge harmless.
ror_root_id <- function(ids, parents, max_depth = 5L) {
  if (is.null(parents) || nrow(parents) == 0) return(ids)
  up <- parents |> dplyr::slice(1, .by = ror_id)
  m <- stats::setNames(up$parent_id, up$ror_id)
  out <- ids
  for (i in seq_len(max_depth)) {
    nxt <- unname(m[out])
    if (all(is.na(nxt))) break
    out <- dplyr::coalesce(nxt, out)
  }
  out
}

# Three lookup tables consulted in order by `resolve_ror_strings()`, plus the
# ror_id -> country map the US label is read from and the parent edges.
build_ror_lookups <- function(variants, orgs = NULL) {
  list(
    parents = if (is.null(orgs)) NULL else build_ror_parents(orgs),
    country = variants |>
      dplyr::distinct(ror_id, country) |>
      dplyr::filter(country != "", !is.na(country)),
    exact = .ror_collapse(variants, "key"),
    core  = .ror_collapse(variants, "core_sorted"),
    # The fuzzy layer scores against ordered cores, so it needs the full
    # variant list rather than a collapsed key -> id table.
    fuzzy = variants |>
      dplyr::filter(core != "", nchar(core) >= ROR_FUZZY_MIN_CHARS) |>
      dplyr::mutate(blk = substr(core, 1, 4)) |>
      dplyr::group_by(core) |>
      .ror_pick_grouped() |>
      dplyr::ungroup() |>
      dplyr::select(core, blk, ror_id, kind)
  )
}

# ---- resolution ladder -------------------------------------------------------

# Fuzzy layer. Blocked on the first four characters of the ordered core key —
# the same prefix Jaro-Winkler rewards, and cheap because the generic
# vocabulary ("university", "institute") has already been stripped out of the
# core, so no block degenerates into "everything".
#
# Two gates on top of the distance, both in the spirit of the ProQuest matcher's
# asymmetric name gate (R/07_proquest.R): nothing substantive may disagree, and
# a short core is never fuzzed at all.
.ror_fuzzy_resolve <- function(cores, fuzzy_lut) {
  empty <- tibble::tibble(core = character(), ror_id = character(),
                          fuzzy_dist = double(), ror_variant_core = character())
  if (length(cores) == 0) return(empty)
  q <- unique(cores[!is.na(cores) & cores != "" &
                      nchar(cores) >= ROR_FUZZY_MIN_CHARS])
  if (length(q) == 0) return(empty)

  # Index the candidate pool by block ONCE, as integer row positions. Splitting
  # the data frame itself costs minutes at this size; splitting seq_len does not.
  # Candidate token sets are likewise pre-split, since the token gate below would
  # otherwise re-split the same strings on every query.
  pool_idx  <- split(seq_len(nrow(fuzzy_lut)), fuzzy_lut$blk)
  cand_core <- fuzzy_lut$core
  cand_id   <- fuzzy_lut$ror_id
  cand_toks <- stringr::str_split(cand_core, " ")

  q_blk  <- substr(q, 1, 4)
  q_toks <- stringr::str_split(q, " ")

  hit_core <- character(0); hit_id <- character(0)
  hit_d <- double(0); hit_var <- character(0)

  for (i in seq_along(q)) {
    idx <- pool_idx[[q_blk[i]]]
    if (is.null(idx)) next
    d <- stringdist::stringdist(q[i], cand_core[idx], method = "jw", p = 0.1)
    near <- idx[d <= ROR_FUZZY_MAX]
    if (length(near) == 0) next
    dn <- d[d <= ROR_FUZZY_MAX]
    ok <- .ror_tokens_compatible(q_toks[[i]], cand_toks[near])
    if (!any(ok)) next
    near <- near[ok]; dn <- dn[ok]
    best <- near[order(dn, cand_id[near])][1]
    hit_core <- c(hit_core, q[i])
    hit_id   <- c(hit_id, cand_id[best])
    hit_d    <- c(hit_d, min(dn))
    hit_var  <- c(hit_var, cand_core[best])
  }

  tibble::tibble(core = hit_core, ror_id = hit_id, fuzzy_dist = hit_d,
                 ror_variant_core = hit_var)
}

# Token gate: every token the two cores do NOT share must be explainable — it is
# either a near-spelling of a token on the other side ("penn"/"pennsylvania"), or
# the two token sets nest ("purdue" inside "purdue system"). What this rejects is
# the dangerous case: both sides carrying a DIFFERENT substantive qualifier, i.e.
# "illinois urbana champaign" against "illinois chicago". Distance alone does not
# catch that — the shared "illinois" prefix carries it under the threshold.
#
# Takes pre-split token vectors, not strings: it runs once per candidate per
# query and re-splitting there dominated the whole build.
.ror_tokens_compatible <- function(a, cand_toks) {
  vapply(cand_toks, function(b) {
    extra_a <- setdiff(a, b)
    if (length(extra_a) == 0) return(TRUE)          # nesting
    extra_b <- setdiff(b, a)
    if (length(extra_b) == 0) return(TRUE)          # nesting
    all(vapply(extra_a, function(t) {
      any(stringdist::stringdist(t, extra_b, method = "jw", p = 0.1) <= 0.20)
    }, logical(1)))
  }, logical(1))
}

# Raw institution string -> ROR id, via exact key, then order-independent core,
# then blocked fuzzy. `ror_tier` records which layer fired, and is used both for
# the merge gate and for the audit table.
resolve_ror_strings <- function(strings, lookups) {
  s <- tibble::tibble(institution = unique(stats::na.omit(strings))) |>
    dplyr::mutate(key = ror_key(institution),
                  core_sorted = ror_core_sorted(key),
                  core = ror_core(key))

  s <- s |>
    dplyr::left_join(lookups$exact |> dplyr::select(key, .id_exact = ror_id),
                     by = "key") |>
    dplyr::left_join(lookups$core |> dplyr::select(key, .id_core = ror_id),
                     by = c("core_sorted" = "key"))

  todo <- s$core[is.na(s$.id_exact) & is.na(s$.id_core)]
  message(sprintf("  exact %s / core %s / fuzzy candidates %s of %s strings",
                  format(sum(!is.na(s$.id_exact)), big.mark = ","),
                  format(sum(is.na(s$.id_exact) & !is.na(s$.id_core)), big.mark = ","),
                  format(length(unique(todo)), big.mark = ","),
                  format(nrow(s), big.mark = ",")))
  fz <- .ror_fuzzy_resolve(todo, lookups$fuzzy)

  s |>
    dplyr::left_join(fz |> dplyr::select(core, .id_fuzzy = ror_id, fuzzy_dist,
                                         ror_variant_core),
                     by = "core") |>
    dplyr::mutate(
      ror_id = dplyr::coalesce(.id_exact, .id_core, .id_fuzzy),
      ror_tier = dplyr::case_when(
        !is.na(.id_exact) ~ "exact",
        !is.na(.id_core)  ~ "core",
        !is.na(.id_fuzzy) ~ "fuzzy",
        TRUE              ~ NA_character_),
      fuzzy_dist = dplyr::if_else(ror_tier == "fuzzy", fuzzy_dist, NA_real_),
      ror_variant_core = dplyr::if_else(ror_tier == "fuzzy", ror_variant_core,
                                        NA_character_)
    ) |>
    dplyr::select(institution, key, core, ror_id, ror_tier, fuzzy_dist,
                  ror_variant_core)
}

# ---- inst_norm crosswalk -----------------------------------------------------

# THE artifact. `inst_strings` is (institution, n) pooled across BOTH sides of
# the join — the LinkedIn education rows and the ProQuest school names — so a
# given inst_norm resolves identically wherever it appears. Resolution happens
# on the raw spelling (it carries the most signal), then the whole inst_norm
# group inherits the id its heaviest resolved spelling voted for.
#
# The group vote is what makes the block key lossless: strings sharing an
# inst_norm can never disagree about their block key, because they are handed
# one answer for the group.
build_ror_crosswalk <- function(inst_strings, lookups, resolved = NULL) {
  stopifnot(all(c("institution", "n") %in% names(inst_strings)))

  if (is.null(resolved)) {
    resolved <- resolve_ror_strings(inst_strings$institution, lookups)
  }

  scored <- inst_strings |>
    dplyr::mutate(inst_norm = norm_institution(institution)) |>
    dplyr::filter(inst_norm != "", !is.na(inst_norm)) |>
    dplyr::left_join(resolved, by = "institution")

  # One vote per (group, candidate id), weighted by rows and broken by tier.
  votes <- scored |>
    dplyr::filter(!is.na(ror_id)) |>
    dplyr::mutate(tier_rank = match(ror_tier, c("exact", "core", "fuzzy"))) |>
    dplyr::summarise(votes = sum(n), tier_rank = min(tier_rank),
                     .by = c(inst_norm, ror_id))

  winners <- votes |>
    dplyr::arrange(inst_norm, dplyr::desc(votes), tier_rank, ror_id) |>
    dplyr::slice(1, .by = inst_norm) |>
    dplyr::mutate(ror_tier = c("exact", "core", "fuzzy")[tier_rank])

  contested <- votes |>
    dplyr::summarise(n_ror_candidates = dplyr::n(), .by = inst_norm)

  group_n <- scored |>
    dplyr::summarise(n = sum(n), n_strings = dplyr::n(), .by = inst_norm)

  cw <- group_n |>
    dplyr::left_join(winners |> dplyr::select(inst_norm, ror_id, ror_tier),
                     by = "inst_norm") |>
    dplyr::left_join(contested, by = "inst_norm") |>
    dplyr::mutate(n_ror_candidates = dplyr::coalesce(n_ror_candidates, 0L)) |>
    .ror_group_country(votes, lookups$country, lookups$parents)

  .ror_apply_merge_gate(cw)
}

# Country label for an inst_norm group, decided by the SAME weighted vote that
# picks the group's ror_id — not by the winning org alone, and emphatically not
# by "do any two candidate orgs disagree".
#
# The existential rule was tried first (2026-07-28) and is wrong: it flagged 160
# groups covering 49,059 education rows, including `columbia` (CA/US), `yale`
# (SG/US), `florida` (CN/US), `rutgers` (DE/US) and `michigan state` (RU/US).
# In every one of those the dominant answer is the obvious US university and the
# dissent is a single rare string resolving to a tiny foreign namesake. Treating
# them as unknown would have deleted the core of the US sample to avoid a
# handful of misassigned rows.
#
# So: aggregate the group's resolved weight BY COUNTRY, take the plurality, and
# flag a conflict only when the winner fails to clear ROR_COUNTRY_MIN_SHARE.
# `cambridge` lands on GB (717 rows Cambridge UK vs ~19 Cambridge College US) —
# the 19 are mislabelled, which is the residual cost and is audited, not hidden.
# Genuinely split groups like `york` (CA/GB/US) still flag.
.ror_group_country <- function(cw, votes, country_lut, parents = NULL) {
  by_country <- votes |>
    # Only exact/core resolutions may name a country (ROR_COUNTRY_TIERS).
    dplyr::filter(tier_rank <= length(ROR_COUNTRY_TIERS)) |>
    # Fold each org up to its topmost ancestor, so a branch campus answers with
    # the parent institution's country.
    dplyr::mutate(root_id = ror_root_id(ror_id, parents)) |>
    dplyr::left_join(country_lut |> dplyr::rename(root_id = ror_id),
                     by = "root_id") |>
    dplyr::filter(!is.na(country), country != "") |>
    dplyr::summarise(cvotes = sum(votes), .by = c(inst_norm, country))

  totals <- by_country |>
    dplyr::summarise(total = sum(cvotes),
                     countries = paste(sort(unique(country)), collapse = "/"),
                     .by = inst_norm)

  winner <- by_country |>
    dplyr::arrange(inst_norm, dplyr::desc(cvotes), country) |>
    dplyr::slice(1, .by = inst_norm) |>
    dplyr::left_join(totals, by = "inst_norm") |>
    dplyr::transmute(inst_norm, country, country_share = cvotes / total,
                     countries)

  cw |>
    dplyr::left_join(winner, by = "inst_norm") |>
    dplyr::mutate(
      country_conflict = !is.na(country) & country_share < ROR_COUNTRY_MIN_SHARE,
      country = dplyr::if_else(country_conflict, NA_character_, country),
      country_candidates = dplyr::if_else(country_conflict, countries,
                                          NA_character_)) |>
    dplyr::select(-countries)
}

# A ROR id claimed by two or more inst_norm groups MERGES them into one block.
# That is where the supplement earns its keep ("boston umass" + "boston
# massachusetts") and also the only way it can do harm — a lone group on an id
# is a no-op, since renaming a blocking key changes nothing.
#
# Gate: the fuzzy layer may not be the thing that fuses two groups together. It
# is the loosest layer and merges are the highest-consequence outcome, so on a
# contested id the FUZZY-tier groups are demoted and the exact/core ones keep
# it. That preserves genuine exact+exact merges (which a blanket "any fuzzy on
# this id disqualifies it" rule would throw away) while never letting a fuzzy
# resolution pull an unrelated school into someone else's block.
#
# MEASURED 2026-07-27 (ablation, strict vs no gate at all):
#   strict (shipped)   coverage 31.2%   net new +0.38%   1,454 merges
#   permissive         coverage 32.1%   net new +1.28%   3,179 merges
# The extra 0.90% is not worth taking. Permissive pools "brook stony", "albany"
# and "binghamton" onto ROR 01y64my43 — which is *University at Buffalo*, whose
# ROR display name is "University at Buffalo, State University of New York".
# Three distinct SUNY campuses land in Buffalo's block, and a namesake at Albany
# could then match a Buffalo dissertation. The gate blocks exactly that, and it
# is what the ablation was run to find out. It also declines real gains — the
# biggest is "maryland park" (U. Maryland College Park, 2,917 rows) not merging
# with "maryland" — so this is a deliberate precision-over-recall trade, the
# same one the surname-exact rule makes in the name matcher.
#
# Demotion means `ror_id = NA`, i.e. back to plain inst_norm — always safe,
# because it restores exactly today's behaviour for that group.
# The gate demotes the BLOCK KEY only. `ror_id` / `ror_tier` / `country` record
# what the resolution ladder concluded and are left intact; `block_id` is the
# gated subset the matcher is allowed to key on. Keeping them separate matters
# now that the crosswalk also supplies the US label: declining to MERGE two
# blocks says nothing about which country the school is in, and a demoted group
# should not silently lose its country too.
.ror_apply_merge_gate <- function(cw) {
  contested <- cw |>
    dplyr::filter(!is.na(ror_id)) |>
    dplyr::summarise(n_on_id = dplyr::n(), .by = ror_id) |>
    dplyr::filter(n_on_id > 1)

  cw <- cw |>
    dplyr::mutate(
      ror_blocked_reason = dplyr::if_else(
        !is.na(ror_id) & ror_tier == "fuzzy" & ror_id %in% contested$ror_id,
        "fuzzy_merge_declined", NA_character_),
      block_id = dplyr::if_else(is.na(ror_blocked_reason), ror_id, NA_character_))

  # Recount after demotion: an id whose only partners were fuzzy is no longer a
  # merge at all.
  surviving <- cw |>
    dplyr::filter(!is.na(block_id)) |>
    dplyr::summarise(n_groups = dplyr::n(), .by = block_id)

  cw |>
    dplyr::left_join(surviving, by = "block_id") |>
    dplyr::select(inst_norm, block_id, ror_id, ror_tier, country,
                  country_conflict, country_candidates, n, n_strings,
                  n_ror_candidates, n_groups, ror_blocked_reason) |>
    dplyr::arrange(dplyr::desc(n), inst_norm)
}

# ---- the block key ----------------------------------------------------------

# coalesce(block_id, inst_norm). Lossless: same inst_norm => same crosswalk row
# => same key, so every pair blocking today still blocks. Unknown or unresolved
# inst_norm falls straight through to today's behaviour.
ror_block_key <- function(inst_norm, crosswalk) {
  if (is.null(crosswalk) || nrow(crosswalk) == 0) return(dplyr::coalesce(inst_norm, ""))
  hit <- crosswalk$block_id[match(inst_norm, crosswalk$inst_norm)]
  dplyr::coalesce(hit, inst_norm, "")
}

# ---- the US label -----------------------------------------------------------

# Country of the PhD institution, or NA when ROR could not resolve the group.
# A `country_conflict` group (candidate orgs in different countries — the
# Cambridge GB / Cambridge College US case) returns NA rather than a coin flip:
# for a sample definition, "unknown" is honest and "probably" is not.
ror_country <- function(inst_norm, crosswalk) {
  if (is.null(crosswalk) || nrow(crosswalk) == 0) return(rep(NA_character_, length(inst_norm)))
  i <- match(inst_norm, crosswalk$inst_norm)
  dplyr::if_else(dplyr::coalesce(crosswalk$country_conflict[i], FALSE),
                 NA_character_, crosswalk$country[i])
}

# Three-state on purpose: TRUE / FALSE / NA. NA means ROR could not place the
# institution, which is NOT the same as "not US" — 10.8% of education rows
# resolve to no ROR org at all. Scoping the analysis with `is_us %in% TRUE`
# silently drops the unknowns; scoping with `!(is_us %in% FALSE)` keeps them.
# Which is right is an analysis decision, so this function refuses to make it.
ror_is_us <- function(inst_norm, crosswalk) {
  cc <- ror_country(inst_norm, crosswalk)
  dplyr::if_else(is.na(cc), NA, cc %in% ROR_US_COUNTRIES)
}

# ---- pipeline seam ----------------------------------------------------------

ROR_CROSSWALK_SCHEMA <- tibble::tibble(
  inst_norm = character(), block_id = character(), ror_id = character(),
  ror_tier = character(), country = character(), country_conflict = logical(),
  country_candidates = character(), n = integer(), n_strings = integer(),
  n_ror_candidates = integer(), n_groups = integer(),
  ror_blocked_reason = character()
)

# Same stub contract as ensure_neurotree_link_stub(): a header-only file keeps
# the graph green before build_ror_crosswalk.R has ever run, and an empty
# crosswalk makes ror_block_key() a no-op that returns inst_norm unchanged.
ensure_ror_crosswalk_stub <- function(path = ROR_CROSSWALK_PATH) {
  if (!file.exists(path)) readr::write_csv(ROR_CROSSWALK_SCHEMA, path)
  path
}

read_ror_crosswalk <- function(path) {
  readr::read_csv(path, show_col_types = FALSE,
                  col_types = readr::cols(
                    inst_norm = readr::col_character(),
                    block_id = readr::col_character(),
                    ror_id = readr::col_character(),
                    ror_tier = readr::col_character(),
                    country = readr::col_character(),
                    country_conflict = readr::col_logical(),
                    country_candidates = readr::col_character(),
                    n = readr::col_integer(),
                    n_strings = readr::col_integer(),
                    n_ror_candidates = readr::col_integer(),
                    n_groups = readr::col_integer(),
                    ror_blocked_reason = readr::col_character()))
}

# ---- audit ------------------------------------------------------------------

# Every ROR id that pools two or more inst_norm groups, worst-first by the rows
# at stake. This is the file to read when a ProQuest match looks wrong: a bad
# merge here widens a candidate pool with a school that does not belong in it.
build_ror_merge_review <- function(crosswalk) {
  crosswalk |>
    dplyr::filter(!is.na(block_id), n_groups > 1) |>
    dplyr::arrange(dplyr::desc(n_groups), block_id, dplyr::desc(n)) |>
    dplyr::summarise(
      n_groups = dplyr::first(n_groups),
      rows = sum(n),
      tiers = paste(sort(unique(ror_tier)), collapse = "+"),
      country = dplyr::first(country),
      merged_inst_norm = paste(inst_norm, collapse = " | "),
      .by = block_id) |>
    dplyr::arrange(dplyr::desc(rows), block_id)
}

# Every inst_norm group whose candidate orgs straddle a border. These are the
# groups a US filter would mis-scope, so they get their own audit file rather
# than a column nobody reads.
build_ror_country_review <- function(crosswalk) {
  crosswalk |>
    dplyr::filter(country_conflict) |>
    dplyr::select(inst_norm, country_candidates, ror_id, ror_tier, n, n_strings) |>
    dplyr::arrange(dplyr::desc(n), inst_norm)
}

write_ror_merge_review <- function(review, path = ROR_MERGE_REVIEW_PATH) {
  readr::write_csv(review, path)
  path
}

write_ror_country_review <- function(review, path = ROR_COUNTRY_REVIEW_PATH) {
  readr::write_csv(review, path)
  path
}
