# R/18_neurotree.R — NeuroTree / AcademicTree linkage
#
# Two independent links, both computed from data already on disk (no network,
# no billing):
#
#   1. LinkedIn <-> NeuroTree, on name + institution (+ year), reusing the exact
#      key machinery and precision gates the ProQuest matcher earned in
#      R/07_proquest.R. NeuroTree ships firstname/middlename/lastname as separate
#      columns, so the key build is simpler than the ProQuest "Last, First Middle"
#      parse — everything else (surname exact, institution as a hard blocking key,
#      middle-initial disconfirmation, education-year window) is shared.
#
#   2. NeuroTree <-> ProQuest, on normalized dissertation title. `connect.pqid` is
#      a 7-digit UMI/ProQuest PUBLICATION number and does NOT join to the TDM
#      export's 10-digit GOID (the export carries no publication-number column) —
#      the title is the working key instead.
#
# Discipline, same as ProQuest: NeuroTree is a positive signal and a Tier-B
# adjudicator, NEVER a gate. A NeuroTree non-match must never drop anyone.
#
# Source: Zenodo record 19739198 (DOI 10.5281/zenodo.4247418), CC-BY 3.0.
# Attribution to academictree.org is required on any published finding; also cite
# 10.1038/s41467-018-07034-y. Staged by data/fetch_neurotree.R.

NT_DIR <- "data/neurotree"

# connect.relation codes. 0-4 are documented on export.php; 5/6/7 appear in the
# export (3,497 / 487 / 86 rows) and are UNDOCUMENTED — they are deliberately not
# given a meaning here and never feed an inclusion decision.
NT_RELATION_GRAD    <- "1"
NT_RELATION_POSTDOC <- "2"

# Placeholder strings the MySQL export uses for a null.
.nt_na <- function(x) {
  x <- dplyr::na_if(dplyr::coalesce(x, ""), "NULL")
  dplyr::na_if(x, "")
}

nt_profile_url <- function(pid) {
  dplyr::if_else(is.na(pid), NA_character_,
                 paste0("https://neurotree.org/neurotree/peopleinfo.php?pid=", pid))
}

# `majorarea` is a comma-separated list of the trees a person belongs to; "neuro"
# marks Neurotree membership. The bulk export covers ALL AcademicTree fields, so
# this is the filter that isolates the neuro population.
nt_is_neuro <- function(majorarea) {
  stringr::str_detect(stringr::str_to_lower(dplyr::coalesce(majorarea, "")),
                      "\\bneuro\\b")
}

# Read the three person/affiliation tables THIS file uses. Both of the big
# auxiliary exports are also used, but neither is read here — both need a pid filter applied during the
# read rather than a whole-table readr load:
#   * `authorPub*` (7 shards x ~18M rows) — R/23_pubs.R::read_nt_author_pubs(),
#     streamed shard-by-shard via read_csv_chunked().
#   * `peopleGrant` — R/24_grants.R, via data.table::fread.
# Materialising either alongside this list would OOM the machine.
# Everything comes back character; callers coerce what they need.
fetch_neurotree <- function(dir = NT_DIR) {
  rd <- function(f) {
    readr::read_csv(fs::path(dir, f), col_types = readr::cols(.default = "c"),
                    progress = FALSE)
  }
  people <- rd("people.csv.gz") |>
    dplyr::distinct(pid, .keep_all = TRUE) |>
    dplyr::mutate(dplyr::across(c(firstname, middlename, lastname, degrees,
                                  location, area, majorarea, homepage,
                                  orcid_id, s2id), .nt_na))
  connect <- rd("connect.csv.gz") |>
    dplyr::mutate(dplyr::across(c(location, dissertation_title, pqid, stopdate),
                                .nt_na))
  locations <- rd("locations.csv.gz") |>
    dplyr::distinct(locid, .keep_all = TRUE) |>
    dplyr::mutate(dplyr::across(c(location, shortname, country), .nt_na))
  list(people = people, connect = connect, locations = locations)
}

# connect.stopdate is "YYYY-MM-DD" with zero-filled month/day ("2005-00-00"), and
# "0000-00-00" for unknown. Take the year, bounded to a plausible range.
.nt_year <- function(stopdate) {
  y <- suppressWarnings(as.integer(stringr::str_sub(dplyr::coalesce(stopdate, ""), 1, 4)))
  dplyr::if_else(!is.na(y) & y >= 1900 & y <= as.integer(format(Sys.Date(), "%Y")),
                 y, NA_integer_)
}

# ---- person keys ----
#
# One row per (person, affiliation). A person's PhD institution can appear on a
# non-grad row (faculty appointment, postdoc), so ALL relation types are kept as
# candidate blocking keys — widening from grad-only lifted exact matches ~9% in
# probing. `relation` rides along so the inclusion rule can distinguish an
# advisor-attested grad-student row from a postdoc affiliation.
#
# Reuses R/07_proquest.R: .norm_tokstr, .drop_credentials, .first_tok, .last_tok,
# .mid_inits (via middle_key) and norm_institution.
prepare_nt_persons <- function(people, connect, locations, neuro_only = TRUE) {
  ppl <- people
  if (neuro_only) ppl <- dplyr::filter(ppl, nt_is_neuro(majorarea))

  ppl <- ppl |>
    dplyr::transmute(
      nt_pid = pid,
      nt_name = stringr::str_squish(paste(dplyr::coalesce(firstname, ""),
                                          dplyr::coalesce(middlename, ""),
                                          dplyr::coalesce(lastname, ""))),
      nt_area = stringr::str_squish(paste(dplyr::coalesce(area, ""),
                                          dplyr::coalesce(majorarea, ""))),
      nt_homepage = homepage,
      name_norm  = norm_name(nt_name),
      first_key  = .first_tok(.drop_credentials(.norm_tokstr(firstname))),
      last_key   = .last_tok(.drop_credentials(.norm_tokstr(lastname))),
      middle_key = .drop_credentials(.norm_tokstr(middlename))
    ) |>
    dplyr::mutate(name_key = stringr::str_squish(paste(first_key, last_key))) |>
    dplyr::filter(first_key != "", last_key != "")

  loc <- locations |> dplyr::select(locid, loc_name = location)

  connect |>
    dplyr::semi_join(ppl, by = c("pid1" = "nt_pid")) |>
    dplyr::left_join(loc, by = "locid") |>
    dplyr::transmute(
      nt_pid = pid1,
      nt_advisor_pid = dplyr::na_if(pid2, "0"),
      nt_relation = relation,
      nt_year = .nt_year(stopdate),
      nt_institution = dplyr::coalesce(loc_name, location),
      nt_dissertation_title = dissertation_title,
      nt_pqid = pqid
    ) |>
    dplyr::left_join(ppl, by = "nt_pid") |>
    dplyr::mutate(inst_norm = norm_institution(nt_institution))
}

# ---- matching: exact (hash join) then fuzzy on the residual ----
#
# Mirrors pq_match_exact / pq_match_fuzzy (R/07_proquest.R:687, :731). The grain
# differs: both sides are PEOPLE here, so candidates are deduped to a mutual best
# (one NeuroTree person per LinkedIn person and vice versa) by .nt_mutual_best.

.nt_empty_match <- function() {
  tibble::tibble(
    nt_pid = character(), matched_id = character(), match_type = character(),
    name_dist = double(), year_dist = integer(), tie = double()
  )
}

# One-to-one reduction: best NeuroTree person per LinkedIn id, then best LinkedIn
# id per NeuroTree person. Two people sharing a name+school can't both claim the
# same profile.
.nt_mutual_best <- function(cand) {
  if (nrow(cand) == 0) return(.nt_empty_match())
  score <- function(d) {
    d$name_dist + 0.05 * dplyr::coalesce(d$year_dist, 0L) +
      1e-3 * dplyr::coalesce(d$tie, 0)
  }
  cand |>
    dplyr::mutate(.score = score(cand)) |>
    dplyr::group_by(matched_id) |>
    dplyr::slice_min(.score, n = 1, with_ties = FALSE) |>
    dplyr::ungroup() |>
    dplyr::group_by(nt_pid) |>
    dplyr::slice_min(.score, n = 1, with_ties = FALSE) |>
    dplyr::ungroup() |>
    dplyr::select(-.score)
}

# Stage 1: exact name_key + inst_norm. Year people require |nt_year - phd_year|
# <= 1; people with no derivable phd_year fall back to the education-year window
# (.within_edu_window), same as the ProQuest institution-only path.
nt_match_exact <- function(persons, nt_persons) {
  p <- persons   |> dplyr::filter(name_key != "", inst_norm != "", !is.na(inst_norm))
  n <- nt_persons |> dplyr::filter(name_key != "", inst_norm != "", !is.na(inst_norm))
  if (nrow(p) == 0 || nrow(n) == 0) return(.nt_empty_match())

  n_keyed <- n |>
    dplyr::select(nt_pid, name_key, inst_norm, nt_year, n_name_norm = name_norm)

  yr <- p |>
    dplyr::filter(!is.na(phd_year)) |>
    dplyr::select(id, phd_year, name_key, inst_norm, p_name_norm = name_norm) |>
    dplyr::inner_join(n_keyed, by = c("name_key", "inst_norm"),
                      relationship = "many-to-many") |>
    dplyr::filter(!is.na(nt_year), abs(nt_year - phd_year) <= 1) |>
    dplyr::transmute(nt_pid, matched_id = id, match_type = "exact_year",
                     name_dist = 0, year_dist = abs(nt_year - phd_year),
                     tie = stringdist::stringdist(p_name_norm, n_name_norm,
                                                  method = "jw"))

  # Institution-only fallback, for the two cases the year branch can't serve:
  # the person has no derivable phd_year, OR the NeuroTree row carries no year
  # (~31% of affiliation rows have "0000-00-00" as stopdate). Both reduce to
  # name+institution exact with the education-year window as the only temporal
  # gate — .within_edu_window passes on NA, which is the existing ProQuest
  # behaviour for an undatable row.
  noyr <- p |>
    dplyr::select(id, phd_year, name_key, inst_norm, p_name_norm = name_norm,
                  edu_min_year, edu_max_year) |>
    dplyr::inner_join(n_keyed, by = c("name_key", "inst_norm"),
                      relationship = "many-to-many") |>
    dplyr::filter(is.na(phd_year) | is.na(nt_year)) |>
    dplyr::filter(.within_edu_window(nt_year, edu_min_year, edu_max_year)) |>
    dplyr::transmute(nt_pid, matched_id = id, match_type = "exact_inst",
                     name_dist = 0, year_dist = NA_integer_,
                     tie = stringdist::stringdist(p_name_norm, n_name_norm,
                                                  method = "jw"))

  .nt_mutual_best(dplyr::bind_rows(yr, noyr))
}

# Shared component gate for both fuzzy paths: first token FUZZY (JW <= NAME_MAX),
# surname EXACT (dl == 0), first initials agree. Identical to the gate in
# .match_by_year / .match_by_institution — surnames are never fuzzy-matched.
.nt_block_hits <- function(p, n, name_max) {
  df <- stringdist::stringdistmatrix(p$first_key, n$first_key, method = "jw")
  dl <- stringdist::stringdistmatrix(p$last_key,  n$last_key,  method = "jw")
  init_ok <- outer(substr(p$first_key, 1, 1), substr(n$first_key, 1, 1), "==")
  hits <- which(df <= name_max & dl == 0 & init_ok, arr.ind = TRUE)
  list(hits = hits, df = df, dl = dl)
}

# Path A: person has a phd_year -> block on (year +-1, institution). The year
# corroborates, so no field gate is applied (mirrors .match_by_year).
.nt_match_by_year <- function(persons, nt_persons, name_max = NAME_MAX,
                              return_all = FALSE) {
  persons <- dplyr::filter(persons, !is.na(phd_year), inst_norm != "",
                           !is.na(inst_norm), first_key != "", last_key != "")
  nt_persons <- dplyr::filter(nt_persons, !is.na(nt_year), inst_norm != "",
                              !is.na(inst_norm), first_key != "", last_key != "")
  if (nrow(persons) == 0 || nrow(nt_persons) == 0) return(.nt_empty_match())

  persons_exp <- persons |>
    tidyr::crossing(year_offset = c(-1L, 0L, 1L)) |>
    dplyr::mutate(join_year = phd_year + year_offset)

  blocks <- dplyr::inner_join(
    persons_exp |> dplyr::distinct(join_year, inst_norm),
    nt_persons |> dplyr::distinct(join_year = nt_year, inst_norm),
    by = c("join_year", "inst_norm"), relationship = "many-to-many")
  if (nrow(blocks) == 0) return(.nt_empty_match())

  res <- purrr::map2_dfr(blocks$join_year, blocks$inst_norm, function(yr, inst) {
    p <- persons_exp |> dplyr::filter(join_year == yr, inst_norm == inst)
    n <- nt_persons  |> dplyr::filter(nt_year == yr, inst_norm == inst)
    if (nrow(p) == 0 || nrow(n) == 0) return(tibble::tibble())
    bh <- .nt_block_hits(p, n, name_max)
    hits <- bh$hits
    if (nrow(hits) == 0) return(tibble::tibble())
    pil <- .mid_inits(p$middle_key); nil <- .mid_inits(n$middle_key)
    tibble::tibble(
      nt_pid     = n$nt_pid[hits[, 2]],
      matched_id = p$id[hits[, 1]],
      match_type = "fuzzy_year",
      name_dist  = pmax(bh$df[hits], bh$dl[hits]),
      year_dist  = abs(p$year_offset[hits[, 1]]),
      tie        = stringdist::stringdist(p$name_norm[hits[, 1]],
                                          n$name_norm[hits[, 2]], method = "jw"),
      field_ok   = TRUE,
      middle_ok  = !mapply(.middle_conflict_scalar, pil[hits[, 1]], nil[hits[, 2]])
    )
  })
  .nt_finish_fuzzy(res, return_all)
}

# Path B: no usable year on one side or the other -> block on institution only,
# with BOTH extra gates the no-year ProQuest path uses: education-year window and
# field/subject corroboration (LinkedIn PhD field text vs the NeuroTree research
# area + tree membership). No year to lean on here, so both are required.
# The CALLER decides which persons land here (see nt_match_fuzzy) — this function
# does not filter on phd_year itself, because a person who HAS a year still needs
# this path when the NeuroTree row has none.
.nt_match_by_institution <- function(persons, nt_persons, name_max = NAME_MAX,
                                     return_all = FALSE) {
  persons <- dplyr::filter(persons, inst_norm != "",
                           !is.na(inst_norm), first_key != "", last_key != "")
  nt_persons <- dplyr::filter(nt_persons, inst_norm != "", !is.na(inst_norm),
                              first_key != "", last_key != "")
  if (nrow(persons) == 0 || nrow(nt_persons) == 0) return(.nt_empty_match())

  shared <- intersect(unique(persons$inst_norm), unique(nt_persons$inst_norm))
  if (length(shared) == 0) return(.nt_empty_match())

  res <- purrr::map_dfr(shared, function(inst) {
    p <- persons    |> dplyr::filter(inst_norm == inst)
    n <- nt_persons |> dplyr::filter(inst_norm == inst)
    if (nrow(p) == 0 || nrow(n) == 0) return(tibble::tibble())
    bh <- .nt_block_hits(p, n, name_max)
    hits <- bh$hits
    if (nrow(hits) == 0) return(tibble::tibble())

    cand <- tibble::tibble(
      .pi = hits[, 1], .ni = hits[, 2],
      nt_pid     = n$nt_pid[hits[, 2]],
      matched_id = p$id[hits[, 1]],
      match_type = "fuzzy_inst",
      name_dist  = pmax(bh$df[hits], bh$dl[hits]),
      year_dist  = NA_integer_,
      tie        = stringdist::stringdist(p$name_norm[hits[, 1]],
                                          n$name_norm[hits[, 2]], method = "jw"),
      .nty = n$nt_year[hits[, 2]],
      .emn = p$edu_min_year[hits[, 1]],
      .emx = p$edu_max_year[hits[, 1]]
    ) |>
      dplyr::filter(.within_edu_window(.nty, .emn, .emx))
    if (nrow(cand) == 0) return(tibble::tibble())

    pil <- .mid_inits(p$middle_key); nil <- .mid_inits(n$middle_key)
    cand |>
      dplyr::mutate(
        middle_ok = !mapply(.middle_conflict_scalar, pil[.pi], nil[.ni]),
        field_ok  = fields_consistent(p$phd_field_text[.pi], n$nt_area[.ni])) |>
      dplyr::select(-.pi, -.ni, -.nty, -.emn, -.emx)
  })
  .nt_finish_fuzzy(res, return_all)
}

.nt_finish_fuzzy <- function(res, return_all) {
  if (nrow(res) == 0) return(.nt_empty_match())
  res <- res |>
    dplyr::mutate(reject_reason = dplyr::case_when(
      !middle_ok ~ "middle_name_conflict",
      !field_ok  ~ "field_not_corroborated",
      TRUE       ~ NA_character_))
  if (return_all) return(res)
  res |>
    dplyr::filter(is.na(reject_reason)) |>
    dplyr::select(nt_pid, matched_id, match_type, name_dist, year_dist, tie)
}

nt_match_fuzzy <- function(persons, nt_persons, exact) {
  residual_p <- dplyr::filter(persons, !(id %in% exact$matched_id))
  residual_n <- dplyr::filter(nt_persons, !(nt_pid %in% exact$nt_pid))

  # Path A gets the year-corroborated pairs. Path B gets everything a year can't
  # corroborate: people with no phd_year (against all NeuroTree rows) and people
  # who DO have one but whose NeuroTree rows are undated.
  a <- .nt_match_by_year(residual_p, residual_n)
  b_noyear_person <- .nt_match_by_institution(
    dplyr::filter(residual_p, is.na(phd_year)), residual_n)
  b_undated_nt <- .nt_match_by_institution(
    dplyr::filter(residual_p, !is.na(phd_year)),
    dplyr::filter(residual_n, is.na(nt_year)))

  .nt_mutual_best(dplyr::bind_rows(a, b_noyear_person, b_undated_nt))
}

match_neurotree <- function(persons, nt_persons) {
  exact <- nt_match_exact(persons, nt_persons)
  fuzzy <- nt_match_fuzzy(persons, nt_persons, exact)
  .nt_mutual_best(dplyr::bind_rows(exact, fuzzy)) |>
    dplyr::select(-tie)
}

# ---- NeuroTree <-> ProQuest, on dissertation title ----

# Normalize a dissertation title to a join key: strip the <sub>/<i> markup the
# ProQuest export carries, fold everything non-alphanumeric to a single space.
nt_norm_title <- function(x) {
  s <- stringr::str_to_lower(dplyr::coalesce(x, ""))
  s <- stringr::str_replace_all(s, "<[^>]+>", " ")
  s <- stringi::stri_trans_general(s, "Latin-ASCII")
  s <- stringr::str_replace_all(s, "[^a-z0-9]+", " ")
  stringr::str_squish(s)
}

# Titles shorter than this are too generic to key on ("introduction", "abstract").
NT_TITLE_MIN_CHARS <- 15L

# Exact normalized-title join. `pqid` (7-digit UMI publication number) does NOT
# join to GOID (10-digit) and the TDM export has no publication-number column, so
# the title is the key. Titles are near-unique but not guaranteed, so a match is
# kept only when school OR year also agrees; the rest go to review.
nt_pq_title_bridge <- function(connect, dissertations) {
  nt_side <- connect |>
    dplyr::filter(relation == NT_RELATION_GRAD) |>
    dplyr::transmute(
      nt_pid = pid1,
      nt_pqid = .nt_na(pqid),
      nt_institution = .nt_na(location),
      nt_year = .nt_year(stopdate),
      nt_dissertation_title = .nt_na(dissertation_title),
      tkey = nt_norm_title(nt_dissertation_title)) |>
    dplyr::filter(nchar(tkey) >= NT_TITLE_MIN_CHARS)

  pq_side <- dissertations |>
    dplyr::transmute(pq_id, pq_title = title, pq_institution = institution,
                     pq_degree_year = degree_year,
                     pq_subject_all = subject_all,
                     tkey = nt_norm_title(title)) |>
    dplyr::filter(nchar(tkey) >= NT_TITLE_MIN_CHARS) |>
    dplyr::distinct(tkey, .keep_all = TRUE)

  nt_side |>
    dplyr::inner_join(pq_side, by = "tkey", relationship = "many-to-many") |>
    dplyr::mutate(
      inst_agree = norm_institution(nt_institution) == norm_institution(pq_institution) &
        norm_institution(nt_institution) != "",
      year_agree = !is.na(nt_year) & !is.na(pq_degree_year) &
        abs(nt_year - pq_degree_year) <= 1,
      corroborated = inst_agree | year_agree) |>
    dplyr::group_by(nt_pid) |>
    dplyr::slice_max(corroborated + inst_agree + year_agree, n = 1,
                     with_ties = FALSE) |>
    dplyr::ungroup() |>
    dplyr::select(nt_pid, pq_id, nt_pqid, nt_dissertation_title, pq_title,
                  nt_institution, pq_institution, nt_year, pq_degree_year,
                  pq_subject_all, inst_agree, year_agree, corroborated)
}

# ---- seam to the ProQuest store (which owns the inclusion rule) ----

NEUROTREE_LINK_PATH <- "output/tables/neurotree_link.csv"

NT_LINK_SCHEMA <- tibble::tibble(
  id = character(), nt_pid = character(), match_type = character(),
  name_dist = double(), year_dist = integer(),
  nt_name = character(), nt_url = character(), linkedin_url = character(),
  nt_relation = character(), nt_neuro_grad = logical(),
  nt_neuro_postdoc = logical(), nt_pq_neuro = logical(), nt_year = integer(),
  nt_institution = character(), nt_advisor_pid = character(),
  nt_advisor_name = character(), nt_dissertation_title = character(),
  nt_goid = character(), nt_pq_subject_terms = character(),
  nt_neuro_basis = character(),
  # authorPub publication counts (R/23_pubs.R). nt_n_pubs is a THRESHOLDED count,
  # never a raw row count, and nt_pub_threshold records the cut that produced it.
  # nt_citations_sum is a lower bound (Semantic Scholar counts, ~50% populated).
  nt_n_pubs = integer(), nt_n_pubs_human_confirmed = integer(),
  nt_citations_sum = double(), nt_pub_threshold = double()
)

# NA-filled publication columns, for the paths where no counts were supplied.
.nt_pub_link_cols <- function(n) {
  tibble::tibble(
    nt_n_pubs = rep(NA_integer_, n),
    nt_n_pubs_human_confirmed = rep(NA_integer_, n),
    nt_citations_sum = rep(NA_real_, n),
    nt_pub_threshold = rep(NA_real_, n))
}

# Collapse the matched NeuroTree person's affiliation rows into one row per
# LinkedIn id: the flags the inclusion rule reads, plus the lineage/era/title
# payload the analysis gains. `title_bridge` is optional (NA columns without it);
# `pq_neuro_classified` is pq_classify_neuro(pq_raw) and supplies nt_pq_neuro.
# `pub_counts` is nt_pub_counts() (R/23_pubs.R) — also optional, NA columns
# without it. Note the 0-vs-NA distinction it creates: with counts supplied, a
# linked pid absent from authorPub gets **0** (searched, nothing found, since the
# shards are the whole corpus); without counts every row gets **NA** (not
# searched). Downstream must not read NA as zero.
build_neurotree_link <- function(nt_matched, nt_persons, people,
                                 nt_people, title_bridge = NULL,
                                 pq_neuro_classified = NULL,
                                 pub_counts = NULL) {
  if (nrow(nt_matched) == 0) return(NT_LINK_SCHEMA)

  advisor_names <- nt_people |>
    dplyr::transmute(nt_advisor_pid = pid,
                     nt_advisor_name = stringr::str_squish(paste(
                       dplyr::coalesce(firstname, ""),
                       dplyr::coalesce(lastname, ""))))

  # Per matched person: does a grad-student / postdoc affiliation exist, and what
  # is the best PhD-era anchor? Grad rows win for year and dissertation title.
  # NB: `nt_relation` is summarised to a collapsed string LAST. dplyr::summarise
  # evaluates sequentially, so a summary column named after a source column
  # shadows it for every later expression in the same call — collapsing it early
  # silently reduced the grad-row subsetting below to a scalar comparison.
  .first_or_na <- function(x) if (length(x) == 0) NA else x[[1]]
  roll <- nt_persons |>
    dplyr::semi_join(nt_matched, by = "nt_pid") |>
    dplyr::group_by(nt_pid) |>
    dplyr::summarise(
      nt_name = dplyr::first(nt_name),
      nt_neuro_grad    = any(nt_relation == NT_RELATION_GRAD),
      nt_neuro_postdoc = any(nt_relation == NT_RELATION_POSTDOC),
      nt_year = .first_or_na(sort(nt_year[nt_relation == NT_RELATION_GRAD &
                                            !is.na(nt_year)])),
      nt_institution = .first_or_na(
        nt_institution[nt_relation == NT_RELATION_GRAD & !is.na(nt_institution)]),
      nt_advisor_pid = .first_or_na(
        nt_advisor_pid[nt_relation == NT_RELATION_GRAD & !is.na(nt_advisor_pid)]),
      nt_dissertation_title = .first_or_na(
        nt_dissertation_title[!is.na(nt_dissertation_title)]),
      nt_relation = paste(sort(unique(nt_relation)), collapse = ","),
      .groups = "drop") |>
    dplyr::mutate(
      nt_year = as.integer(nt_year),
      nt_institution = as.character(nt_institution),
      nt_advisor_pid = as.character(nt_advisor_pid),
      nt_dissertation_title = as.character(nt_dissertation_title))

  bridge <- if (is.null(title_bridge) || nrow(title_bridge) == 0) {
    tibble::tibble(nt_pid = character(), nt_goid = character(),
                   nt_pq_subject_terms = character(),
                   nt_pq_neuro = logical(), nt_neuro_basis = character())
  } else {
    b <- title_bridge |>
      dplyr::filter(corroborated) |>
      dplyr::transmute(nt_pid, nt_goid = as.character(pq_id),
                       nt_pq_subject_terms = pq_subject_all)
    # The bridged dissertation's own neuro classification — the ONLY NeuroTree
    # signal strong enough to count outright. See the tier note below.
    if (!is.null(pq_neuro_classified) && nrow(pq_neuro_classified) > 0) {
      b <- b |>
        dplyr::left_join(
          pq_neuro_classified |>
            dplyr::transmute(nt_goid = as.character(pq_id),
                             nt_pq_neuro = pq_neuro,
                             nt_neuro_basis = neuro_basis),
          by = "nt_goid")
    } else {
      b$nt_pq_neuro <- NA
      b$nt_neuro_basis <- NA_character_
    }
    b
  }

  out <- nt_matched |>
    dplyr::left_join(roll, by = "nt_pid") |>
    dplyr::left_join(advisor_names, by = "nt_advisor_pid") |>
    dplyr::left_join(bridge, by = "nt_pid") |>
    dplyr::left_join(people |> dplyr::select(id, linkedin_url = url) |>
                       dplyr::distinct(), by = c("matched_id" = "id")) |>
    dplyr::mutate(nt_url = nt_profile_url(nt_pid)) |>
    dplyr::transmute(
      id = matched_id, nt_pid, match_type, name_dist, year_dist,
      nt_name, nt_url, linkedin_url, nt_relation,
      nt_neuro_grad = dplyr::coalesce(nt_neuro_grad, FALSE),
      nt_neuro_postdoc = dplyr::coalesce(nt_neuro_postdoc, FALSE),
      nt_pq_neuro = dplyr::coalesce(nt_pq_neuro, FALSE),
      nt_year, nt_institution, nt_advisor_pid, nt_advisor_name,
      nt_dissertation_title, nt_goid, nt_pq_subject_terms, nt_neuro_basis)

  if (is.null(pub_counts) || nrow(pub_counts) == 0) {
    return(dplyr::bind_cols(out, .nt_pub_link_cols(nrow(out))))
  }
  # One threshold produced the whole table (nt_pub_counts() writes it constant),
  # so it applies to the zero-filled rows as much as to the joined ones.
  chosen <- stats::na.omit(pub_counts$pub_threshold)
  chosen <- if (length(chosen) > 0) as.double(chosen[[1]]) else NA_real_
  out |>
    dplyr::left_join(
      pub_counts |>
        dplyr::transmute(nt_pid,
                         nt_n_pubs = n_pubs,
                         nt_n_pubs_human_confirmed = n_pubs_human_confirmed,
                         nt_citations_sum = citations_sum),
      by = "nt_pid") |>
    dplyr::mutate(
      nt_n_pubs = dplyr::coalesce(nt_n_pubs, 0L),
      nt_n_pubs_human_confirmed = dplyr::coalesce(nt_n_pubs_human_confirmed, 0L),
      nt_citations_sum = dplyr::coalesce(nt_citations_sum, 0),
      nt_pub_threshold = chosen)
}

write_neurotree_link <- function(nt_link, path = NEUROTREE_LINK_PATH) {
  readr::write_csv(nt_link, path)
  path
}

# Guarantee the seam exists so the ProQuest store's format="file" target is
# satisfied before this store has ever run. Never clobbers a real link table.
ensure_neurotree_link_stub <- function(path = NEUROTREE_LINK_PATH) {
  if (!file.exists(path)) readr::write_csv(NT_LINK_SCHEMA, path)
  path
}

.read_neurotree_link <- function(nt_link_csv) {
  readr::read_csv(nt_link_csv, show_col_types = FALSE,
                  col_types = readr::cols(
                    id = readr::col_character(),
                    nt_pid = readr::col_character(),
                    match_type = readr::col_character(),
                    name_dist = readr::col_double(),
                    year_dist = readr::col_integer(),
                    nt_name = readr::col_character(),
                    nt_url = readr::col_character(),
                    linkedin_url = readr::col_character(),
                    nt_relation = readr::col_character(),
                    nt_neuro_grad = readr::col_logical(),
                    nt_neuro_postdoc = readr::col_logical(),
                    nt_pq_neuro = readr::col_logical(),
                    nt_year = readr::col_integer(),
                    nt_institution = readr::col_character(),
                    nt_advisor_pid = readr::col_character(),
                    nt_advisor_name = readr::col_character(),
                    nt_dissertation_title = readr::col_character(),
                    nt_goid = readr::col_character(),
                    nt_pq_subject_terms = readr::col_character(),
                    nt_neuro_basis = readr::col_character(),
                    nt_n_pubs = readr::col_integer(),
                    nt_n_pubs_human_confirmed = readr::col_integer(),
                    nt_citations_sum = readr::col_double(),
                    nt_pub_threshold = readr::col_double()))
}

# ---- QC / review artifacts ----

neurotree_match_summary <- function(nt_matched) {
  nt_matched |>
    dplyr::count(match_type) |>
    dplyr::mutate(pct = n / sum(n) * 100)
}

# Match rate on the cohort we can actually check: how many included (Tier-A)
# people got a NeuroTree link. The NeuroTree analogue of proquest_match_rate().
neurotree_match_rate <- function(nt_link, inclusion) {
  tier_a <- inclusion$id[inclusion$tier == "A_include"]
  tibble::tibble(
    n_linked_total = dplyr::n_distinct(nt_link$id),
    n_tier_a = length(tier_a),
    n_tier_a_linked = dplyr::n_distinct(nt_link$id[nt_link$id %in% tier_a]),
    tier_a_rate = dplyr::n_distinct(nt_link$id[nt_link$id %in% tier_a]) /
      max(length(tier_a), 1)
  )
}

# Every accepted FUZZY match, worst-first, with both profile URLs so a reviewer
# can open the two sides and adjudicate by hand. Mirrors
# build_fuzzy_match_review() (R/07_proquest.R:1087). Exact matches are omitted.
build_nt_fuzzy_review <- function(nt_link, persons) {
  nt_link |>
    dplyr::filter(stringr::str_starts(match_type, "fuzzy")) |>
    dplyr::left_join(persons |>
                       dplyr::select(id, li_name = name, li_institution = phd_institution,
                                     li_field = phd_field_text, li_phd_year = phd_year),
                     by = "id") |>
    dplyr::select(id, match_type, name_dist, year_dist,
                  li_name, nt_name, li_institution, nt_institution,
                  li_phd_year, nt_year, li_field, nt_relation,
                  nt_dissertation_title, linkedin_url, nt_url) |>
    dplyr::arrange(dplyr::desc(name_dist))
}

write_nt_fuzzy_review <- function(review, path) {
  readr::write_csv(review, path)
  path
}

write_nt_pq_title_review <- function(title_bridge, path) {
  title_bridge |>
    dplyr::arrange(corroborated, dplyr::desc(inst_agree)) |>
    dplyr::mutate(pq_url = pq_url(pq_id), nt_url = nt_profile_url(nt_pid)) |>
    readr::write_csv(path)
  path
}

