# R/23_pubs.R — publication counts from the NeuroTree `authorPub` export
#
# Scope note: publication counts were added 2026-07-29 (fetch_neurotree()
# originally skipped these shards). Citation linkage to Google Scholar is out
# of scope, and nothing here touches the network.
#
# Source: Zenodo record 19739198, CC-BY 3.0. Attribution to academictree.org is
# required on any published finding, plus a cite of 10.1038/s41467-018-07034-y or
# 10.5281/zenodo.4247418.
#
# WHAT THIS DATA IS, AND WHY A RAW COUNT IS WRONG
#
# `authorPub` is an AUTOMATED name-based author→publication linkage over PubMed /
# Semantic Scholar / Scopus with **no accuracy guarantee** (see
# https://academictree.org/export.php). Every row is a *candidate*
# authorship, not an authorship. Measured on our own linked pids, only 6.6% of
# rows clear the `score_total > 0.3` the export's doc page suggests, so a raw
# per-person row count is badly inflated and must never be reported as a
# publication count.
#
# `score_human` looked like the escape hatch: 1 = human-confirmed author,
# -1 = confirmed NOT author, 0 = unreviewed. `nt_pub_threshold_calibration()`
# scores every candidate `score_total` cut against those labels — and the answer,
# MEASURED 2026-07-30, is that THE LABELS CANNOT CALIBRATE THE SCORE. The export
# writes the human verdict back into `score_total` (confirmed rows carry 1.0,
# rejected rows 0.0), so the grid is flat at precision = recall = 1.00 everywhere
# and says nothing about the unreviewed 99%+. See .nt_labels_informative() for
# the measurement and for why an unhandled flat grid is worse than no
# calibration at all. The pick therefore falls back to the doc page's 0.3, and
# `choice_basis` on the seam records that it is UNVALIDATED, not calibrated.
#
# THREE LIMITS TO STATE WHENEVER THESE COLUMNS ARE PUBLISHED
#
#   1. `nt_n_pubs` rests on an UNVALIDATED threshold (above), and the human
#      labels are additionally not a random sample of `authorPub` — rows get
#      reviewed because someone had a reason to look, typically on their own
#      profile. `nt_pub_threshold_calibration()` reports how many *unlabeled*
#      rows each cut keeps, which is the quantity that actually moves a published
#      count: at 0.3 that is 6.6% of candidate rows, at 0.05 it is 42%. The only
#      route to a real precision number is a hand audit of a random sample of
#      accepted rows (planned).
#   2. `citations` is populated on only ~50% of rows and is a Semantic Scholar
#      count. Every citation sum here is a **lower bound**; the accompanying
#      `nt_citations_n_with_data` says on how many publications it was computable.
#   3. `authorPub` carries NO publication year and no title, so
#      publications-per-year-since-PhD is NOT derivable offline. A PMID→year
#      network lookup would be, and is deliberately not done (no web scraping
#      in this phase). Counts and citation sums only.

# The export ships authorPub in 7 shards named by pubid range (05..35), 1.56 GB
# gzipped. Never bind them all: one shard is ~18M rows.
# Leading "*" because fs::dir_ls() matches the glob against the FULL path.
NT_PUB_SHARD_GLOB <- "*authorPub*.csv.gz"

# Stub target, same contract as ensure_neurotree_link_stub() / the Glassdoor and
# ROR seams: when the shards are absent, hand the reader a header-only file so
# the store is green offline instead of failing on a missing input.
NT_PUB_STUB_PATH <- "output/tables/nt_author_pub_stub.csv"

NT_PUB_CALIBRATION_PATH <- "output/tables/nt_pub_threshold_calibration.csv"
NT_PUB_COUNTS_PATH      <- "output/tables/neurotree_pub_counts.csv"

# Rows per chunk while streaming a shard. The whole point of chunking is that
# peak memory is a chunk, not a shard — this machine has 15.7 GB and the core
# store has already been OOM-killed once (see the memory note in _targets.R).
NT_PUB_CHUNK <- 1e6

# Candidate cuts scored against the human labels. Deliberately spans the whole
# range so the doc page's 0.3 is one row of the audit trail, not the answer.
NT_PUB_THRESHOLD_GRID <- seq(0.05, 0.95, by = 0.05)

# Precision is what a publication count needs — an inflated count is the failure
# mode here, and recall loss is a known, reportable undercount. Smallest cut that
# clears this wins (so recall is maximised subject to precision); if nothing
# clears it, best F1 wins. Both bases are recorded in the returned basis string.
NT_PUB_MIN_PRECISION <- 0.90

# Fallback only for a store with no human labels at all (i.e. the stub). Matches
# the export doc page so the fallback is at least the documented default.
NT_PUB_DEFAULT_THRESHOLD <- 0.3

NT_AUTHOR_PUB_SCHEMA <- tibble::tibble(
  nt_pid = character(), pid = integer(), pubid = integer(),
  score_total = double(), score_human = integer(), citations = double()
)

# score_coa / pmid / doi / s2id are not read: nothing here needs them and each is
# ~18M values per shard. pid/pubid are read as integers rather than characters
# because a chunk of 1e6 character keys costs several times more than the ints.
.NT_PUB_COL_TYPES <- readr::cols_only(
  pid         = readr::col_integer(),
  pubid       = readr::col_integer(),
  score_total = readr::col_double(),
  score_human = readr::col_integer(),
  citations   = readr::col_double()
)

nt_pub_shard_paths <- function(dir = NT_DIR) {
  if (!dir.exists(dir)) return(character())
  sort(as.character(fs::dir_ls(dir, glob = NT_PUB_SHARD_GLOB)))
}

# Resolve the reader's inputs to at least one existing file so a format="file"
# target is always satisfiable (a zero-length file target is an error).
ensure_nt_pub_shards <- function(dir = NT_DIR, stub = NT_PUB_STUB_PATH) {
  paths <- nt_pub_shard_paths(dir)
  if (length(paths) > 0) return(paths)
  if (!file.exists(stub)) {
    readr::write_csv(dplyr::select(NT_AUTHOR_PUB_SCHEMA, -nt_pid), stub)
  }
  stub
}

# ---- reading: stream one shard at a time, filtered to the pids we linked ----
#
# The filter is what makes this tractable. 7 shards x ~18M rows is ~125M
# candidate authorships; our LinkedIn<->NeuroTree link covers ~6.2k pids, and one
# shard alone yielded 174,328 rows for them. Filtering inside the chunk callback
# means a shard is never materialised.

.nt_read_pub_shard <- function(path, keep_pids, chunk_size = NT_PUB_CHUNK) {
  cb <- readr::DataFrameCallback$new(function(chunk, pos) {
    chunk[chunk$pid %in% keep_pids, , drop = FALSE]
  })
  res <- tryCatch(
    readr::read_csv_chunked(path, callback = cb, chunk_size = chunk_size,
                            col_types = .NT_PUB_COL_TYPES, progress = FALSE),
    error = function(e) {
      # Chunked streaming is the memory-safe path. If readr ever refuses to
      # chunk this gzipped stream, fall back to a whole-shard read of the five
      # kept columns (measured 22 s, one shard resident) rather than failing the
      # store — but say so loudly, because the memory profile is not the same.
      warning(sprintf(
        "chunked read of %s failed (%s); falling back to a whole-shard read",
        basename(path), conditionMessage(e)), call. = FALSE)
      whole <- readr::read_csv(path, col_types = .NT_PUB_COL_TYPES,
                               progress = FALSE)
      whole[whole$pid %in% keep_pids, , drop = FALSE]
    })
  if (is.null(res) || nrow(res) == 0) {
    return(dplyr::select(NT_AUTHOR_PUB_SCHEMA, -nt_pid))
  }
  tibble::as_tibble(res)
}

# `pids` is the NeuroTree pid set to keep (character, as everywhere else in this
# project). `paths` is normally the format="file" target so a shard change
# invalidates downstream; pass NULL to resolve from `dir`.
read_nt_author_pubs <- function(pids, paths = NULL, dir = NT_DIR,
                               chunk_size = NT_PUB_CHUNK) {
  if (is.null(paths)) paths <- ensure_nt_pub_shards(dir)
  keep <- suppressWarnings(as.integer(unique(stats::na.omit(as.character(pids)))))
  keep <- keep[!is.na(keep)]
  if (length(keep) == 0 || length(paths) == 0) return(NT_AUTHOR_PUB_SCHEMA)

  out <- vector("list", length(paths))
  for (i in seq_along(paths)) {
    out[[i]] <- .nt_read_pub_shard(paths[[i]], keep, chunk_size)
    message(sprintf("authorPub: %s -> %d rows for %d linked pids",
                    basename(paths[[i]]), nrow(out[[i]]), length(keep)))
    gc()  # never let two shards' worth of allocation coexist
  }

  # (pid, pubid) is the documented unique key; dedup across shards defensively —
  # the shards are pubid-ranged and should not overlap, but a re-export could.
  dplyr::bind_rows(out) |>
    dplyr::distinct(pid, pubid, .keep_all = TRUE) |>
    dplyr::mutate(nt_pid = as.character(pid)) |>
    dplyr::relocate(nt_pid)
}

# ---- threshold calibration against the human labels ----

# One row per candidate cut. tp/fp/fn/tn are counted over the labelled rows only
# (score_human in {1, -1}); n_kept_all / share_kept_all are counted over EVERY
# row, labelled or not, because that is what a published count is made of.
#
# Cuts are applied as `score_total >= threshold` throughout (counts included), so
# the calibration and nt_pub_counts() can never disagree at a boundary.
nt_pub_threshold_calibration <- function(author_pubs,
                                         thresholds = NT_PUB_THRESHOLD_GRID,
                                         min_precision = NT_PUB_MIN_PRECISION) {
  lab <- author_pubs |>
    dplyr::filter(!is.na(score_human), score_human %in% c(1L, -1L)) |>
    dplyr::mutate(is_author = score_human == 1L)
  n_all <- nrow(author_pubs)

  cal <- purrr::map_dfr(thresholds, function(t) {
    pred <- !is.na(lab$score_total) & lab$score_total >= t
    tp <- sum(pred & lab$is_author)
    fp <- sum(pred & !lab$is_author)
    fn <- sum(!pred & lab$is_author)
    tn <- sum(!pred & !lab$is_author)
    kept_all <- sum(!is.na(author_pubs$score_total) & author_pubs$score_total >= t)
    prec <- if (tp + fp > 0) tp / (tp + fp) else NA_real_
    rec  <- if (tp + fn > 0) tp / (tp + fn) else NA_real_
    tibble::tibble(
      threshold = t,
      n_labeled = nrow(lab),
      n_confirmed_author = sum(lab$is_author),
      n_confirmed_not_author = sum(!lab$is_author),
      tp = tp, fp = fp, fn = fn, tn = tn,
      precision = prec, recall = rec,
      f1 = if (!is.na(prec) && !is.na(rec) && prec + rec > 0) {
        2 * prec * rec / (prec + rec)
      } else NA_real_,
      n_rows_total = n_all,
      n_kept_all = kept_all,
      share_kept_all = if (n_all > 0) kept_all / n_all else NA_real_
    )
  })

  # The chosen cut and the rule that chose it are stamped on EVERY row, not just
  # the winning one, so the CSV is self-describing even when the pick falls back
  # to a default that is not on the grid.
  pick <- nt_pub_pick_threshold(cal, min_precision)
  cal |>
    dplyr::mutate(
      min_precision_target = min_precision,
      labels_informative = .nt_labels_informative(cal),
      chosen_threshold = pick$threshold,
      choice_basis = pick$basis,
      chosen = dplyr::near(threshold, pick$threshold))
}

# MEASURED 2026-07-30, AND THE REASON THIS FUNCTION EXISTS: the human labels
# CANNOT calibrate score_total, because the export writes the verdict back into
# the score. On our pids in authorPub05, every score_human == 1 row carries
# score_total exactly 1.0 (n = 510) and every score_human == -1 row exactly 0.0
# (n = 196). Checked wider on authorPub20 over ALL pids (134,127 labelled rows):
# negatives top out at 0.200, positives bottom out at 0.289, with 8 exceptions in
# 134k. The classes are perfectly separated at every cut in the grid, so
# precision = recall = 1.00 everywhere and the grid is FLAT.
#
# A flat grid is not a strong result, it is an ABSENT one — it says nothing about
# where to cut the unreviewed rows, which are 99%+ of the corpus. Left unhandled
# it is actively harmful: "smallest cut clearing precision 0.90" then returns the
# grid MINIMUM (0.05), which keeps 42% of candidate rows and produces exactly the
# inflated count the calibration was supposed to prevent.
#
# So: detect the flat grid and refuse to choose from it.
.nt_labels_informative <- function(cal) {
  if (is.null(cal) || nrow(cal) == 0) return(FALSE)
  p <- round(cal$precision[!is.na(cal$precision)], 6)
  r <- round(cal$recall[!is.na(cal$recall)], 6)
  length(p) > 0 && (dplyr::n_distinct(p) > 1 || dplyr::n_distinct(r) > 1)
}

# Smallest cut clearing `min_precision`; failing that, best F1. Both are gated
# behind .nt_labels_informative() — on this export the labels are NOT informative
# (see its comment), so in practice this returns the documented 0.3 default with
# `labels_uninformative_score_overwritten`. The basis string travels with the
# number so a reader never has to guess which rule fired, and 0.3 arriving via
# that basis is an UNVALIDATED cut, not a calibrated one.
nt_pub_pick_threshold <- function(calibration,
                                  min_precision = NT_PUB_MIN_PRECISION) {
  empty <- is.null(calibration) || nrow(calibration) == 0 ||
    all(is.na(calibration$precision))
  if (empty) {
    return(list(threshold = NT_PUB_DEFAULT_THRESHOLD,
                basis = "default_no_human_labels",
                precision = NA_real_, recall = NA_real_, f1 = NA_real_,
                n_labeled = 0L))
  }
  if (!.nt_labels_informative(calibration)) {
    return(list(threshold = NT_PUB_DEFAULT_THRESHOLD,
                basis = "labels_uninformative_score_overwritten",
                precision = NA_real_, recall = NA_real_, f1 = NA_real_,
                n_labeled = as.integer(calibration$n_labeled[[1]])))
  }
  ok <- calibration |>
    dplyr::filter(!is.na(precision), precision >= min_precision, tp > 0) |>
    dplyr::arrange(threshold)
  row <- if (nrow(ok) > 0) {
    dplyr::mutate(dplyr::slice_head(ok, n = 1),
                  basis = sprintf("min_threshold_with_precision_ge_%.2f",
                                  min_precision))
  } else {
    calibration |>
      dplyr::filter(!is.na(f1)) |>
      dplyr::slice_max(f1, n = 1, with_ties = FALSE) |>
      dplyr::mutate(basis = "max_f1_precision_target_unreachable")
  }
  if (nrow(row) == 0) {
    return(list(threshold = NT_PUB_DEFAULT_THRESHOLD,
                basis = "default_no_usable_labels",
                precision = NA_real_, recall = NA_real_, f1 = NA_real_,
                n_labeled = 0L))
  }
  list(threshold = row$threshold[[1]], basis = row$basis[[1]],
       precision = row$precision[[1]], recall = row$recall[[1]],
       f1 = row$f1[[1]], n_labeled = as.integer(row$n_labeled[[1]]))
}

write_nt_pub_calibration <- function(calibration,
                                     path = NT_PUB_CALIBRATION_PATH) {
  readr::write_csv(calibration, path)
  path
}

# ---- per-pid counts ----

NT_PUB_COUNTS_SCHEMA <- tibble::tibble(
  nt_pid = character(), n_pubs = integer(),
  n_pubs_human_confirmed = integer(), n_pubs_human_rejected = integer(),
  n_pubs_candidate_all = integer(), citations_sum = double(),
  citations_n_with_data = integer(), pub_threshold = double()
)

# Acceptance rule, in precedence order:
#   score_human ==  1  -> counted regardless of score_total (a human said yes)
#   score_human == -1  -> dropped regardless of score_total (a human said no)
#   otherwise          -> counted iff score_total >= threshold
#
# `n_pubs_candidate_all` is the unfiltered row count, kept so the inflation is
# visible rather than implied: it is what a naive count would have reported.
nt_pub_counts <- function(author_pubs, threshold = NT_PUB_DEFAULT_THRESHOLD) {
  if (is.null(author_pubs) || nrow(author_pubs) == 0) {
    return(NT_PUB_COUNTS_SCHEMA)
  }
  author_pubs |>
    dplyr::mutate(
      .confirmed = !is.na(score_human) & score_human == 1L,
      .rejected  = !is.na(score_human) & score_human == -1L,
      .accepted  = .confirmed |
        (!.rejected & !is.na(score_total) & score_total >= threshold)) |>
    dplyr::group_by(nt_pid) |>
    dplyr::summarise(
      n_pubs                 = sum(.accepted),
      n_pubs_human_confirmed = sum(.confirmed),
      n_pubs_human_rejected  = sum(.rejected),
      n_pubs_candidate_all   = dplyr::n(),
      # Lower bound by construction: `citations` is absent on ~half the rows.
      citations_sum          = sum(citations[.accepted], na.rm = TRUE),
      citations_n_with_data  = sum(.accepted & !is.na(citations)),
      .groups = "drop") |>
    dplyr::mutate(
      dplyr::across(c(n_pubs, n_pubs_human_confirmed, n_pubs_human_rejected,
                      n_pubs_candidate_all, citations_n_with_data), as.integer),
      citations_sum = as.double(citations_sum),
      pub_threshold = as.double(threshold))
}

write_nt_pub_counts <- function(pub_counts, path = NT_PUB_COUNTS_PATH) {
  readr::write_csv(pub_counts, path)
  path
}

# QC one-liner: how inflated the naive count would have been, and how far the
# citation sums are from complete. Reported, never silently absorbed.
nt_pub_summary <- function(pub_counts, threshold_pick = NULL) {
  if (is.null(pub_counts) || nrow(pub_counts) == 0) {
    return(tibble::tibble(n_pids = 0L, n_pubs_total = 0L,
                          n_pubs_candidate_total = 0L, kept_share = NA_real_,
                          median_n_pubs = NA_real_,
                          n_pids_human_confirmed = 0L,
                          citations_coverage = NA_real_,
                          threshold = NA_real_, threshold_basis = NA_character_))
  }
  tibble::tibble(
    n_pids = nrow(pub_counts),
    n_pubs_total = sum(pub_counts$n_pubs),
    n_pubs_candidate_total = sum(pub_counts$n_pubs_candidate_all),
    kept_share = sum(pub_counts$n_pubs) /
      max(sum(pub_counts$n_pubs_candidate_all), 1),
    median_n_pubs = stats::median(pub_counts$n_pubs),
    n_pids_human_confirmed = sum(pub_counts$n_pubs_human_confirmed > 0),
    citations_coverage = sum(pub_counts$citations_n_with_data) /
      max(sum(pub_counts$n_pubs), 1),
    threshold = if (is.null(threshold_pick)) pub_counts$pub_threshold[[1]]
                else threshold_pick$threshold,
    threshold_basis = if (is.null(threshold_pick)) NA_character_
                      else threshold_pick$basis)
}
