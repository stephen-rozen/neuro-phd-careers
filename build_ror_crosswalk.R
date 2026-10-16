#!/usr/bin/env Rscript
# build_ror_crosswalk.R — build the inst_norm -> ROR id crosswalk and measure it
# against `norm_institution` before anything is wired in.
#
# Reads the ROR dump staged by data/fetch_ror.R (not in git — 285 MB, CC0,
# regenerable) plus the institution strings from BOTH sides of the ProQuest
# join, and writes:
#
#   output/tables/ror_institution_crosswalk.csv   the artifact the pipeline reads
#   output/tables/ror_crosswalk_merge_review.csv  every inst_norm group ROR pools
#   output/tables/ror_vs_norm_institution.csv     per-string scheme comparison
#
# Human-run and offline, same contract as data/fetch_ror.R — NOT a tar_target,
# because it parses a 50 MB dump that lives outside the repo. The pipeline
# consumes the crosswalk CSV through a stub-guarded format="file" target.
#
# Usage:  Rscript build_ror_crosswalk.R
#
# Paths are overridable so the script can run from a worktree against the
# main checkout's gitignored data:
#   ROR_DIR   directory holding *ror-data.csv   (default data/ror/extracted)
#   PQ_STORE  targets store for education_long / pq_diss (default _targets_proquest)
#   ROR_CACHE .rds path to cache the ROR name-variant parse across runs (off by
#             default; useful only when tuning the resolution ladder)

suppressPackageStartupMessages({
  library(tidyverse)
  library(targets)
})

purrr::walk(fs::dir_ls("R", glob = "*.R"), source)

ror_dir  <- Sys.getenv("ROR_DIR",  ROR_DIR)
pq_store <- Sys.getenv("PQ_STORE", "_targets_proquest")

message("== inputs ==")
message("  ROR dump : ", ror_dir)
message("  pq store : ", pq_store)

# ---- institution strings, both sides ----------------------------------------

edu <- tar_read(education_long, store = pq_store)
li <- edu |>
  dplyr::filter(!is.na(institution), stringr::str_squish(institution) != "") |>
  dplyr::count(institution, name = "n")

pq <- tar_read(pq_diss, store = pq_store) |>
  dplyr::filter(!is.na(institution), stringr::str_squish(institution) != "") |>
  dplyr::count(institution, name = "n")

message(sprintf("  LinkedIn : %s distinct strings over %s education rows",
                format(nrow(li), big.mark = ","), format(sum(li$n), big.mark = ",")))
message(sprintf("  ProQuest : %s distinct schools over %s dissertations",
                format(nrow(pq), big.mark = ","), format(sum(pq$n), big.mark = ",")))

# Break ties on the string itself: these artifacts are committed, and a
# frequency-only sort reshuffles tens of thousands of equal-count rows on every
# rebuild, burying the real diff.
readr::write_csv(li |> dplyr::arrange(dplyr::desc(n), institution),
                 "output/tables/li_institution_strings.csv")

# ---- ROR variants + lookups --------------------------------------------------

message("\n== ROR dump ==")
# Parsing 130k orgs into 400k name variants takes minutes and the dump only
# changes when data/fetch_ror.R is re-run. Set ROR_CACHE to an .rds path outside
# the repo to reuse the parse across runs when tuning; off by default, since
# data/ is read-only and output/ takes tables, not intermediates.
cache <- Sys.getenv("ROR_CACHE", "")
if (nzchar(cache) && file.exists(cache) &&
    file.mtime(cache) > file.mtime(ror_csv_path(ror_dir))) {
  message("  reusing cached lookups: ", cache)
  lookups <- readRDS(cache)
} else {
  orgs <- read_ror_orgs(ror_csv_path(ror_dir))
  message("  active orgs      : ", format(nrow(orgs), big.mark = ","))
  variants <- build_ror_variants(orgs)
  message("  name variants    : ", format(nrow(variants), big.mark = ","))
  lookups <- build_ror_lookups(variants, orgs)
  message("  parent edges     : ", format(nrow(lookups$parents), big.mark = ","))
  if (nzchar(cache)) saveRDS(lookups, cache)
}
message(sprintf("  exact keys       : %s  (%s collide)",
                format(nrow(lookups$exact), big.mark = ","),
                format(sum(lookups$exact$n_orgs > 1), big.mark = ",")))
message(sprintf("  core keys        : %s  (%s collide)",
                format(nrow(lookups$core), big.mark = ","),
                format(sum(lookups$core$n_orgs > 1), big.mark = ",")))
message("  fuzzy candidates : ", format(nrow(lookups$fuzzy), big.mark = ","))

# ---- resolve both sides ------------------------------------------------------

message("\n== resolving institution strings ==")
all_strings <- dplyr::bind_rows(li, pq) |>
  dplyr::summarise(n = sum(n), .by = institution)

resolved <- resolve_ror_strings(all_strings$institution, lookups)
res_n <- resolved |>
  dplyr::left_join(all_strings, by = "institution") |>
  dplyr::mutate(ror_tier = dplyr::coalesce(ror_tier, "unresolved"))
message("  per-string resolution (distinct / weighted by rows):")
res_n |>
  dplyr::summarise(strings = dplyr::n(), rows = sum(n), .by = ror_tier) |>
  dplyr::arrange(match(ror_tier, c("exact", "core", "fuzzy", "unresolved"))) |>
  dplyr::mutate(pct_strings = sprintf("%.1f%%", 100 * strings / sum(strings)),
                pct_rows    = sprintf("%.1f%%", 100 * rows / sum(rows))) |>
  as.data.frame() |>
  print(row.names = FALSE)

# ---- crosswalk ---------------------------------------------------------------

message("\n== crosswalk (keyed on inst_norm) ==")
crosswalk <- build_ror_crosswalk(all_strings, lookups, resolved)
readr::write_csv(crosswalk, ROR_CROSSWALK_PATH)
message(sprintf("  groups           : %s", format(nrow(crosswalk), big.mark = ",")))
message(sprintf("  with a ROR id    : %s (%.1f%% of groups, %.1f%% weighted)",
                format(sum(!is.na(crosswalk$ror_id)), big.mark = ","),
                100 * mean(!is.na(crosswalk$ror_id)),
                100 * sum(crosswalk$n[!is.na(crosswalk$ror_id)]) / sum(crosswalk$n)))
message(sprintf("  merge declined   : %s groups",
                format(sum(!is.na(crosswalk$ror_blocked_reason)), big.mark = ",")))
message("  wrote ", ROR_CROSSWALK_PATH)

cat("\n  PhD-institution country coverage (weighted by education rows):\n")
print(as.data.frame(crosswalk |>
  dplyr::mutate(bucket = dplyr::case_when(
    country_conflict ~ "conflict (treated as unknown)",
    is.na(country)   ~ "no trusted country",
    country %in% ROR_US_COUNTRIES ~ "US (incl. PR)",
    TRUE             ~ "non-US")) |>
  dplyr::summarise(groups = dplyr::n(), rows = sum(n), .by = bucket) |>
  dplyr::mutate(pct_rows = sprintf("%.1f%%", 100 * rows / sum(rows))) |>
  dplyr::arrange(dplyr::desc(rows))), row.names = FALSE)

country_review <- build_ror_country_review(crosswalk)
invisible(write_ror_country_review(country_review))
message(sprintf("  country conflicts: %s groups (%s rows) -> %s",
                format(nrow(country_review), big.mark = ","),
                format(sum(country_review$n), big.mark = ","),
                ROR_COUNTRY_REVIEW_PATH))

merge_review <- build_ror_merge_review(crosswalk)
invisible(write_ror_merge_review(merge_review))
message(sprintf("  merges           : %s ROR ids pool >1 inst_norm group (%s rows)",
                format(nrow(merge_review), big.mark = ","),
                format(sum(merge_review$rows), big.mark = ",")))
message("  wrote ", ROR_MERGE_REVIEW_PATH)

# ---- the measurement ---------------------------------------------------------
#
# Coverage = share of LinkedIn education ROWS whose institution has a ProQuest
# counterpart under a given scheme, i.e. can block at all.

li_norm <- norm_institution(li$institution)
pq_norm <- norm_institution(pq$institution)

# Scheme 1 — today.
li_k_norm <- li_norm
pq_k_norm <- pq_norm

# Scheme 2 — ROR as a REPLACEMENT, exact variant only, collisions dropped. This
# reproduces the 2026-07-27 build that measured 23.9%, as a control.
exact_unique <- lookups$exact |> dplyr::filter(n_orgs == 1)
.old_ror <- function(x) exact_unique$ror_id[match(ror_key(x), exact_unique$key)]
li_k_old <- .old_ror(li$institution)
pq_k_old <- .old_ror(pq$institution)

# Scheme 3 — ROR as a REPLACEMENT with the new ladder (exact -> core -> fuzzy),
# still resolving the RAW string and still ignoring inst_norm.
.new_ror <- function(x) resolved$ror_id[match(x, resolved$institution)]
li_k_new <- .new_ror(li$institution)
pq_k_new <- .new_ror(pq$institution)

# Scheme 4 — ROR as a SUPPLEMENT: coalesce(ror_id, inst_norm) on the group.
li_k_blk <- ror_block_key(li_norm, crosswalk)
pq_k_blk <- ror_block_key(pq_norm, crosswalk)

.reach <- function(li_key, pq_key) {
  ok <- !is.na(li_key) & li_key != ""
  pool <- unique(pq_key[!is.na(pq_key) & pq_key != ""])
  ok & li_key %in% pool
}

r_norm <- .reach(li_k_norm, pq_k_norm)
r_old  <- .reach(li_k_old,  pq_k_old)
r_new  <- .reach(li_k_new,  pq_k_new)
r_blk  <- .reach(li_k_blk,  pq_k_blk)

tot <- sum(li$n)
pct <- function(v) 100 * sum(li$n[v]) / tot

cat("\n=== LinkedIn education rows whose institution has a ProQuest counterpart ===\n")
cat(sprintf("  %-46s %6.1f%%\n", "norm_institution (today)", pct(r_norm)))
cat(sprintf("  %-46s %6.1f%%\n", "ROR replacement, exact-variant (2026-07-27)", pct(r_old)))
cat(sprintf("  %-46s %6.1f%%\n", "ROR replacement, new ladder", pct(r_new)))
cat(sprintf("  %-46s %6.1f%%\n", "SUPPLEMENT coalesce(ror_id, inst_norm)", pct(r_blk)))
cat(sprintf("  %-46s %6.1f%%\n", "either norm or ROR (union)", pct(r_norm | r_new)))
cat("\n")
cat(sprintf("  %-46s %+6.1f%%\n", "net NEW vs today (supplement)", pct(r_blk & !r_norm)))
cat(sprintf("  %-46s %+6.1f%%\n", "would LOSE  -- exact-variant replacement", -pct(r_norm & !r_old)))
cat(sprintf("  %-46s %+6.1f%%\n", "would LOSE  -- new-ladder replacement", -pct(r_norm & !r_new)))
cat(sprintf("  %-46s %+6.1f%%   <-- must be 0.0\n", "would LOSE  -- SUPPLEMENT", -pct(r_norm & !r_blk)))

lost <- li[r_norm & !r_blk, ] |> dplyr::arrange(dplyr::desc(n))
if (nrow(lost) > 0) {
  cat("\n!! the supplement LOSES these institutions:\n")
  print(as.data.frame(head(lost, 25)), row.names = FALSE)
} else {
  cat("\n  supplement loses nothing (lossless by construction -- see R/20_ror.R).\n")
}

gained <- li[r_blk & !r_norm, ] |> dplyr::arrange(dplyr::desc(n))
cat(sprintf("\ntop institutions the supplement newly reaches (%s strings, %s rows):\n",
            format(nrow(gained), big.mark = ","),
            format(sum(gained$n), big.mark = ",")))
print(as.data.frame(head(gained, 25)), row.names = FALSE)

readr::write_csv(
  tibble::tibble(institution = li$institution, n = li$n,
                 norm = li_k_norm, ror = li_k_new,
                 block = li_k_blk,
                 norm_shared = r_norm, ror_shared = r_new,
                 block_shared = r_blk) |>
    dplyr::arrange(dplyr::desc(n), institution),
  "output/tables/ror_vs_norm_institution.csv")
message("\nwrote output/tables/ror_vs_norm_institution.csv")

cat("\ntop merges (ROR ids pooling >1 inst_norm group), worst-first by rows:\n")
print(as.data.frame(head(merge_review |>
                           dplyr::mutate(merged_inst_norm =
                             stringr::str_trunc(merged_inst_norm, 90)), 25)),
      row.names = FALSE)
