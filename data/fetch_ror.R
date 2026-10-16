#!/usr/bin/env Rscript
# data/fetch_ror.R — download the ROR (Research Organization Registry) data dump
#
# Source: Zenodo community `ror-data`, concept DOI 10.5281/zenodo.6347574
# (resolves to the latest monthly release). License: **CC0 1.0 Universal**
# (public domain dedication) — no attribution obligation, unlike the NeuroTree
# export. https://ror.org/
#
# WHY: `R/07_proquest.R::norm_institution` blocks on an exact normalized
# token-set match, reached through ~140 lines of hand-curated aliases. Measured
# 2026-07-27 against the rebuilt store: **53,368 distinct institution strings**
# across 271,410 education rows, and **75.4% of them appear exactly once**.
# Hand-curation cannot reach a 40,000-string singleton tail — that is one alias
# per person, forever. A registry crosswalk generalizes to spellings nobody has
# seen, which is the entire point.
#
# SCOPE NOTE — **country is in scope (2026-07-28).**
# The analysis focuses on US PhD institutions. ROR's `country` is the
# only source of that fact in the pipeline, so the crosswalk emits it and
# `prepare_pq_persons()` attaches `phd_country` / `phd_is_us` to each person.
# It is a LABEL: non-US people stay collected and stay in the tables. There is
# no country FILTER in the pipeline — scoping belongs at the analysis layer.
# See METHODS.md, "Country labels".
#
# Not a tar_target — a large, occasional, human-run acquisition step, same
# contract as data/fetch_neurotree.R.
# This is the one write into data/ that is sanctioned (staging external source
# data), matching the NeuroTree precedent.
#
# Usage: Rscript data/fetch_ror.R

suppressPackageStartupMessages({
  library(httr2)
  library(tidyverse)
})

DEST_DIR   <- "data/ror"
ZENODO_API <- "https://zenodo.org/api/records"
CONCEPT_ID <- "6347574"   # ror-data concept record; resolves to latest version

dir.create(DEST_DIR, showWarnings = FALSE, recursive = TRUE)

message("== resolving latest ROR release from Zenodo ==")
rec <- request(ZENODO_API) |>
  req_url_query(q = paste0("conceptrecid:", CONCEPT_ID),
                sort = "mostrecent", size = 1, all_versions = "false") |>
  req_retry(max_tries = 4, backoff = ~ 2^.x) |>
  req_perform() |>
  resp_body_json()

hits <- rec$hits$hits
if (length(hits) == 0) {
  stop("No Zenodo record found for concept ", CONCEPT_ID,
       ". Check https://zenodo.org/communities/ror-data manually.", call. = FALSE)
}
latest <- hits[[1]]
message("  release: ", latest$metadata$title %||% "?",
        "  (published ", latest$metadata$publication_date %||% "?", ")")

files <- latest$files
if (length(files) == 0) stop("Release carries no files.", call. = FALSE)

for (f in files) {
  fname <- f$key
  dest  <- file.path(DEST_DIR, fname)
  size_mb <- round((f$size %||% 0) / 1e6, 1)
  if (file.exists(dest) && file.size(dest) == (f$size %||% -1)) {
    message(sprintf("  %-52s already present (%.1f MB)", fname, size_mb))
    next
  }
  message(sprintf("  downloading %-42s (%.1f MB) ...", fname, size_mb))
  request(f$links$self) |>
    req_retry(max_tries = 4, backoff = ~ 2^.x) |>
    req_perform(path = dest)
}

message("\n== staged to ", DEST_DIR, " ==")
print(fs::dir_info(DEST_DIR) |> dplyr::select(path, size))

message("\nThe dump ships a JSON (and usually CSV) of every organization with:")
message("  id (ROR ID), name, aliases, acronyms, labels (translated names),")
message("  country, types, links. Build the crosswalk off name + aliases +")
message("  acronyms + labels -- the alias/label fields are what let foreign")
message("  spellings resolve, which is exactly where norm_institution fails.")
message("\nNEXT: build the institution -> ROR ID crosswalk and block on the ID")
message("instead of the token-set string. Cache it as a static file; do NOT")
message("make it a per-run network call. No country gate (see header).")
