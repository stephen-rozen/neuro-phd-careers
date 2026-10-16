#!/usr/bin/env Rscript
# data/fetch_neurotree.R — download the AcademicTree/NeuroTree bulk export
#
# Source: Zenodo, DOI 10.5281/zenodo.4247418 (concept DOI; resolves to latest
# version). Granted directly by the AcademicTree maintainer via email, 2026-07-22.
# Two citation/attribution obligations if findings are published:
#   (1) cite the dataset DOI or https://doi.org/10.1038/s41467-018-07034-y
#       (Nature Communications), per the maintainer's email
#   (2) attribute academictree.org, per the CC-BY 3.0 license noted at
#       https://academictree.org/export.php
# To request a refreshed export, contact admin at neurotree.org.
#
# Not a tar_target — this is a large (~1.9 GB), occasional, human-run acquisition
# step. Files are left gzip-compressed
# (.csv.gz); readr::read_csv() reads gzip natively, so there's no need to
# decompress on disk.
#
# Usage: Rscript data/fetch_neurotree.R

suppressPackageStartupMessages({
  library(httr2)
  library(tidyverse)
})

concept_doi_url <- "https://doi.org/10.5281/zenodo.4247418"
out_dir         <- "data/neurotree"
manifest_path   <- "data/dictionaries/neurotree_manifest.csv"

dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# ---- 1. resolve the concept DOI to the current record id -------------------

message("Resolving concept DOI to current Zenodo record ...")
doi_resp   <- request(concept_doi_url) |> req_perform()
final_url  <- resp_url(doi_resp)
record_id  <- str_match(final_url, "zenodo\\.org/records/(\\d+)")[, 2]
if (is.na(record_id)) {
  stop("Could not resolve a Zenodo record id from redirect target: ", final_url,
       call. = FALSE)
}
message(sprintf("  -> record %s (%s)", record_id, final_url))

# ---- 2. pull the live file manifest from the Zenodo REST API ---------------

api_url <- paste0("https://zenodo.org/api/records/", record_id)
record  <- request(api_url) |>
  req_retry(max_tries = 4, backoff = ~ 2^.x) |>
  req_perform() |>
  resp_body_json(simplifyVector = FALSE)

version     <- record$metadata$version %||% NA_character_
pub_date    <- record$metadata$publication_date %||% NA_character_
files       <- record$files
if (is.null(files) || !length(files)) {
  stop("Zenodo record ", record_id, " returned no files.", call. = FALSE)
}

message(sprintf("Record version: %s (published %s), %d files.",
                version %||% "unknown", pub_date %||% "unknown", length(files)))

# ---- 3. download each file, verify checksum ---------------------------------

download_one <- function(f) {
  fname     <- f$key
  size      <- f$size
  checksum  <- f$checksum          # e.g. "md5:xxxxxxxx"
  url       <- f$links$self
  dest      <- file.path(out_dir, fname)

  message(sprintf("Downloading %s (%.1f MB) ...", fname, size / 1e6))
  request(url) |>
    req_retry(max_tries = 4, backoff = ~ 2^.x) |>
    req_perform(path = dest)

  algo   <- str_split_1(checksum, ":")[1]
  expect <- str_split_1(checksum, ":")[2]
  actual <- if (algo == "md5") {
    unname(tools::md5sum(dest))
  } else {
    NA_character_
  }
  if (is.na(actual) || !identical(actual, expect)) {
    stop(sprintf("Checksum mismatch for %s: expected %s (%s), got %s",
                 fname, expect, algo, actual), call. = FALSE)
  }

  tibble(
    filename       = fname,
    size_bytes     = size,
    checksum       = checksum,
    zenodo_record  = record_id,
    record_version = version %||% NA_character_,
    publish_date   = pub_date %||% NA_character_,
    downloaded_at  = as.character(Sys.time())
  )
}

manifest <- map_dfr(files, download_one)

# ---- 4. write manifest sidecar ----------------------------------------------

write_csv(manifest, manifest_path)
message(sprintf("\nDone. %d files verified and written to %s.\nManifest: %s",
                nrow(manifest), out_dir, manifest_path))
