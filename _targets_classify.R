# _targets_classify.R — dissertation content-area / methodology classification
#
# LLM (Claude) classification of neuro dissertations into two orthogonal
# 4-category schemes: content area and primary methodology. Separated from
# _targets_proquest.R for the same reason as before — it works over the neuro
# subset of ProQuest dissertations, which is a slow join to redo inline.
#
# REPLACED 2026-07-31: this store used to run a from-scratch BoW nearest-
# centroid classifier over a 13-category taxonomy, plus an OpenAlex crosswalk
# comparison. Both are gone — see the R/08_classify.R header for why.
#
# THIS STORE NEVER CALLS THE LIVE LLM API ITSELF. `classify_dissertations_llm()`
# defaults to backend = "stub" (returns NA, offline-safe) and prefers a cache
# CSV when one exists. The only thing that ever hits the network is
# `refresh_llm_classify_cache()`, run BY HAND — billable, same convention as
# the data-acquisition steps. Run it once, then this store just reads
# the cache it wrote.
#
# Run with: Rscript -e 'targets::tar_make(script = "_targets_classify.R", store = "_targets_classify")'
# Inspect:  Rscript -e 'targets::tar_outdated(script = "_targets_classify.R", store = "_targets_classify")'

library(targets)
library(tarchetypes)

tar_option_set(
  packages = c("tidyverse", "jsonlite", "lubridate", "stringdist", "fuzzyjoin", "httr2"),
  format = "rds",
  error = "abridge"
)

purrr::walk(fs::dir_ls("R", glob = "*.R"), source)

list(
  # ---- inputs: read proquest store artifacts ----
  tar_target(pq_raw, fetch_proquest()),
  tar_target(pq_neuro_classified, pq_classify_neuro(pq_raw)),
  tar_target(pq_neuro_only,
             pq_neuro_classified |> dplyr::filter(pq_neuro) |>
               dplyr::mutate(text = stringr::str_squish(paste(
                 dplyr::coalesce(title, ""), dplyr::coalesce(keywords, ""))))),

  # ---- LLM content-area / methodology classification ----
  tar_target(pq_llm_classified,
             classify_dissertations_llm(
               pq_neuro_only,
               backend    = "stub",
               cache_path = "output/tables/pq_llm_classify_cache.csv")),
  tar_target(pq_llm_summary, classify_llm_summary(pq_llm_classified)),
  tar_target(pq_llm_summary_csv,
             write_classify_llm_summary(pq_llm_summary,
                                        "output/tables/pq_llm_classify_summary.csv"),
             format = "file")
)
