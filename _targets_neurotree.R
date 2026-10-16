# _targets_neurotree.R — NeuroTree / AcademicTree linkage pipeline
#
# Separate store, like _targets_proquest.R: the export is
# person-grain and ~1.9M connect rows, and nothing in the core pipeline should
# rebuild because a NeuroTree table changed.
#
# Run with: Rscript -e 'targets::tar_make(script = "_targets_neurotree.R", store = "_targets_neurotree")'
# Inspect:  Rscript -e 'targets::tar_outdated(script = "_targets_neurotree.R", store = "_targets_neurotree")'
#
# Writes output/tables/neurotree_link.csv — the seam the ProQuest store reads
# (format="file") and folds into build_inclusion(). Run this store BEFORE
# re-running _targets_proquest.R if you want the inclusion rescue to take effect.

library(targets)
library(tarchetypes)

tar_option_set(
  # data.table is here only for fread() on the 131 MB gzipped peopleGrant export
  # (R/24_grants.R) — readr::read_csv() on it is the memory problem this avoids.
  packages = c("tidyverse", "jsonlite", "lubridate", "stringdist", "stringi",
               "data.table"),
  format = "rds",
  error = "abridge"
)

purrr::walk(fs::dir_ls("R", glob = "*.R"), source)

list(
  # ---- LinkedIn side: identical person prep to the ProQuest store ----
  # prepare_pq_persons() is reused verbatim (R/07_proquest.R:598) so both
  # matchers see exactly the same name keys, institution normalization and
  # education-year window.
  tar_target(people_csv,      "data/LinkedIn people profiles.csv", format = "file"),
  # Identity-seeded supplement (acquisition step; not in this repository):
  # people a content-filtered profile pull structurally couldn't reach because their
  # LinkedIn education carries no degree/field text, found instead by NeuroTree
  # name -> SERP -> verified-URL -> Filter-by-URL. Same raw schema as
  # people_csv, so parse_people() reads it unchanged. Unioned in here (not just
  # in the core store) so nt_match_exact/fuzzy can find them on their own merits
  # and populate neurotree_link.csv with a real LinkedIn id.
  tar_target(serp_people_csv, "output/tables/serp_full_pull_collected.csv", format = "file"),
  # All-years top-up (1995-2010, pre-1995, no-year) — same union pattern,
  # separate file.
  tar_target(serp_allyears_people_csv, "output/tables/serp_allyears_pull_collected.csv", format = "file"),
  # ProQuest-seeded SERP cohort — unioned here too so
  # nt_match_exact/fuzzy can find these people and give them advisor lineage.
  tar_target(pq_serp_people_csv, "output/tables/pq_serp_pull_collected.csv", format = "file"),
  # Recovery of people THIS store's own two routes verified but never collected
  # — they already have nt_pids, so folding them in
  # here is what finally turns those verifications into advisor lineage.
  tar_target(serp_recovery_path, ensure_serp_collected_stub("output/tables/serp_recovery_collected.csv")),
  tar_target(serp_recovery_people_csv, serp_recovery_path, format = "file"),
  # Medium-confidence identity filter — see the note in _targets.R. Applied here
  # too so nt_match_* can't hand advisor lineage to the wrong person.
  tar_target(serp_rejects_path, ensure_serp_rejects_stub()),
  tar_target(serp_rejects_csv,  serp_rejects_path, format = "file"),
  tar_target(people,          dplyr::bind_rows(parse_people(people_csv),
                                               parse_people(serp_people_csv),
                                               parse_people(serp_allyears_people_csv),
                                               parse_people(pq_serp_people_csv),
                                               parse_people(serp_recovery_people_csv)) |>
                               dplyr::distinct(id, .keep_all = TRUE) |>
                               dplyr::anti_join(
                                 readr::read_csv(serp_rejects_csv, col_select = "id",
                                                 col_types = readr::cols(.default = "c")),
                                 by = "id")),
  tar_target(education_long,  parse_education(people)),
  tar_target(experience_long, parse_experience(people)),
  tar_target(broad_ids,       broad_phd_base(people, education_long, experience_long)),
  tar_target(broad_features,  build_features(broad_ids, education_long, experience_long)),
  tar_target(li_persons,      prepare_pq_persons(education_long, people, broad_features)),

  # ---- NeuroTree side ----
  tar_target(nt_files, c(fs::path(NT_DIR, "people.csv.gz"),
                         fs::path(NT_DIR, "connect.csv.gz"),
                         fs::path(NT_DIR, "locations.csv.gz")), format = "file"),
  tar_target(nt_raw,     fetch_neurotree(NT_DIR)),
  tar_target(nt_people,  nt_raw$people),
  tar_target(nt_connect, nt_raw$connect),
  tar_target(nt_persons, prepare_nt_persons(nt_raw$people, nt_raw$connect,
                                            nt_raw$locations, neuro_only = TRUE)),

  # ---- link 1: LinkedIn <-> NeuroTree (exact -> fuzzy on the residual) ----
  tar_target(nt_exact,   nt_match_exact(li_persons, nt_persons)),
  tar_target(nt_fuzzy,   nt_match_fuzzy(li_persons, nt_persons, nt_exact)),
  tar_target(nt_matched, .nt_mutual_best(dplyr::bind_rows(nt_exact, nt_fuzzy)) |>
                           dplyr::select(-tie)),
  tar_target(nt_summary, neurotree_match_summary(nt_matched)),

  # ---- link 2: NeuroTree <-> ProQuest on dissertation title ----
  # `connect.pqid` is a UMI publication number and does NOT join to GOID; the
  # normalized title is the working key. See R/18_neurotree.R.
  tar_target(pq_raw,        fetch_proquest()),
  tar_target(pq_diss,       prepare_pq_diss(pq_raw)),
  tar_target(pq_neuro_classified, pq_classify_neuro(pq_raw)),
  tar_target(nt_pq_bridge,  nt_pq_title_bridge(nt_raw$connect, pq_diss)),
  tar_target(nt_pq_title_review_csv,
             write_nt_pq_title_review(nt_pq_bridge,
                                      "output/tables/nt_pq_title_review.csv"),
             format = "file"),

  # ---- publication counts from authorPub (R/23_pubs.R) ----
  # In scope as of 2026-07-29. The shards
  # are 7 x ~18M rows / 1.56 GB gzipped, so read_nt_author_pubs() streams them
  # one at a time in 1e6-row chunks, filtering to linked pids inside the chunk
  # callback — peak memory is a chunk, not a shard. Keep it that way: this store
  # has to fit on the same 16 GB workstation that OOM-killed the core store.
  #
  # The threshold is CALIBRATED against authorPub's human labels (score_human),
  # not taken from the export doc page's 0.3 — a raw count is inflated ~15x. The
  # whole precision/recall grid is written out so the choice is auditable.
  tar_target(nt_pub_shards, ensure_nt_pub_shards(NT_DIR)),
  tar_target(nt_pub_files,  nt_pub_shards, format = "file"),
  tar_target(nt_pub_pids,   unique(nt_matched$nt_pid)),
  tar_target(nt_author_pubs, read_nt_author_pubs(nt_pub_pids,
                                                 paths = nt_pub_files)),
  tar_target(nt_pub_calibration, nt_pub_threshold_calibration(nt_author_pubs)),
  tar_target(nt_pub_calibration_csv,
             write_nt_pub_calibration(nt_pub_calibration,
                                      NT_PUB_CALIBRATION_PATH),
             format = "file"),
  tar_target(nt_pub_threshold, nt_pub_pick_threshold(nt_pub_calibration)),
  tar_target(nt_pub_counts_tbl, nt_pub_counts(nt_author_pubs,
                                              nt_pub_threshold$threshold)),
  tar_target(nt_pub_counts_csv,
             write_nt_pub_counts(nt_pub_counts_tbl, NT_PUB_COUNTS_PATH),
             format = "file"),
  tar_target(nt_pub_summary_tbl, nt_pub_summary(nt_pub_counts_tbl,
                                                nt_pub_threshold)),

  # ---- seam to the ProQuest store (which owns the inclusion rule) ----
  tar_target(nt_link, build_neurotree_link(nt_matched, nt_persons, people,
                                           nt_people, nt_pq_bridge,
                                           pq_neuro_classified,
                                           nt_pub_counts_tbl)),
  tar_target(nt_link_csv, write_neurotree_link(nt_link,
                                               "output/tables/neurotree_link.csv"),
             format = "file"),

  # ---- NIH funding history (R/24_grants.R) ----
  # peopleGrant.csv.gz is 6.95M rows; the reader takes only the columns the
  # feature needs and filters to the pids nt_link actually resolved (~58k rows)
  # before anything else touches it. Seeding from nt_link, not nt_persons, is
  # what keeps it small — an unlinked NeuroTree pid can never reach a LinkedIn
  # person, so its awards are dead weight.
  #
  # The threshold calibration comes back from the SAME read (it has to be
  # computed on all 60,952 human-labeled rows, which mostly sit outside our pid
  # slice), so the 131 MB file is decompressed once per rebuild.
  tar_target(nt_grant_file, fs::path(NT_DIR, NT_GRANT_FILE), format = "file"),
  tar_target(nt_grants_raw, fetch_people_grants(nt_link$nt_pid, nt_grant_file)),
  tar_target(nt_person_grants, build_person_grants(nt_grants_raw$grants)),
  tar_target(nt_grants_csv, write_neurotree_grants(nt_person_grants,
                                                   NT_GRANTS_PATH),
             format = "file"),
  # Threshold justification, rebuilt every run so the chosen NT_GRANT_SCORE_MIN
  # is always auditable against the labels rather than against a stale note.
  tar_target(nt_grant_calibration_csv,
             write_nt_grant_threshold_calibration(nt_grants_raw$calibration,
                                                  NT_GRANT_CALIBRATION_PATH),
             format = "file"),
  # Coverage in its three real states (unknown / zero / funded) plus the source
  # extent facts — max funded year especially. Read this before quoting any
  # funding number.
  tar_target(nt_grant_coverage_summary,
             nt_grant_coverage(nt_link, nt_person_grants,
                               nt_grants_raw$source_extent)),

  # ---- review artifacts ----
  # Every accepted FUZZY match, worst-first, both URLs — hand-audit before
  # anything downstream trusts the fuzzy stage.
  tar_target(nt_fuzzy_review_csv,
             write_nt_fuzzy_review(
               build_nt_fuzzy_review(nt_link, li_persons),
               "output/tables/neurotree_fuzzy_match_review.csv"),
             format = "file")
)
