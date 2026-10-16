# _targets_proquest.R — ProQuest dissertation linkage pipeline
#
# Separate from _targets.R so it doesn't entangle with Phase 1.
# Run with: Rscript -e 'targets::tar_make(script = "_targets_proquest.R", store = "_targets_proquest")'
# Inspect:  Rscript -e 'targets::tar_outdated(script = "_targets_proquest.R", store = "_targets_proquest")'

library(targets)
library(tarchetypes)

tar_option_set(
  packages = c("tidyverse", "jsonlite", "lubridate", "stringdist", "fuzzyjoin"),
  format = "rds",
  error = "abridge"
)

# Source all R/ helpers (same as main pipeline)
purrr::walk(fs::dir_ls("R", glob = "*.R"), source)

list(
  # ---- inputs: borrow Phase 1 artifacts via tar_read ----
  tar_target(people_csv,      "data/LinkedIn people profiles.csv", format = "file"),
  tar_target(company_csv,     "data/LinkedIn company information.csv", format = "file"),
  # SERP-acquired supplement — see _targets_neurotree.R for why. Must be
  # unioned in here too: build_inclusion() below only sees ids present in
  # `people`/`broad_features`, so without this the SERP people would gain a
  # neurotree_link.csv row but never appear in the inclusion universe.
  tar_target(serp_people_csv, "output/tables/serp_full_pull_collected.csv", format = "file"),
  # All-years top-up (1995-2010, pre-1995, no-year) — same union pattern,
  # separate file.
  tar_target(serp_allyears_people_csv, "output/tables/serp_allyears_pull_collected.csv", format = "file"),
  # ProQuest-seeded SERP cohort: unmatched core-neuro
  # dissertation authors resolved to LinkedIn. Unioned here so pq_match_* can
  # re-find them on their own merits and build_inclusion() sees them at all.
  tar_target(pq_serp_people_csv, "output/tables/pq_serp_pull_collected.csv", format = "file"),
  # Recovery of people the two NeuroTree routes verified but never collected
  # — unioned here so pq_match_* can find them.
  tar_target(serp_recovery_path, ensure_serp_collected_stub("output/tables/serp_recovery_collected.csv")),
  tar_target(serp_recovery_people_csv, serp_recovery_path, format = "file"),
  # Medium-confidence identity filter — see the note in _targets.R. It matters
  # most here: these people reach Tier A through pq_matched, so a misresolved
  # profile binds a real neuro dissertation to a stranger's career history.
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
  # Real company rows fetched by the company pull — the delivered
  # company_csv is a 1,000-row sample with a measured 0.0% join rate against the
  # companies our people actually work at. See the note in _targets.R.
  tar_target(companies_pull_path, ensure_serp_collected_stub(
    "output/tables/companies_pull_collected.csv",
    schema_from = "data/LinkedIn company information.csv")),
  tar_target(companies_pull_csv, companies_pull_path, format = "file"),
  tar_target(companies,       dplyr::bind_rows(
                                readr::read_csv(company_csv, show_col_types = FALSE),
                                readr::read_csv(companies_pull_csv, show_col_types = FALSE)) |>
                               dplyr::distinct(id, .keep_all = TRUE)),
  tar_target(filter_funnel,   build_filter_funnel(people, education_long, experience_long)),
  tar_target(analytic_ids,    filter_funnel |> dplyr::filter(stage == "final") |> dplyr::pull(id)),
  tar_target(features,        build_features(analytic_ids, education_long, experience_long)),

  # ---- broad base: EVERY collected person (collection already gated to PhD) ----
  tar_target(broad_ids,      broad_phd_base(people, education_long, experience_long)),
  tar_target(broad_features, build_features(broad_ids, education_long, experience_long)),

  # ---- ROR institution crosswalk (written by build_ror_crosswalk.R) ----
  # SUPPLEMENT to norm_institution, not a replacement: the matchers block on
  # coalesce(ror_id, inst_norm). Stub-guarded like the NeuroTree seam, and an
  # empty crosswalk makes the block key identical to today's inst_norm. See
  # R/20_ror.R for why this is lossless by construction.
  tar_target(ror_crosswalk_path, ensure_ror_crosswalk_stub(ROR_CROSSWALK_PATH)),
  tar_target(ror_crosswalk_file, ror_crosswalk_path, format = "file"),
  tar_target(ror_crosswalk,      read_ror_crosswalk(ror_crosswalk_file)),

  # ---- ProQuest matching: exact (cached fast path) → fuzzy on the residual ----
  tar_target(pq_raw,         fetch_proquest()),
  tar_target(pq_persons,     prepare_pq_persons(education_long, people,
                                                broad_features, ror_crosswalk)),
  tar_target(pq_diss,        prepare_pq_diss(pq_raw, ror_crosswalk)),
  tar_target(pq_exact,       pq_match_exact(pq_persons, pq_diss)),
  tar_target(pq_fuzzy,       pq_match_fuzzy(pq_persons, pq_diss, pq_exact)),
  tar_target(pq_matched,     assemble_pq_matched(pq_raw, pq_exact, pq_fuzzy)),
  tar_target(pq_summary,     proquest_match_summary(pq_matched)),
  # Backfill phd_year from matched dissertations (for people LinkedIn can't date).
  tar_target(broad_features_enriched,
             backfill_phd_year(broad_features, pq_matched, pq_raw)),

  # ---- NeuroTree seam (written by _targets_neurotree.R) ----
  # Stub-guarded like the Glassdoor seam, so this store stays green before the
  # NeuroTree store has ever run. A real link table changes the file hash and
  # re-adjudicates inclusion.
  tar_target(nt_link_path,  ensure_neurotree_link_stub("output/tables/neurotree_link.csv")),
  tar_target(nt_link_file,  nt_link_path, format = "file"),
  tar_target(nt_link,       .read_neurotree_link(nt_link_file)),
  # NIH funding history, same seam contract and same stub guard. It is joined
  # onto inclusion_table.csv via nt_pid and feeds NO inclusion decision — a grant
  # is a career OUTCOME, so letting it into build_inclusion() would make the
  # sample definition depend on the outcome being measured.
  tar_target(nt_grants_path, ensure_neurotree_grants_stub(NT_GRANTS_PATH)),
  tar_target(nt_grants_file, nt_grants_path, format = "file"),
  tar_target(nt_grants,      .read_neurotree_grants(nt_grants_file)),

  # ---- tiered, OR-of-evidence inclusion (supersedes the 02_filter gate) ----
  # Universe = all collected people; the tiered rule narrows it: education /
  # self-ID / ProQuest-neuro / NeuroTree-grad count outright; experience-only,
  # feeder-field and NeuroTree-postdoc people are adjudicated by ProQuest.
  tar_target(inclusion,      build_inclusion(broad_features, education_long,
                                             experience_long, people,
                                             pq_raw, pq_matched, nt_link)),
  tar_target(inclusion_tiers, inclusion_summary(inclusion)),
  tar_target(nt_match_rate,  neurotree_match_rate(nt_link, inclusion)),
  # Seam to the core pipeline: _targets.R reads this CSV (format="file"),
  # takes Tier-A included ids as its analytic sample, and backfills phd_year
  # from the pq_phd_year column.
  tar_target(inclusion_table_csv,
             write_inclusion_table(inclusion, people, pq_matched, nt_link,
                                   nt_grants,
                                   path = "output/tables/inclusion_table.csv"),
             format = "file"),
  tar_target(inclusion_review_csv,
             write_inclusion_review(inclusion, people,
                                    "output/tables/inclusion_review.csv"),
             format = "file"),
  # Reproducible audit of in-scope people ProQuest missed, with near-miss diag.
  tar_target(pq_unmatched_review_csv,
             write_unmatched_review(
               build_unmatched_review(pq_persons, pq_diss, pq_matched,
                                      inclusion, education_long),
               "output/tables/pq_unmatched_review.csv"),
             format = "file"),
  # All Tier A (LinkedIn-validated) people ProQuest did not match.
  tar_target(pq_tier_a_unmatched_csv,
             write_tier_a_unmatched(
               build_tier_a_unmatched(inclusion, pq_matched, people,
                                      broad_features),
               "output/tables/pq_tier_a_unmatched.csv"),
             format = "file"),
  # Does ProQuest deserve to be a gate? Match rate on the known-neuro cohort.
  tar_target(pq_match_rate,  proquest_match_rate(pq_matched, education_long)),

  # ---- neuro classification + feeder review ----
  tar_target(pq_neuro_classified, pq_classify_neuro(pq_raw)),
  tar_target(pq_feeder_review_csv,
             write_pq_feeder_review(pq_neuro_classified,
                                    "output/tables/pq_feeder_review.csv"),
             format = "file"),

  # Hand-review EVERY accepted fuzzy match (catch false positives), with both
  # the LinkedIn and ProQuest URLs and the dissertation's neuro classification.
  tar_target(pq_fuzzy_match_review_csv,
             write_fuzzy_match_review(
               build_fuzzy_match_review(pq_matched, pq_persons,
                                        pq_neuro_classified),
               "output/tables/pq_fuzzy_match_review.csv"),
             format = "file"),

  # Audit what the fuzzy gates (field corroboration + middle-name conflict)
  # dropped, so recall loss stays visible.
  tar_target(pq_fuzzy_rejected_tbl,
             pq_fuzzy_rejected(pq_persons, pq_diss, pq_exact)),
  tar_target(pq_fuzzy_rejected_review_csv,
             write_fuzzy_rejected_review(
               build_fuzzy_rejected_review(pq_fuzzy_rejected_tbl, pq_persons,
                                           pq_neuro_classified),
               "output/tables/pq_fuzzy_rejected_review.csv"),
             format = "file")
)
