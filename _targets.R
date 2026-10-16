# _targets.R — neuro PhD career outcomes pipeline
#
# Run with: Rscript -e 'targets::tar_make()'
# Inspect:  Rscript -e 'targets::tar_outdated()'
# Visualize: Rscript -e 'targets::tar_visnetwork()'

library(targets)
library(tarchetypes)

# memory = "transient" + garbage_collection: on a 16 GB workstation the full
# build runs deep into the page file. The
# default "persistent" memory keeps every built target resident, and the core
# store crashed the callr subprocess outright — no R error, an OS-level kill —
# once `people` joined `features`' dependencies alongside education_long and
# experience_long. Transient memory unloads each target after its dependents
# have consumed it. Costs some re-reading from the store; buys a pipeline that
# completes. Do not revert without re-checking free RAM.
tar_option_set(
  packages = c(
    "tidyverse", "jsonlite", "lubridate", "fs",
    "ggplot2", "cowplot", "showtext", "ragg", "zoo"
  ),
  format = "rds",
  error = "abridge",
  memory = "transient",
  garbage_collection = TRUE
)

# Source helper functions
purrr::walk(fs::dir_ls("R", glob = "*.R"), source)

list(
  # ---- inputs ----
  tar_target(people_csv,    "data/LinkedIn people profiles.csv",      format = "file"),
  tar_target(company_csv,   "data/LinkedIn company information.csv",  format = "file"),

  # ---- 0. QC on raw data ----
  # Scoped to what the acquisition step DELIVERED -- the assembled `people` /
  # `companies` / `experience_long`, not the analytic sample. It used to read
  # people_csv / company_csv directly, which pinned it to the delivered 1,000-row
  # company sample and made it report a stale 0.000 join rate.
  tar_target(qc_raw,        run_qc_raw(people, companies, experience_long)),
  tar_target(qc_report,     { readr::write_csv(qc_raw, "output/tables/qc_report.csv"); "output/tables/qc_report.csv" }, format = "file"),

  # ---- 1. Parse ----
  # Identity-seeded supplement — see _targets_neurotree.R for why. Unioned here
  # too so the final analytic sample (features_full) can include Tier-A people
  # the inclusion seam adds via nt_pq_neuro. Two cohorts, same pattern: PhD>=2010
  # and an all-years top-up covering 1995-2010, pre-1995, and no-year entries.
  # The acquisition scripts that produce these files are not part of this
  # repository; see DATA_SOURCES.md.
  tar_target(serp_people_csv,  "output/tables/serp_full_pull_collected.csv", format = "file"),
  tar_target(serp_allyears_people_csv, "output/tables/serp_allyears_pull_collected.csv", format = "file"),
  # Third SERP cohort: seeded from ProQuest instead of NeuroTree (unmatched
  # core-neuro dissertation authors, PhD>=2010). Every
  # person here already carries a matched neuro dissertation, so they reach
  # Tier A through the existing pq_matched path with no inclusion-rule change.
  tar_target(pq_serp_people_csv, "output/tables/pq_serp_pull_collected.csv", format = "file"),
  # Recovery of people the two NeuroTree routes verified but never collected —
  # their `url includes` fetch over-matched and their batch checkpointing
  # shuffled. Same cohorts as serp_people_csv /
  # serp_allyears_people_csv, so no new inclusion logic; distinct(id) below
  # absorbs any overlap.
  tar_target(serp_recovery_path, ensure_serp_collected_stub("output/tables/serp_recovery_collected.csv")),
  tar_target(serp_recovery_people_csv, serp_recovery_path, format = "file"),
  # Identity filter on the SERP cohorts. `confidence == "medium"` accepts matched
  # the NAME and nothing else; a hand audit of 130 of them measured a 7.7% FP
  # rate, all same-name namesakes. This drops the ones whose fetched education
  # years contradict the seed PhD year (the rejects list is produced offline by
  # the acquisition step); `high` accepts and the original content-filter pull
  # are never touched.
  tar_target(serp_rejects_path, ensure_serp_rejects_stub()),
  tar_target(serp_rejects_csv,  serp_rejects_path, format = "file"),
  tar_target(people,           dplyr::bind_rows(parse_people(people_csv),
                                                parse_people(serp_people_csv),
                                                parse_people(serp_allyears_people_csv),
                                                parse_people(pq_serp_people_csv),
                                                parse_people(serp_recovery_people_csv)) |>
                                dplyr::distinct(id, .keep_all = TRUE) |>
                                dplyr::anti_join(
                                  readr::read_csv(serp_rejects_csv, col_select = "id",
                                                  col_types = readr::cols(.default = "c")),
                                  by = "id")),
  tar_target(education_long,   parse_education(people)),
  tar_target(experience_long,  parse_experience(people)),
  # The delivered company_csv is a 1,000-row SAMPLE, and none of it overlapped
  # the companies our people actually work at — measured join rate on
  # current_company_company_id was 0.0%. The company pull fetches the real
  # ones by exact id (10,935 of 11,334 Tier-A-referenced companies, 96.5%),
  # taking the join rate to 99.4%. Sector assignment depends on this join, so
  # without it `industries` was effectively missing for the whole cohort.
  tar_target(companies_pull_path, ensure_serp_collected_stub(
    "output/tables/companies_pull_collected.csv",
    schema_from = "data/LinkedIn company information.csv")),
  tar_target(companies_pull_csv, companies_pull_path, format = "file"),
  tar_target(companies,        dplyr::bind_rows(
                                 read_csv(company_csv, show_col_types = FALSE),
                                 read_csv(companies_pull_csv, show_col_types = FALSE)) |>
                                dplyr::distinct(id, .keep_all = TRUE)),

  # ---- 2. Analytic sample ----
  # The sample is the tiered inclusion (Tier A) computed in the ProQuest store
  # and consumed here through the inclusion_table.csv seam (see R/12_inclusion.R).
  # The education-only funnel is retained as a QC/comparison artifact and as the
  # fallback sample while the seam file is still the empty stub.
  tar_target(filter_funnel,    build_filter_funnel(people, education_long, experience_long)),
  tar_target(funnel_csv,       { readr::write_csv(filter_funnel, "output/tables/filter_funnel.csv"); "output/tables/filter_funnel.csv" }, format = "file"),
  tar_target(inclusion_path,   ensure_inclusion_stub("output/tables/inclusion_table.csv")),
  tar_target(inclusion_file,   inclusion_path, format = "file"),
  # Tier A (~14k). RETAINED as the funnel endpoint and the comparison denominator
  # for Arm V — nothing downstream builds on it any more.
  tar_target(analytic_ids,     analytic_ids_from_inclusion(inclusion_file, filter_funnel, people)),
  # ARM V is the analytic sample: Tier A AND ProQuest-matched (see
  # R/12_inclusion.R for the definition and what it changed). Every figure
  # and table below is built on this, NOT on analytic_ids. Before 2026-07-29 they
  # were built on Tier A, so any figure predating that is on the wrong sample.
  tar_target(armv_ids,         armv_ids_from_inclusion(inclusion_file, people)),

  # ---- 3. Person-level features ----
  # phd_year backfill: people LinkedIn can't date get their matched ProQuest
  # dissertation's degree_year via the inclusion seam (no-op on the stub).
  # Slim two-column projection of `people`. build_features() only needs
  # id + country_code, and depending on the full profile table (1.2 GB raw, with
  # the JSON list columns) is what pushed the 2026-07-29 rebuild into an
  # out-of-memory kill. Keep this narrow — widening it re-creates the crash.
  tar_target(people_country,
             dplyr::distinct(dplyr::select(people, id, dplyr::any_of("country_code")), id,
                             .keep_all = TRUE)),
  # phd_year policy: ProQuest degree_year OVERRIDES the
  # LinkedIn-derived year (it is the gold standard, and every Arm-V member has
  # one by construction), NeuroTree fills the residual, then the sample is
  # capped at PHD_YEAR_MAX = 2025 to exclude in-progress degrees. See
  # R/12_inclusion.R::apply_phd_year_policy and R/06_figure_settings.R.
  #
  # NIH funding history (R/24_grants.R) rides the same seam and is attached on
  # top rather than in a parallel features_* target. THREE-STATE — see
  # NT_GRANT_SEAM_COLS in R/12_inclusion.R: NA means "no NeuroTree pid, funding
  # unknown", which is most of the sample, and must never be read as unfunded.
  tar_target(features,
             attach_grant_features(
               apply_phd_year_policy(
                 build_features(armv_ids, education_long, experience_long, people_country),
                 inclusion_file),
               inclusion_file)),

  # ---- 4. Link to companies & sector classification ----
  tar_target(linked_exp,       link_companies(experience_long, companies)),
  tar_target(sectored_exp,     classify_sectors(linked_exp)),
  # UCOT-Exp2 twin of the row-level classification (R/22_ucot.R), wired
  # 2026-07-30. Adds ucot_sector without touching sector/subsector, so
  # sectored_exp and everything downstream of it is unchanged.
  tar_target(sectored_exp_ucot, attach_ucot(sectored_exp)),

  # ---- 4b. PhD-program rankings (prestige covariate) ----
  # Both sources are GLOBAL RANK ONLY. Tracked as file
  # targets so replacing either CSV rehashes and rebuilds the join.
  # research.com reaches 89.2% of Arm V, US News 74.7%, either 90.3%.
  # See R/22_rankings.R for the matcher and the collision policy.
  tar_target(usnews_rankings_csv,      USNEWS_RANKINGS_PATH,      format = "file"),
  tar_target(researchcom_rankings_csv, RESEARCHCOM_RANKINGS_PATH, format = "file"),
  tar_target(features_full,
             join_ucot_sector_features(
               attach_rankings(join_sector_features(features, sectored_exp),
                               usnews_path      = usnews_rankings_csv,
                               researchcom_path = researchcom_rankings_csv),
               sectored_exp_ucot)),
  tar_target(rankings_review_csv,
             { force(features_full); RANKINGS_REVIEW_PATH }, format = "file"),

  # ---- 5. Figures ----
  tar_target(fig_funnel,        plot_funnel(filter_funnel,
                                            "output/figures/fig_filter_funnel.png"), format = "file"),
  tar_target(fig_cohort_size,   plot_cohort_size(features_full,
                                            "output/figures/fig_cohort_size.png"), format = "file"),
  # Headline (4-level sector) plus a subsector drill-down for each.
  tar_target(fig_first_sector,  plot_sector_by_cohort(features_full, which = "first",
                                            "output/figures/fig_first_sector_by_cohort.png"), format = "file"),
  tar_target(fig_curr_sector,   plot_sector_by_cohort(features_full, which = "current",
                                            "output/figures/fig_current_sector_by_cohort.png"), format = "file"),
  tar_target(fig_first_subsector, plot_sector_by_cohort(features_full, which = "first",
                                            "output/figures/fig_first_subsector_by_cohort.png",
                                            level = "subsector"), format = "file"),
  tar_target(fig_curr_subsector, plot_sector_by_cohort(features_full, which = "current",
                                            "output/figures/fig_current_subsector_by_cohort.png",
                                            level = "subsector"), format = "file"),
  tar_target(fig_time_km,       plot_time_to_industry(features_full,
                                            "output/figures/fig_time_to_industry_km.png"), format = "file"),
  tar_target(fig_neurotech,     plot_neurotech_share(features_full, sectored_exp, companies,
                                            "output/figures/fig_neurotech_share.png"), format = "file"),
  tar_target(neurotech_companies_csv,
                                write_neurotech_companies(companies,
                                            "output/tables/neurotech_companies.csv"), format = "file"),
  # Subfield labels are read off DISK (inclusion_table.csv seam + the TDM
  # export + the pq_llm_classify_cache.csv written by hand via
  # refresh_llm_classify_cache(), see _targets_classify.R), not out of a
  # store, so this figure can be redrawn with dev_figs.R. The target exists to
  # keep the pipeline the reproducible record.
  # The plotted `subfield_terms` label is unaffected by the LLM
  # content_area/methodology columns riding alongside it.
  tar_target(subfield_labels,   build_subfield_labels()),
  tar_target(fig_first_subfield, plot_first_sector_by_subfield(features_full, subfield_labels,
                                            "output/figures/fig_first_sector_by_subfield.png"),
                                format = "file"),
  tar_target(fig_top_emp,       plot_top_employers(features_full, sectored_exp,
                                            "output/figures/fig_top_employers.png",
                                            which = "current"), format = "file"),
  tar_target(fig_top_emp_all,  plot_top_employers(features_full, sectored_exp,
                                            "output/figures/fig_top_employers_alltime.png",
                                            which = "all_time"), format = "file"),
  tar_target(fig_top_emp_first, plot_top_employers(features_full, sectored_exp,
                                            "output/figures/fig_top_employers_first.png",
                                            which = "first"), format = "file"),
  tar_target(fig_top_emp_first_industry, plot_top_employers(features_full, sectored_exp,
                                            "output/figures/fig_top_employers_first_industry.png",
                                            which = "first", sector_filter = "Industry"),
                                format = "file"),
  tar_target(fig_pareto,       plot_employer_pareto(sectored_exp, features_full,
                                            "output/figures/fig_employer_pareto.png"), format = "file"),
  tar_target(fig_first_sec_yr, plot_first_sector_counts_by_cohort(features_full,
                                            "output/figures/fig_first_sector_counts_by_cohort.png"), format = "file"),
  # UCOT-Exp2 twins of the three sector-classification figures above. Same
  # people, same cohorts, a different taxonomy — see METHODS.md, "Sector
  # classification", for why the two are not interchangeable.
  tar_target(fig_first_sector_ucot, plot_sector_by_cohort(features_full, which = "first",
                                            "output/figures/fig_first_sector_by_cohort_ucot.png",
                                            level = "ucot"), format = "file"),
  tar_target(fig_curr_sector_ucot,  plot_sector_by_cohort(features_full, which = "current",
                                            "output/figures/fig_current_sector_by_cohort_ucot.png",
                                            level = "ucot"), format = "file"),
  tar_target(fig_first_sec_yr_ucot, plot_first_sector_counts_by_cohort(features_full,
                                            "output/figures/fig_first_sector_counts_by_cohort_ucot.png",
                                            taxonomy = "ucot"), format = "file"),
  # New 2026-07-29. fig_prestige consumes the rankings covariate built in
  # R/22_rankings.R, which until now fed no figure. fig_postdoc_dur measures
  # COMPLETED postdocs only — see the length-bias note in R/05_plots.R.
  tar_target(fig_prestige,     plot_prestige_by_sector(features_full,
                                            "output/figures/fig_prestige_by_sector.png"), format = "file"),
  tar_target(fig_postdoc_dur,  plot_postdoc_duration(features_full, sectored_exp,
                                            "output/figures/fig_postdoc_duration.png"), format = "file"),
  # Added 2026-07-31. AI/ML is matched on job TITLE (is_ai_ml_title(),
  # R/sector_map.R), not on `industries` — the industries-based `AI/ML`
  # subsector barely ever fires (1 person in current_subsector, 0 in
  # first_subsector), the same failure mode as
  # the pre-fix neurotech rule. The postdoc figure needs no new
  # classification — first_subsector already carries it.
  tar_target(fig_ai_ml_cohort, plot_ai_ml_share_by_cohort(features_full, sectored_exp,
                                            "output/figures/fig_ai_ml_share_by_cohort.png"),
                                format = "file"),
  tar_target(ai_ml_titles_csv, write_ai_ml_titles(sectored_exp, features_full,
                                            "output/tables/ai_ml_titles_matched.csv"), format = "file"),
  tar_target(fig_postdoc_first, plot_postdoc_first_job_share(features_full,
                                            "output/figures/fig_postdoc_first_job_share.png"),
                                format = "file"),
  # Consumes the NIH funding columns (R/24_grants.R). Denominator is the
  # NeuroTree-matched subset of Arm V, NOT Arm V — see the note on the function.
  # Draws a placeholder rather than failing while the seam is still the stub.
  tar_target(fig_grant_cohort, plot_grant_by_cohort(features_full,
                                            "output/figures/fig_grant_by_cohort.png"), format = "file"),
  # Representative job titles per sector / subsector. Every dated experience
  # row of an Arm-V person counts, not just current/first job. Selection rule
  # and its rationale are in R/05_plots.R (plot_titles_by_group).
  tar_target(fig_titles_sector,    plot_titles_by_group(features_full, sectored_exp,
                                            "output/figures/fig_titles_by_sector.png",
                                            level = "sector"), format = "file"),
  tar_target(fig_titles_subsector, plot_titles_by_group(features_full, sectored_exp,
                                            "output/figures/fig_titles_by_subsector.png",
                                            level = "subsector"), format = "file"),

  # ---- 6. Tables ----
  tar_target(tab_desc,          write_sample_description(features_full,
                                            "output/tables/tab_sample_description.csv"), format = "file"),
  tar_target(tab_unmapped,      write_unmapped_industries(sectored_exp,
                                            "output/tables/tab_unmapped_industries.csv"), format = "file"),
  # Every row where the company_raw name signal overruled a low-confidence
  # company_id join (METHODS.md, "Employer-name overrides").
  tar_target(tab_join_overrides, write_company_join_overrides(sectored_exp,
                                            "output/tables/company_join_override_audit.csv"), format = "file"),

  # ---- 7. Final check ----
  tar_target(done_check, {
    force(fig_funnel); force(fig_cohort_size); force(fig_first_sector)
    force(fig_curr_sector); force(fig_first_subsector); force(fig_curr_subsector)
    force(fig_time_km)
    force(fig_neurotech); force(fig_top_emp); force(fig_top_emp_all)
    force(fig_pareto); force(fig_first_sec_yr)
    force(fig_first_sector_ucot); force(fig_curr_sector_ucot); force(fig_first_sec_yr_ucot)
    force(fig_prestige); force(fig_postdoc_dur); force(fig_grant_cohort)
    force(fig_first_subfield)
    force(fig_ai_ml_cohort); force(ai_ml_titles_csv); force(fig_postdoc_first)
    force(tab_desc); force(tab_unmapped); force(tab_join_overrides)
    force(rankings_review_csv)
    force(neurotech_companies_csv)
    verify_done(
      figures_dir = "output/figures",
      tables_dir  = "output/tables",
      qc          = qc_raw
    )
  })
)
