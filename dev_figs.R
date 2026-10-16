# dev_figs.R — figure workbench. NOT a tar_target; never run by the pipeline.
#
# Loads the built data targets straight out of the `targets` store and sources
# the plot code, so you can iterate on a figure without a rebuild. The data is
# static; only R/05_plots.R and R/06_figure_settings.R change.
#
# Use:
#   Rscript -e 'source("dev_figs.R"); fig("first_sector")'   # one figure
#   Rscript -e 'source("dev_figs.R"); fig()'                 # all of them
#
# Or interactively (better — the store read happens once):
#   source("dev_figs.R")
#   fig("prestige")
#   reload(); fig("prestige")   # after editing a plot function
#
# Figures land in output/figures_dev/ by default so a half-finished draft can
# never be mistaken for a pipeline output. Pass to = "output/figures" when you
# want to overwrite the real ones; `targets` will then see them as changed and
# rebuild that one file target on the next tar_make(), which is correct.

library(targets)
suppressPackageStartupMessages({
  library(tidyverse); library(ggplot2); library(cowplot)
  library(showtext);  library(ragg);   library(zoo)
})

DEV_FIG_DIR <- "output/figures_dev"

# Source only the code, not the pipeline. Cheap — call it after every edit.
reload <- function() {
  purrr::walk(fs::dir_ls("R", glob = "*.R"), source, local = globalenv())
  invisible(TRUE)
}

# Pull the built targets once.
#
# `companies` joined 2026-07-30 for the neurotech figure: organization_type is
# one of its three gates and is not carried on sectored_exp.
load_fig_data <- function(store = targets::tar_config_get("store")) {
  targets::tar_load(c(features_full, sectored_exp, filter_funnel, companies),
                    store = store, envir = globalenv())
  invisible(TRUE)
}

# Subfield labels for the by-subfield figure. Read off DISK, not out of a store
# — `pq_subject_terms` already rides the inclusion_table.csv seam and the
# dissertation text is in the read-only TDM export, so no rebuild is involved.
# Cached in globalenv() because scoring the classifier over the Arm-V
# dissertations takes a few seconds and the figure is redrawn often.
load_subfield_labels <- function(refresh = FALSE) {
  if (!refresh && exists("subfield_labels", envir = globalenv())) {
    return(invisible(TRUE))
  }
  assign("subfield_labels", build_subfield_labels(), envir = globalenv())
  invisible(TRUE)
}

# name -> a function of (to) that writes one PNG and returns its path.
FIG_SPECS <- list(
  funnel          = function(to) plot_funnel(filter_funnel, file.path(to, "fig_filter_funnel.png")),
  cohort_size     = function(to) plot_cohort_size(features_full, file.path(to, "fig_cohort_size.png")),
  first_sector    = function(to) plot_sector_by_cohort(features_full, which = "first",
                                   file.path(to, "fig_first_sector_by_cohort.png")),
  curr_sector     = function(to) plot_sector_by_cohort(features_full, which = "current",
                                   file.path(to, "fig_current_sector_by_cohort.png")),
  first_subsector = function(to) plot_sector_by_cohort(features_full, which = "first",
                                   file.path(to, "fig_first_subsector_by_cohort.png"),
                                   level = "subsector"),
  curr_subsector  = function(to) plot_sector_by_cohort(features_full, which = "current",
                                   file.path(to, "fig_current_subsector_by_cohort.png"),
                                   level = "subsector"),
  # UCOT-Exp2 twins of the three sector-classification figures. Same people,
  # same cohorts/years, six-level UCOT sector instead of our four — read as a
  # second classification of the same data, not as an alternate colouring.
  first_sector_ucot = function(to) plot_sector_by_cohort(features_full, which = "first",
                                     file.path(to, "fig_first_sector_by_cohort_ucot.png"),
                                     level = "ucot"),
  curr_sector_ucot  = function(to) plot_sector_by_cohort(features_full, which = "current",
                                     file.path(to, "fig_current_sector_by_cohort_ucot.png"),
                                     level = "ucot"),
  first_sec_yr_ucot = function(to) plot_first_sector_counts_by_cohort(features_full,
                                     file.path(to, "fig_first_sector_counts_by_cohort_ucot.png"),
                                     taxonomy = "ucot"),
  time_km         = function(to) plot_time_to_industry(features_full, file.path(to, "fig_time_to_industry_km.png")),
  neurotech       = function(to) plot_neurotech_share(features_full, sectored_exp, companies,
                                   file.path(to, "fig_neurotech_share.png")),
  first_subfield  = function(to) {
                      load_subfield_labels()
                      plot_first_sector_by_subfield(features_full, subfield_labels,
                        file.path(to, "fig_first_sector_by_subfield.png"))
                    },
  top_emp         = function(to) plot_top_employers(features_full, sectored_exp,
                                   file.path(to, "fig_top_employers.png"), which = "current"),
  top_emp_all     = function(to) plot_top_employers(features_full, sectored_exp,
                                   file.path(to, "fig_top_employers_alltime.png"), which = "all_time"),
  top_emp_first   = function(to) plot_top_employers(features_full, sectored_exp,
                                   file.path(to, "fig_top_employers_first.png"), which = "first"),
  top_emp_first_industry = function(to) plot_top_employers(features_full, sectored_exp,
                                   file.path(to, "fig_top_employers_first_industry.png"),
                                   which = "first", sector_filter = "Industry"),
  pareto          = function(to) plot_employer_pareto(sectored_exp, features_full,
                                   file.path(to, "fig_employer_pareto.png")),
  first_sec_yr    = function(to) plot_first_sector_counts_by_cohort(features_full,
                                   file.path(to, "fig_first_sector_counts_by_cohort.png")),
  prestige        = function(to) plot_prestige_by_sector(features_full,
                                   file.path(to, "fig_prestige_by_sector.png")),
  postdoc_dur     = function(to) plot_postdoc_duration(features_full, sectored_exp,
                                   file.path(to, "fig_postdoc_duration.png")),
  grant_cohort    = function(to) plot_grant_by_cohort(features_full,
                                   file.path(to, "fig_grant_by_cohort.png")),
  titles_sector    = function(to) plot_titles_by_group(features_full, sectored_exp,
                                    file.path(to, "fig_titles_by_sector.png"), level = "sector"),
  titles_subsector = function(to) plot_titles_by_group(features_full, sectored_exp,
                                    file.path(to, "fig_titles_by_subsector.png"), level = "subsector"),
  ai_ml_cohort     = function(to) plot_ai_ml_share_by_cohort(features_full, sectored_exp,
                                    file.path(to, "fig_ai_ml_share_by_cohort.png")),
  postdoc_first    = function(to) plot_postdoc_first_job_share(features_full,
                                    file.path(to, "fig_postdoc_first_job_share.png"))
)

#' Build one figure (or all of them) from already-built targets.
#'
#' @param which figure name(s) from names(FIG_SPECS); NULL builds every one.
#' @param to    output directory. Defaults to output/figures_dev.
#' @param fresh re-source R/*.R first. TRUE is the point of this script.
fig <- function(which = NULL, to = DEV_FIG_DIR, fresh = TRUE) {
  if (fresh) reload()
  if (!exists("features_full", envir = globalenv())) load_fig_data()
  which <- which %||% names(FIG_SPECS)
  unknown <- setdiff(which, names(FIG_SPECS))
  if (length(unknown)) {
    stop("unknown figure(s): ", paste(unknown, collapse = ", "),
         "\navailable: ", paste(names(FIG_SPECS), collapse = ", "))
  }
  fs::dir_create(to)
  # Keep going on failure and report at the end. A batch redraw that dies on
  # figure 7 of 15 tells you less than one that draws the other 14 and names
  # the one that broke.
  res <- purrr::map(which, function(nm) {
    message("building ", nm)
    tryCatch(FIG_SPECS[[nm]](to),
             error = function(e) structure(conditionMessage(e), class = "fig_error"))
  })
  names(res) <- which
  failed <- purrr::keep(res, ~ inherits(.x, "fig_error"))
  ok     <- purrr::discard(res, ~ inherits(.x, "fig_error"))
  message("wrote ", length(ok), " of ", length(which), " figure(s) to ", to)
  if (length(failed)) {
    message("FAILED (", length(failed), "):")
    purrr::iwalk(failed, ~ message("  ", .y, ": ", .x))
  }
  invisible(res)
}

# Load the data at source time so an interactive session is ready to go.
if (interactive()) {
  reload()
  load_fig_data()
  message("dev_figs.R ready. fig(\"", names(FIG_SPECS)[1], "\") or fig() for all.\n",
          "figures: ", paste(names(FIG_SPECS), collapse = ", "))
}
