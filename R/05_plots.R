# R/05_plots.R — the figures.
#
# Theme, palette and cohort bins live in R/06_figure_settings.R, not here.
# Change them there once and every figure follows; no theme or break sequence
# is defined in this file.
#
# Retained conventions: right-side y-axis, zero baseline, 'YY year ticks,
# Source Serif 4 via showtext / ragg.
#
# Each plot function takes data + an output path, writes the PNG, and returns
# the path (so it slots into a `format = "file"` target cleanly).

# NOTE: there is deliberately no top-level `PAL <- FIG_PAL` alias — _targets.R
# sources R/*.R alphabetically, so this file is read BEFORE 06_figure_settings.R
# and FIG_PAL does not exist yet at source time. References inside function
# bodies resolve at call time and are fine, so the figures below use FIG_PAL
# directly.

econ_line <- function(df, x, y, color = NULL, title = NULL, subtitle = NULL,
                      caption = NULL, y_label = NULL) {
  p <- ggplot2::ggplot(df, ggplot2::aes({{ x }}, {{ y }}, color = {{ color }})) +
    ggplot2::geom_line(linewidth = 0.7) +
    ggplot2::geom_point(size = 1.2) +
    ggplot2::scale_y_continuous(position = "right", expand = c(0, 0)) +
    ggplot2::scale_color_manual(values = FIG_PAL) +
    ggplot2::labs(title = title, subtitle = subtitle, caption = caption, y = y_label, x = NULL) +
    fig_theme()
  p
}

.save <- function(p, path, w = 6.5, h = 4.2) {
  fs::dir_create(fs::path_dir(path))
  ragg::agg_png(path, width = w, height = h, units = "in", res = 200)
  print(p); dev.off()
  path
}

# Relative-luminance contrast pick, so a percentage label printed on top of a
# fill colour stays legible regardless of whether that fill is light (yellow,
# sky blue) or dark (blue, black) — a single hard-coded text colour is
# unreadable against half the Okabe-Ito palette.
.contrast_text_colour <- function(hex) {
  rgb <- grDevices::col2rgb(hex) / 255
  lum <- 0.2126 * rgb[1, ] + 0.7152 * rgb[2, ] + 0.0722 * rgb[3, ]
  unname(ifelse(lum > 0.55, "black", "white"))
}

# Shared helper for a stacked/grouped bar chart's percentage-per-segment
# labels: blank out anything under `min_share` so a long tail of thin slices
# doesn't turn into unreadable overlapping text, and pick a legible text
# colour per segment from its own fill colour.
.pct_labels <- function(share, fill_value, fill_map, min_share = 0.05) {
  label  <- dplyr::if_else(share >= min_share, scales::percent(share, accuracy = 1), "")
  colour <- .contrast_text_colour(unname(fill_map[as.character(fill_value)]))
  list(label = label, colour = colour)
}

# ---- the actual figures ----

plot_funnel <- function(filter_funnel, path) {
  d <- filter_funnel |> dplyr::distinct(stage, n) |>
    dplyr::mutate(stage = factor(stage, levels = c("start","has_neuro_phd",
      "phd_year_derivable","phd_year_in_range","has_dated_experience","final")))
  p <- ggplot2::ggplot(d, ggplot2::aes(stage, n)) +
    ggplot2::geom_col(fill = FIG_PAL[2]) +
    ggplot2::geom_text(ggplot2::aes(label = scales::comma(n)), vjust = -0.4, size = 3) +
    ggplot2::scale_y_continuous(position = "right", expand = ggplot2::expansion(c(0, 0.1))) +
    ggplot2::labs(title = "Sample funnel", y = NULL, x = NULL) +
    fig_theme()
  .save(p, path)
}

plot_cohort_size <- function(features_full, path) {
  n_total <- nrow(features_full)
  d0 <- features_full |> drop_na_levels(cohort)
  n_used <- nrow(d0)
  d <- d0 |>
    dplyr::count(cohort, name = "n") |>
    dplyr::mutate(cohort = factor(cohort, levels = cohort_levels()))
  p <- ggplot2::ggplot(d, ggplot2::aes(cohort, n)) +
    ggplot2::geom_col(fill = FIG_PAL[2]) +
    ggplot2::scale_x_discrete(drop = FALSE) +
    ggplot2::scale_y_continuous(position = "right", expand = c(0, 0)) +
    ggplot2::labs(title = "Neuroscience PhDs by cohort",
      subtitle = "Analytic sample",
      caption = n_caption(n_used, n_total),
      y = NULL, x = NULL) +
    fig_theme() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
  .save(p, path, h = 4.8)
}

# Sector shares by cohort. `level = "sector"` gives our four-level headline;
# `level = "subsector"` gives the drill-down; `level = "ucot"` gives the
# UCOT-Exp2 workforce-sector twin of the headline (R/22_ucot.R,
# join_ucot_sector_features()) — same cohort binning and NA handling, a
# different classification of the same people.
#
# NA is excluded from the denominator. It used to be counted
# as a level, so an average of 50.7% of every bar was "no employment data"
# rendered as though it were a career outcome. The excluded count goes in the
# caption instead of the bar. (UCOT's own "Unknown" level is a determinate
# answer, not a stand-in for missingness, and is NOT dropped here.)
plot_sector_by_cohort <- function(features_full, which = c("first", "current"),
                                  path, level = c("sector", "subsector", "ucot")) {
  which <- match.arg(which)
  level <- match.arg(level)
  col <- if (level == "ucot") paste0(which, "_ucot_sector") else paste0(which, "_", level)

  n_total <- nrow(features_full)
  d0 <- features_full |>
    drop_na_levels(cohort) |>
    (\(x) x[!is.na(x[[col]]), , drop = FALSE])()
  n_used <- nrow(d0)

  d <- d0 |>
    dplyr::count(cohort, .value = .data[[col]], name = "n") |>
    dplyr::group_by(cohort) |>
    dplyr::mutate(share = n / sum(n)) |>
    dplyr::ungroup() |>
    dplyr::mutate(cohort = factor(cohort, levels = cohort_levels()))

  fill_map <- if (level == "sector") {
    SECTOR_COLORS
  } else if (level == "ucot") {
    UCOT_SECTOR_COLORS
  } else {
    vals <- sort(unique(d$.value))
    stats::setNames(grDevices::colorRampPalette(FIG_PAL[-1])(length(vals)), vals)
  }
  fill_scale <- ggplot2::scale_fill_manual(values = fill_map, drop = FALSE)

  lbl <- .pct_labels(d$share, d$.value, fill_map)
  d$.label <- lbl$label
  d$.text_colour <- lbl$colour

  p <- ggplot2::ggplot(d, ggplot2::aes(cohort, share, fill = .value)) +
    ggplot2::geom_col() +
    ggplot2::geom_text(ggplot2::aes(label = .label, colour = .text_colour),
                       position = ggplot2::position_stack(vjust = 0.5),
                       size = 2.4, show.legend = FALSE) +
    ggplot2::scale_colour_identity() +
    fill_scale +
    ggplot2::scale_y_continuous(labels = scales::percent, position = "right",
                                expand = c(0, 0)) +
    ggplot2::scale_x_discrete(drop = FALSE) +
    ggplot2::labs(
      title = paste0(stringr::str_to_title(which), " post-PhD sector by cohort",
                     if (level == "ucot") " (UCOT-Exp2)" else ""),
      subtitle = if (level == "subsector") "Detailed subsector" else NULL,
      caption = n_caption(n_used, n_total),
      y = NULL, x = NULL, fill = NULL) +
    fig_theme() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
  .save(p, path, h = 4.8)
}

# Cumulative incidence of the first industry job, by cohort.
#
# THE RISK SET IS RESTRICTED TO PEOPLE WITH OBSERVABLE EMPLOYMENT HISTORY
# (`has_employment_history`, set in join_sector_features()). This is the single
# most consequential correction in the figure set.
#
# What it was doing before (measured 2026-07-29): 5,799 of 8,117 Arm-V people
# were censored, and 3,412 of those — 59% of the censored, 42% of the whole risk
# set — had NO dated experience row at all. Their LinkedIn payload simply
# carried no dates. The curve read every one of them as "still at risk, never
# left academia", which is informative missingness treated as an observation and
# biases cumulative incidence downward by an unquantified amount.
#
# Censoring people we HAVE data for at years_since_phd is correct and stays.
# Censoring people we have no data for is not censoring, it is invention.
plot_time_to_industry <- function(features_full, path) {
  n_total <- nrow(features_full)
  d <- features_full |>
    drop_na_levels(cohort) |>
    dplyr::filter(has_employment_history %in% TRUE) |>
    dplyr::mutate(
      t = dplyr::coalesce(time_to_first_industry_yrs, as.numeric(years_since_phd)),
      event = !is.na(time_to_first_industry_yrs)
    ) |>
    dplyr::filter(!is.na(t), t >= 0) |>
    dplyr::select(cohort, t, event)

  if (nrow(d) == 0) {
    p <- ggplot2::ggplot() + ggplot2::labs(title = "Time to first industry job — insufficient data") +
      fig_theme()
    return(.save(p, path))
  }

  # Manual Kaplan-Meier: at each ordered time the risk set is everyone not yet
  # removed, and only events step the estimator down.
  km <- d |>
    dplyr::group_by(cohort) |>
    dplyr::arrange(t, .by_group = TRUE) |>
    dplyr::mutate(
      at_risk = dplyr::n() - dplyr::row_number() + 1,
      surv    = cumprod(dplyr::if_else(event, 1 - 1 / at_risk, 1))
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(cohort = factor(cohort, levels = cohort_levels()))

  # Size the palette by the number of LEVELS, not the number present. The scale
  # below is `drop = FALSE` (so the legend is the same in every cohort figure),
  # which asks the palette for one colour per level — sizing it on
  # n_distinct() errored with "11 needed but only 2 provided" whenever the KM
  # filters left fewer cohorts than the binning defines.
  n_cohorts <- nlevels(km$cohort)
  pal <- grDevices::colorRampPalette(FIG_PAL[-1])(n_cohorts)
  p <- ggplot2::ggplot(km, ggplot2::aes(t, 1 - surv, colour = cohort)) +
    ggplot2::geom_step(linewidth = 0.7) +
    ggplot2::scale_y_continuous(labels = scales::percent, position = "right",
                                limits = c(0, 1), expand = c(0, 0)) +
    ggplot2::scale_colour_manual(values = pal, drop = FALSE) +
    ggplot2::labs(title = "Cumulative incidence of first industry job",
      subtitle = "Years since PhD, by cohort",
      caption = n_caption(nrow(d), n_total),
      y = NULL, x = "Years since PhD") +
    fig_theme()
  .save(p, path)
}

# plot_geo_flows() was removed 2026-07-29. It had never been implemented.
#
# Dropped rather than built: the planned figure was phd_country -> current_country,
# and both sides are now degenerate. ProQuest's corpus is ~96.9% US, so the PhD
# side is nearly one value, and current_country is 93.4% US on Arm V. A Sankey
# whose dominant ribbon is US->US carries almost no information. `current_country`
# is still derived in build_features() and is the useful residue: it supports an
# outflow rate or a stayer flag, which is the same question at the right altitude.

# Neurotech is a SUBSECTOR, not a sector — it collapses into Industry at the
# headline level.
#
# REBUILT 2026-07-30. The previous version read `current_subsector ==
# "Neurotech (specific)"` and drew a line by cohort. That line was flat at
# EXACTLY 0% and always had been, because no LinkedIn `industries` string
# contains "neuro" — see is_neurotech_company() in R/sector_map.R. The figure
# was reporting a broken rule as a finding.
#
# THE BY-COHORT LINE IS GONE ON PURPOSE. With the rule fixed, 33 Arm-V people
# are currently at one of the 88 neurotech employers. Spread over 11 cohort
# bins that is ~3 per cell, and a percentage built on 3 people moves 33
# percentage points when one person changes job. There is no trend to read at
# that N, so the figure reports the LEVEL and names the employers instead.
#
# Takes `companies` because organization_type is one of the three gates and is
# not carried on sectored_exp.
plot_neurotech_share <- function(features_full, sectored_exp, companies, path) {
  n_total <- nrow(features_full)

  nt_co <- companies |>
    dplyr::filter(is_neurotech_company(name, industries, organization_type)) |>
    dplyr::distinct(company_id, name)

  rows <- sectored_exp |>
    dplyr::filter(id %in% features_full$id) |>
    dplyr::filter(company_id %in% nt_co$company_id | company_name %in% nt_co$name)

  ever_n    <- dplyr::n_distinct(rows$id)
  # NUMERATOR AND DENOMINATOR MUST USE THE SAME CURRENT-JOB RULE. This read
  # `is_current %in% TRUE` until 2026-07-31, which was fine while the denominator
  # was also dated-only. It stopped being fine when the denominator gained the
  # public-card branch (METHODS.md, "Date-less profiles"): a date-less row can never
  # carry is_current, so 3,311 people would have entered the denominator with no
  # way of ever entering the numerator, deflating the share by ~40% on a
  # definition change alone. current_job_rows() is the single rule, cut to the
  # same bases the `current_subsector` denominator below is built from
  # (DATED_CURRENT_BASES) so the two sides can never disagree.
  current   <- current_job_rows(sectored_exp, ids = features_full$id) |>
    dplyr::filter(current_job_basis %in% DATED_CURRENT_BASES) |>
    dplyr::filter(company_id %in% nt_co$company_id |
                    company_name %in% nt_co$name)
  current_n <- dplyr::n_distinct(current$id)
  # Denominator: people with an observable current employer at all. Anyone with
  # no current job is not evidence about neurotech either way.
  denom <- features_full |> dplyr::filter(!is.na(current_subsector)) |> nrow()

  top <- current |>
    dplyr::count(company_name, sort = TRUE) |>
    dplyr::filter(!is.na(company_name)) |>
    head(12)
  if (nrow(top) == 0) {
    p <- ggplot2::ggplot() +
      ggplot2::labs(title = "Neurotech employment — no matching employers") + fig_theme()
    return(.save(p, path))
  }

  top <- top |> dplyr::mutate(company_name = forcats::fct_reorder(company_name, n))
  p <- ggplot2::ggplot(top, ggplot2::aes(n, company_name)) +
    ggplot2::geom_col(fill = FIG_PAL[1], width = 0.7) +
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(0, 0.08))) +
    ggplot2::labs(
      title = "Neurotech employers of neuroscience PhDs",
      subtitle = sprintf(
        "%s people (%.1f%% of those with a current employer) are at one of %s neurotech companies now; %s have ever been",
        format(current_n, big.mark = ","), 100 * current_n / max(denom, 1),
        format(nrow(nt_co), big.mark = ","), format(ever_n, big.mark = ",")),
      # Wrapped by hand: ggplot does not wrap a caption, and this one is long
      # enough to run off the canvas unbroken.
      caption = paste0(
        n_caption(denom, n_total), "\n",
        "Neurotech is matched on employer NAME + a commercial industry + a ",
        "non-nonprofit org type,\nnot on LinkedIn's `industries` field, which ",
        "never contains \"neuro\"."),
      x = "People currently employed", y = NULL) +
    fig_theme()
  .save(p, path)
}

# Write the neurotech employer list so the 88-company definition is auditable by
# hand rather than taken on trust. Lives here because the figure is the only
# consumer; see is_neurotech_company() for the rule.
write_neurotech_companies <- function(companies, path) {
  companies |>
    dplyr::filter(is_neurotech_company(name, industries, organization_type)) |>
    dplyr::select(company_id, name, industries, organization_type,
                  website_simplified, country_code) |>
    dplyr::arrange(name) |>
    readr::write_csv(path)
  path
}

# ---- AI/ML career share by cohort ------------------------------------------
#
# `is_ai_ml_title()` (R/sector_map.R) matches the JOB TITLE, not `industries`
# — see the note on the AI/ML row of subsector_lookup_table for why:
# `current_subsector == "AI/ML"` is 1 person and `first_subsector == "AI/ML"`
# is 0, because almost no employer's LinkedIn `industries` string says "AI".
# A figure built on that column would be the neurotech bug again (flat line
# read as "neuro PhDs don't go into AI/ML" when the rule just never fires).
#
# EVER, across every post-PhD experience row, not "current job". Measured
# 2026-07-31: 102 of 4,333 people with observable post-PhD employment have
# EVER held an AI/ML-titled role, concentrated almost entirely in 2010+
# cohorts (0-1 per cohort before 2010; 2.2-5.2% after) — a real generational
# shift, not noise, unlike neurotech's ~3-per-bin figure. "Ever" is used
# because current-job assignment needs the is_current -> latest_dated ->
# public_card_top_entry machinery (current_job_rows(), R/04_link.R), which is
# heavier than this question needs and "ever reached an AI/ML role" is the
# more robust read at this N regardless.
#
# READ THE TREND AS A FLOOR, NOT A CEILING. Older cohorts have had more career
# time to reach an AI/ML role than younger ones, so the rise into the 2020s
# cohorts is happening despite LESS follow-up time for those people — if
# anything this understates how much more the recent cohorts are moving into
# AI/ML.
plot_ai_ml_share_by_cohort <- function(features_full, sectored_exp, path,
                                       min_cell = 25L) {
  n_total <- nrow(features_full)
  post <- post_phd_experience_rows(sectored_exp, features_full)

  d0 <- post |>
    dplyr::mutate(ai_ml_title = is_ai_ml_title(title_raw)) |>
    dplyr::group_by(id) |>
    dplyr::summarise(ever_ai_ml = any(ai_ml_title), .groups = "drop") |>
    dplyr::inner_join(dplyr::select(features_full, id, cohort), by = "id") |>
    drop_na_levels(cohort)
  n_used <- nrow(d0)

  d <- d0 |>
    dplyr::count(cohort, ever_ai_ml, name = "n") |>
    tidyr::complete(cohort, ever_ai_ml, fill = list(n = 0)) |>
    dplyr::group_by(cohort) |>
    dplyr::mutate(total = sum(n), share = n / total) |>
    dplyr::ungroup() |>
    dplyr::filter(ever_ai_ml, total >= min_cell) |>
    dplyr::mutate(cohort = factor(cohort, levels = cohort_levels()))

  if (nrow(d) == 0) {
    p <- ggplot2::ggplot() +
      ggplot2::labs(title = "AI/ML careers by cohort — insufficient data") +
      fig_theme()
    return(.save(p, path))
  }

  p <- ggplot2::ggplot(d, ggplot2::aes(cohort, share, group = 1)) +
    ggplot2::geom_line(colour = FIG_PAL[2], linewidth = 0.8) +
    ggplot2::geom_point(colour = FIG_PAL[2], size = 1.6) +
    ggplot2::scale_y_continuous(labels = scales::percent, position = "right",
                                limits = c(0, NA), expand = c(0, 0.02)) +
    ggplot2::scale_x_discrete(drop = FALSE) +
    ggplot2::labs(
      title = "Neuroscience PhDs ever holding an AI/ML-titled role",
      subtitle = "Share by PhD cohort, among those with observable post-PhD employment",
      caption = paste0(
        n_caption(n_used, n_total),
        "\nMatched on job title (\"machine learning engineer\", \"AI researcher\", ...), ",
        "not LinkedIn industry.",
        "\nOlder cohorts have had more career time to reach an AI/ML role, so the rise ",
        "into recent cohorts is a floor, not a ceiling.",
        "\nCohorts with fewer than ", min_cell, " observable people are omitted."),
      y = NULL, x = NULL) +
    fig_theme() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
  .save(p, path, h = 4.8)
}

# Every matched title string, so the free-text-noise tradeoff in
# is_ai_ml_title() is auditable by hand rather than trusted. Same contract as
# write_neurotech_companies().
write_ai_ml_titles <- function(sectored_exp, features_full, path) {
  post_phd_experience_rows(sectored_exp, features_full) |>
    dplyr::filter(is_ai_ml_title(title_raw)) |>
    dplyr::count(title_raw, name = "n_rows") |>
    dplyr::arrange(dplyr::desc(n_rows)) |>
    readr::write_csv(path)
  path
}

# ---- Share whose first post-PhD position is a postdoc, by cohort ----------
#
# `first_subsector == "Academia — postdoc"` already exists on features_full —
# the "First post-PhD position" rule (METHODS.md) explicitly counts a postdoc as
# the first position, and classify_academic_title() tags it from the title
# text ("postdoc", "post-doctoral"). No new classification needed, just a
# by-cohort read of an existing column — unlike AI/ML above, which needed one.
plot_postdoc_first_job_share <- function(features_full, path) {
  n_total <- nrow(features_full)
  d0 <- features_full |>
    drop_na_levels(cohort) |>
    dplyr::filter(!is.na(first_subsector))
  n_used <- nrow(d0)

  d <- d0 |>
    dplyr::mutate(is_postdoc = first_subsector == "Academia — postdoc") |>
    dplyr::count(cohort, is_postdoc, name = "n") |>
    dplyr::group_by(cohort) |>
    dplyr::mutate(share = n / sum(n)) |>
    dplyr::ungroup() |>
    dplyr::filter(is_postdoc) |>
    dplyr::mutate(cohort = factor(cohort, levels = cohort_levels()))

  p <- ggplot2::ggplot(d, ggplot2::aes(cohort, share, group = 1)) +
    ggplot2::geom_line(colour = FIG_PAL[2], linewidth = 0.8) +
    ggplot2::geom_point(colour = FIG_PAL[2], size = 1.6) +
    ggplot2::scale_y_continuous(labels = scales::percent, position = "right",
                                limits = c(0, NA), expand = c(0, 0.02)) +
    ggplot2::scale_x_discrete(drop = FALSE) +
    ggplot2::labs(
      title = "Share whose first post-PhD position is a postdoc",
      subtitle = "By PhD cohort",
      caption = n_caption(n_used, n_total),
      y = NULL, x = NULL) +
    fig_theme() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
  .save(p, path, h = 4.8)
}

# First sector out of the PhD, by dissertation subfield.
#
# `subfield_labels` comes from build_subfield_labels() (R/08_classify.R) and is
# read off disk, not out of a store — the subject terms ride the
# inclusion_table.csv seam already. That is what keeps this a dev_figs.R figure
# rather than a ProQuest rebuild.
#
# PLOTTED ON THE DETERMINISTIC LABEL (`subfield_terms`), never on a model
# label. `subfield_labels` also carries `content_area`/`methodology` (LLM
# classification, R/08_classify.R) alongside `subfield_terms`, but those are a
# different, orthogonal taxonomy now (not a 13-way subfield guess), so they
# are not comparable to this figure's categories and are not plotted here.
#
# "General / unspecified" IS PLOTTED as its own bar. 83.9% of Arm V carries the
# subject term "Neurosciences" and nothing more specific, so people whose terms
# are all generic are a large and honest category — hiding them would make the
# remaining shares look like they describe the whole sample.
plot_first_sector_by_subfield <- function(features_full, subfield_labels, path,
                                          min_n = 30) {
  n_total <- nrow(features_full)
  d0 <- features_full |>
    dplyr::select(id, first_sector) |>
    dplyr::inner_join(subfield_labels |> dplyr::select(id, subfield_terms),
                      by = "id") |>
    drop_na_levels(first_sector, subfield_terms)

  # A subfield with a handful of people produces a 100%-of-3 bar that reads as
  # a finding. Pooled rather than dropped, so the denominator is preserved.
  keep <- d0 |> dplyr::count(subfield_terms) |>
    dplyr::filter(n >= min_n) |> dplyr::pull(subfield_terms)
  d0 <- d0 |>
    dplyr::mutate(subfield_terms = dplyr::if_else(
      subfield_terms %in% keep, subfield_terms,
      sprintf("Other subfields (< %d each)", min_n)))

  if (nrow(d0) == 0) {
    p <- ggplot2::ggplot() +
      ggplot2::labs(title = "First sector by subfield — insufficient data") + fig_theme()
    return(.save(p, path))
  }

  counts <- d0 |> dplyr::count(subfield_terms, name = "n_subfield")
  d <- d0 |>
    dplyr::count(subfield_terms, first_sector) |>
    dplyr::group_by(subfield_terms) |>
    dplyr::mutate(share = n / sum(n)) |>
    dplyr::ungroup() |>
    dplyr::left_join(counts, by = "subfield_terms") |>
    # Build the axis label ONCE, then order that same column. Deriving the
    # factor levels from a second sprintf()/format() call is how this went wrong
    # the first time: format() pads to a common width, so the levels did not
    # match the labels and the ordering silently half-applied.
    dplyr::mutate(
      subfield_lab = sprintf("%s  (n = %s)", subfield_terms,
                             format(n_subfield, big.mark = ",", trim = TRUE)),
      .is_bucket = grepl("^Other subfields|^General", subfield_terms),
      .ind = dplyr::if_else(first_sector == "Industry", share, 0))

  # Level 1 renders at the BOTTOM of a discrete y-axis, so descending industry
  # share puts the most industry-bound subfield at the top, and the pooled /
  # unspecified buckets sort to the bottom where they can't be mistaken for a
  # ranked subfield.
  ord <- d |>
    dplyr::group_by(subfield_lab) |>
    dplyr::summarise(.is_bucket = dplyr::first(.is_bucket),
                     .ind = max(.ind), .groups = "drop") |>
    dplyr::arrange(dplyr::desc(.is_bucket), .ind) |>
    dplyr::pull(subfield_lab)

  d <- d |> dplyr::mutate(
    subfield_lab = factor(subfield_lab, levels = ord),
    first_sector = factor(first_sector, levels = names(SECTOR_COLORS)))

  lbl <- .pct_labels(d$share, d$first_sector, SECTOR_COLORS)
  d$.label <- lbl$label
  d$.text_colour <- lbl$colour

  p <- ggplot2::ggplot(d, ggplot2::aes(share, subfield_lab, fill = first_sector)) +
    ggplot2::geom_col(width = 0.72) +
    ggplot2::geom_text(ggplot2::aes(label = .label, colour = .text_colour),
                       position = ggplot2::position_stack(vjust = 0.5),
                       size = 2.3, show.legend = FALSE) +
    ggplot2::scale_colour_identity() +
    ggplot2::scale_x_continuous(labels = scales::percent, expand = c(0, 0)) +
    ggplot2::scale_fill_manual(values = SECTOR_COLORS, drop = FALSE, name = NULL) +
    ggplot2::labs(
      title = "First sector out of the PhD, by dissertation subfield",
      subtitle = "Subfield from ProQuest subject terms; ordered by industry share",
      caption = paste0(n_caption(nrow(d0), n_total),
                       " Subfield is a deterministic map from ProQuest subject terms — ",
                       "see SUBFIELD_TERM_MAP."),
      x = NULL, y = NULL) +
    fig_theme() +
    ggplot2::theme(legend.position = "top")
  .save(p, path)
}

# NIH funding attainment by PhD cohort (columns from R/24_grants.R, reaching
# features_full through the inclusion_table.csv seam).
#
# THE DENOMINATOR IS NOT ARM V. It is Arm V members with `nt_grant_known` —
# i.e. those who matched a NeuroTree pid, so peopleGrant could be consulted at
# all: 3,478 of 8,117 (42.8%), measured 2026-07-29. The other 4,639 are UNKNOWN,
# not unfunded, and including them would halve every share for no reason but
# missing linkage. The subtitle states this; do not drop it.
#
# TWO SERIES on purpose. Alone, the R01 line looks like a collapse (20.6% of the
# 2000 cohort, 9.1% of 2012, 0% from 2022) and it is not one — an R01 typically
# lands 10+ years post-PhD and the record ends 2025, so every cohort is biased
# down by its remaining observation window. The F/K training line is the control:
# those awards land 0-5 years post-PhD, so they are barely censored, and they
# stay ~19-22% flat across exactly the cohorts where the R01 line falls.
#
# **The censoring rule marks where the R01 measure becomes hopeless, NOT where it
# becomes clean.** The 2000-2012 stretch is still censored, just less; the
# decline across it is not evidence that R01s got harder to win. The figure
# supports comparing the two LEVELS within a cohort, not reading either trend
# across cohorts, and the caption says so in those words. If anyone ever wants
# the trend claim, it needs a hazard model on years-since-PhD, not this figure.
#
# T32 is excluded from the training series (the PI of a T32 is the program
# director, not the trainee) — see NT_GRANT_T32_CODES.
#
# Pre-2000 cohorts are dropped: cells are 1-35 people and badly survivorship-
# selected, because an old NeuroTree pid we could match is disproportionately a
# well-known PI. From 2000 every 2-year cell is >= 131.
NT_GRANT_FIG_FIRST_COHORT <- 2000L
NT_GRANT_FIG_MIN_CELL     <- 30L
# Years post-PhD before an R01 is realistically observable. Used only to place
# the censoring boundary on this figure.
NT_GRANT_R01_LAG_YRS      <- 13L

plot_grant_by_cohort <- function(features_full, path,
                                 first_cohort = NT_GRANT_FIG_FIRST_COHORT,
                                 min_cell = NT_GRANT_FIG_MIN_CELL) {
  need <- c("nt_grant_known", "nt_ever_r01", "nt_ever_training_award")
  if (!all(need %in% names(features_full)) ||
      sum(features_full$nt_grant_known %in% TRUE) == 0) {
    # Seam still on the stub: emit a placeholder rather than failing the run,
    # same contract as plot_top_employers() when it has nothing to draw.
    p <- ggplot2::ggplot() +
      ggplot2::labs(title = "NIH funding by cohort",
                    subtitle = "No funding data on the inclusion seam yet — run the NeuroTree store.") +
      fig_theme()
    return(.save(p, path))
  }

  known <- features_full |>
    dplyr::filter(nt_grant_known %in% TRUE, !is.na(phd_year))
  n_known <- nrow(known)
  n_total <- nrow(features_full)

  # Censoring boundary from the data, not a hardcoded year, so the figure keeps
  # itself honest when the export refreshes.
  last_funded <- suppressWarnings(max(known$nt_last_grant_year, na.rm = TRUE))
  if (!is.finite(last_funded)) last_funded <- as.integer(format(Sys.Date(), "%Y"))
  cutoff <- last_funded - NT_GRANT_R01_LAG_YRS

  d <- known |>
    dplyr::mutate(coh = floor(phd_year / 2) * 2) |>
    dplyr::filter(coh >= first_cohort, coh <= last_funded) |>
    dplyr::group_by(coh) |>
    dplyr::summarise(n = dplyr::n(),
                     `Ever held an R01` = mean(nt_ever_r01, na.rm = TRUE),
                     `Ever held an F/K training award` =
                       mean(nt_ever_training_award, na.rm = TRUE),
                     .groups = "drop") |>
    dplyr::filter(n >= min_cell) |>
    tidyr::pivot_longer(-c(coh, n), names_to = "series", values_to = "share") |>
    dplyr::mutate(series = factor(series, levels = c("Ever held an R01",
                                                     "Ever held an F/K training award")))

  # Solid where the cohort has had a full window; dashed+faded beyond. The
  # boundary row belongs to BOTH layers so the line has no gap at the join.
  obs <- d |> dplyr::filter(coh <= cutoff)
  cen <- d |> dplyr::filter(coh >= cutoff)

  p <- ggplot2::ggplot(mapping = ggplot2::aes(coh, share, color = series)) +
    ggplot2::geom_vline(xintercept = cutoff, linetype = "dotted",
                        color = "grey55", linewidth = 0.4) +
    ggplot2::geom_line(data = cen, linetype = "22", linewidth = 0.7, alpha = 0.45) +
    ggplot2::geom_point(data = cen, size = 1.2, alpha = 0.45) +
    ggplot2::geom_line(data = obs, linewidth = 0.8) +
    ggplot2::geom_point(data = obs, size = 1.4) +
    ggplot2::annotate("text", x = cutoff + 0.6, y = Inf,
                      label = paste0("cohorts with <", NT_GRANT_R01_LAG_YRS,
                                     " yrs observed →"),
                      hjust = 0, vjust = 1.6, size = 2.7, colour = "grey45",
                      # Resolved, not hard-coded: Source Serif 4 is not installed
                      # everywhere and a literal family here silently falls back
                      # to sans with a warning per grob.
                      family = .register_fig_font()) +
    ggplot2::scale_x_continuous(
      breaks = function(lim) seq(floor(lim[1] / 2) * 2, ceiling(lim[2]), 2),
      labels = function(x) paste0("'", substr(x, 3, 4))) +
    ggplot2::scale_y_continuous(labels = scales::percent, position = "right",
                                limits = c(0, NA), expand = c(0, 0.02)) +
    # FIG_PAL[1] is the reserved vermillion (neurotech marker only), so the two
    # series start at 2.
    ggplot2::scale_color_manual(values = c(
      "Ever held an R01" = FIG_PAL[2],
      "Ever held an F/K training award" = FIG_PAL[6])) +
    ggplot2::labs(
      title = "NIH funding attainment by PhD cohort",
      subtitle = paste0(
        "Share ever named PI on an NIH award, among the ", scales::comma(n_known),
        " of ", scales::comma(n_total),
        " Arm V members with a NeuroTree match.\nThe rest are unknown, not unfunded."),
      caption = paste0(
        "READ LEVELS WITHIN A COHORT, NOT THE TREND ACROSS THEM. An R01 lands 10+ yrs post-PhD ",
        "and the record ends ", last_funded, ", so the R01\nline is biased down by ",
        "observation window at EVERY point, not only past the rule — the '",
        substr(first_cohort, 3, 4), "-'", substr(cutoff, 3, 4),
        " decline is not evidence that R01s got harder to win.\nThe F/K line (awards that land 0-5 yrs ",
        "post-PhD, so barely censored) is the control. Linkage is an automated name match, score ≥ ",
        NT_GRANT_SCORE_MIN, ".\nSource: NeuroTree / academictree.org (CC-BY 3.0), ",
        "peopleGrant ← NIH ExPORTER. doi:10.1038/s41467-018-07034-y"),
      y = NULL, x = NULL, color = NULL) +
    fig_theme() +
    ggplot2::theme(legend.position = "top",
                   legend.justification = "left",
                   plot.caption = ggplot2::element_text(size = 7))
  .save(p, path, h = 4.8)
}

# ---- job titles by sector / subsector ----
#
# Two figures answering "what does a career in this bucket look like, in
# people's own words": for each sector (fig_titles_by_sector.png) or subsector
# (fig_titles_by_subsector.png), a handful of representative `title_raw`
# strings. Every DATED experience row of an Arm-V person counts, not just the
# current or first job — a title someone held briefly
# mid-career still shows up.
#
# Selection rule: within a
# group, rank distinct titles by how many qualifying experience rows carry
# that (case-folded) wording, split into three roughly equal frequency
# terciles, and draw 2/3 of the picks from the TOP tercile ("common") and 1/3
# from the MIDDLE tercile ("semi-rare but representative") — sectors get 6
# titles (4 common + 2 semi-rare), subsectors get 3 (2 common + 1 semi-rare).
# The BOTTOM tercile (the true long tail) is never drawn from: an earlier
# 1/3-1/3-1/3 version pulled from it and mostly surfaced parsing noise —
# employer names and headline fragments sitting in `title_raw` at n=2, the
# TITLE_MIN_N floor — because a floor of 2 does not stop noise from
# dominating the bottom third of a long-tailed distribution. Dropping that
# tercile removes most of it.
#
# title_norm (lower-cased) is what's grouped and ranked; the DISPLAYED string
# is the most common title_raw casing/spelling for that title_norm, so
# "Postdoctoral Fellow" prints even if a few rows spell it lowercase.
TITLE_MIN_N       <- 2L   # a title must recur at least this many times to be eligible
TITLE_TRUNC_CHARS <- 40L  # long titles are truncated so the list stays on one line

.pick_group_titles <- function(sectored_exp, features_full, group_col,
                               n_per_group, min_n = TITLE_MIN_N) {
  d <- sectored_exp |>
    dplyr::filter(id %in% features_full$id, !is.na(title_norm), title_norm != "",
                  !is.na(.data[[group_col]]))

  # Most common title_raw spelling for each (group, title_norm) pair.
  raw_pick <- d |>
    dplyr::count(.data[[group_col]], title_norm, title_raw, name = ".rawn") |>
    dplyr::arrange(dplyr::desc(.rawn)) |>
    dplyr::distinct(.data[[group_col]], title_norm, .keep_all = TRUE) |>
    dplyr::select(-.rawn)

  n_common <- max(1L, ceiling(n_per_group * 2 / 3))
  n_semi   <- max(1L, n_per_group - n_common)

  ranked <- d |>
    dplyr::count(.data[[group_col]], title_norm, name = "n") |>
    dplyr::filter(n >= min_n) |>
    dplyr::group_by(.data[[group_col]]) |>
    dplyr::mutate(tier = dplyr::ntile(dplyr::desc(n), 3)) |>
    dplyr::ungroup() |>
    dplyr::mutate(tier = factor(tier, levels = 1:3,
                                labels = c("common", "semi_rare", "long_tail")))

  common <- ranked |>
    dplyr::filter(tier == "common") |>
    dplyr::group_by(.data[[group_col]]) |>
    dplyr::slice_max(n, n = n_common, with_ties = FALSE) |>
    dplyr::ungroup()
  semi_rare <- ranked |>
    dplyr::filter(tier == "semi_rare") |>
    dplyr::group_by(.data[[group_col]]) |>
    dplyr::slice_max(n, n = n_semi, with_ties = FALSE) |>
    dplyr::ungroup()

  dplyr::bind_rows(common, semi_rare) |>
    dplyr::left_join(raw_pick, by = c(group_col, "title_norm")) |>
    dplyr::mutate(title_label = stringr::str_trim(title_raw))
}

plot_titles_by_group <- function(features_full, sectored_exp, path,
                                 level = c("sector", "subsector")) {
  level <- match.arg(level)
  n_per_group <- if (level == "sector") 6L else 3L
  ncol        <- if (level == "sector") 2L else 4L
  emoji_map   <- if (level == "sector") SECTOR_EMOJI else SUBSECTOR_EMOJI
  n_common    <- max(1L, ceiling(n_per_group * 2 / 3))
  n_semi      <- max(1L, n_per_group - n_common)

  n_total <- nrow(features_full)
  n_used  <- sectored_exp |>
    dplyr::filter(id %in% features_full$id, !is.na(.data[[level]]),
                  !is.na(title_norm), title_norm != "") |>
    dplyr::pull(id) |> dplyr::n_distinct()

  picked <- .pick_group_titles(sectored_exp, features_full, level, n_per_group)
  if (nrow(picked) == 0) {
    p <- ggplot2::ggplot() +
      ggplot2::labs(title = sprintf("Job titles by %s — insufficient data", level)) +
      fig_theme()
    return(.save(p, path))
  }

  d <- picked |>
    dplyr::mutate(tier = factor(tier, levels = c("common", "semi_rare"))) |>
    dplyr::arrange(.data[[level]], tier, dplyr::desc(n)) |>
    dplyr::group_by(.data[[level]]) |>
    dplyr::mutate(row_i = dplyr::row_number(), n_rows = dplyr::n()) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      y   = n_rows - row_i + 1,
      lab = sprintf("%s  (n = %s)",
                    stringr::str_trunc(title_label, TITLE_TRUNC_CHARS),
                    scales::comma(n)))

  # icon_fam is NA when the icon font isn't installed — fall back to the
  # figure's own text font and a plain bullet rather than crashing.
  icon_fam <- .register_icon_font()
  hdr <- d |>
    dplyr::distinct(.data[[level]], n_rows) |>
    dplyr::mutate(
      y     = n_rows + 1,
      label = .data[[level]],
      icon  = dplyr::coalesce(unname(emoji_map[.data[[level]]]), ICON_FALLBACK_GLYPH))

  p <- ggplot2::ggplot() +
    ggplot2::geom_text(data = hdr, ggplot2::aes(x = -0.05, y = y, label = icon),
                       hjust = 1, vjust = 0.5, size = 5.5,
                       family = icon_fam %||% .register_fig_font()) +
    ggplot2::geom_text(data = hdr, ggplot2::aes(x = 0.05, y = y, label = label),
                       hjust = 0, vjust = 0.5, size = 3.6, fontface = "bold") +
    ggplot2::geom_text(data = d, ggplot2::aes(x = 0.05, y = y, label = lab),
                       hjust = 0, vjust = 0.5, size = 2.85) +
    ggplot2::facet_wrap(ggplot2::vars(.data[[level]]), ncol = ncol, scales = "free_y") +
    ggplot2::scale_x_continuous(limits = c(-0.6, 3), expand = c(0, 0)) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0.08, 0.15))) +
    ggplot2::labs(
      title = sprintf("Job titles held, by %s", level),
      subtitle = sprintf(
        paste0("%d per group: %d most common and %d semi-rare but recurring title(s) ",
               "(a title must recur ≥ %d times to qualify)."),
        n_per_group, n_common, n_semi, TITLE_MIN_N),
      caption = paste0(
        n_caption(n_used, n_total),
        "\nCounts are experience ROWS carrying that title, not people — the same person can ",
        "appear under two titles.\nEvery dated job counts, not only the current or first one; ",
        "spelling shown is the most common raw variant."),
      x = NULL, y = NULL) +
    fig_theme() +
    ggplot2::theme(
      axis.line = ggplot2::element_blank(), axis.text = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      strip.background = ggplot2::element_blank(), strip.text = ggplot2::element_blank(),
      panel.spacing = grid::unit(1.1, "lines"))

  if (level == "sector") {
    .save(p, path, w = 9, h = 6.5)
  } else {
    .save(p, path, w = 13, h = 11)
  }
}

plot_top_employers <- function(features_full, sectored_exp, path,
                               which = c("current", "all_time", "first"),
                               sector_filter = NULL) {
  which <- match.arg(which)
  if (which == "first") {
    # first_post_phd_job() (R/04_link.R) is the same "first post-PhD position"
    # row join_sector_features() uses for first_sector, so this figure and the
    # first_sector figures always name the same job for the same person.
    d <- first_post_phd_job(sectored_exp, features_full) |>
      dplyr::filter(id %in% features_full$id, !is.na(company_name))
    if (!is.null(sector_filter)) {
      d <- d |> dplyr::filter(sector == sector_filter)
    }
    d <- d |> dplyr::count(company_name, sector, sort = TRUE)
  } else {
    d <- sectored_exp |>
      dplyr::filter(id %in% features_full$id, !is.na(company_name))
    if (which == "current") {
      d <- d |>
        dplyr::filter(is_current) |>
        dplyr::count(company_name, sector, sort = TRUE)
    } else {
      d <- d |>
        dplyr::arrange(dplyr::desc(start_date)) |>
        dplyr::distinct(id, company_name, .keep_all = TRUE) |>
        dplyr::count(company_name, sector, sort = TRUE)
    }
  }
  d <- d |> dplyr::slice_head(n = 20)
  if (nrow(d) == 0) {
    lbl <- switch(which,
      current  = "No current employers identified",
      all_time = "No employers identified",
      first    = if (!is.null(sector_filter))
                   paste("No first", sector_filter, "employers identified")
                 else "No first employers identified")
    p <- ggplot2::ggplot() + ggplot2::labs(title = lbl) + fig_theme()
    return(.save(p, path))
  }
  title <- switch(which,
    current  = "Top 20 current employers",
    all_time = "Top 20 employers, all time",
    first    = if (!is.null(sector_filter))
                 paste0("Top 20 first employers — ", sector_filter, " sector")
               else "Top 20 first employers")
  # SECTOR_COLORS, not the ggplot default hue scale. This figure was the one
  # place a default palette survived the 2026-07-30 Okabe-Ito sweep, so the same
  # sector was salmon here and orange in every other figure.
  p <- ggplot2::ggplot(d, ggplot2::aes(n, reorder(company_name, n), fill = sector)) +
    ggplot2::geom_col() +
    ggplot2::scale_fill_manual(values = SECTOR_COLORS, drop = FALSE) +
    ggplot2::scale_x_continuous(position = "top", expand = c(0,0)) +
    ggplot2::labs(title = title, y = NULL, x = NULL, fill = NULL) +
    fig_theme() +
    ggplot2::theme(panel.grid.major.y = ggplot2::element_blank(),
                   panel.grid.major.x = ggplot2::element_line(color = "grey85"))
  .save(p, path, h = 5)
}

plot_employer_pareto <- function(sectored_exp, features_full, path,
                                 color_by = c("none", "grad_year", "start_year"),
                                 period = c("5yr", "decade")) {
  color_by <- match.arg(color_by)
  period <- match.arg(period)
  bin_width <- if (period == "5yr") 5L else 10L

  d <- sectored_exp |>
    dplyr::filter(id %in% features_full$id, !is.na(company_name))

  if (color_by == "grad_year") {
    d <- d |>
      dplyr::left_join(
        features_full |> dplyr::select(id, phd_year),
        by = "id"
      ) |>
      dplyr::filter(!is.na(phd_year)) |>
      dplyr::mutate(
        period_start = (phd_year %/% bin_width) * bin_width,
        period_label = paste0(period_start, "–", period_start + bin_width - 1L)
      )
  } else if (color_by == "start_year") {
    d <- d |>
      dplyr::filter(!is.na(start_year)) |>
      dplyr::mutate(
        period_start = (start_year %/% bin_width) * bin_width,
        period_label = paste0(period_start, "–", period_start + bin_width - 1L)
      )
  }

  if (color_by == "none") {
    counts <- d |>
      dplyr::distinct(id, company_name) |>
      dplyr::count(company_name, name = "n_phds")
    ccdf <- counts |>
      dplyr::count(n_phds, name = "n_at_level") |>
      dplyr::arrange(dplyr::desc(n_phds)) |>
      dplyr::mutate(n_employers_gte = cumsum(n_at_level))
    p <- ggplot2::ggplot(ccdf, ggplot2::aes(n_phds, n_employers_gte)) +
      ggplot2::geom_point(color = FIG_PAL[2], size = 1.5)
  } else {
    counts <- d |>
      dplyr::distinct(id, company_name, period_label) |>
      dplyr::count(company_name, period_label, name = "n_phds")
    ccdf <- counts |>
      dplyr::group_by(period_label) |>
      dplyr::count(n_phds, name = "n_at_level") |>
      dplyr::arrange(dplyr::desc(n_phds)) |>
      dplyr::mutate(n_employers_gte = cumsum(n_at_level)) |>
      dplyr::ungroup()
    p <- ggplot2::ggplot(ccdf, ggplot2::aes(n_phds, n_employers_gte,
                                             color = period_label)) +
      ggplot2::geom_point(size = 1.5) +
      ggplot2::scale_color_manual(values = FIG_PAL)
  }

  p <- p +
    ggplot2::scale_x_log10() +
    ggplot2::scale_y_log10(position = "right") +
    # "br", not "bl" — the y-axis is on the right (house convention), so
    # left-side log ticks render as a column of stray dashes against no axis.
    ggplot2::annotation_logticks(sides = "br", linewidth = 0.2) +
    ggplot2::labs(
      title = "PhD employment concentration",
      subtitle = "Complementary cumulative distribution",
      x = "Number of PhDs employed",
      y = "Employers with ≥ x PhDs",
      color = NULL
    ) +
    fig_theme()

  .save(p, path)
}

# collapse_to_mega() REMOVED 2026-07-29. The four-level collapse is now
# collapse_to_sector() in R/sector_map.R and is applied in classify_sectors(),
# so `first_sector` / `current_sector` ARE the mega level and no figure has to
# re-derive it. Keeping a second copy of the mapping here is exactly how the two
# drift apart.

# `taxonomy = "ucot"` gives the UCOT-Exp2 twin (`first_ucot_sector`, from
# join_ucot_sector_features()) — same year binning, same figure, a different
# classification of the same people. Our four levels are not the same set as
# UCOT's six, so this is not a re-colouring of one figure but a genuinely
# separate classification of the same underlying data.
plot_first_sector_counts_by_cohort <- function(features_full, path,
                                      taxonomy = c("ours", "ucot")) {
  taxonomy <- match.arg(taxonomy)
  col    <- if (taxonomy == "ucot") "first_ucot_sector" else "first_sector"
  levs   <- if (taxonomy == "ucot") UCOT_SECTOR_LEVELS else c("Industry", "Academia", "Government", "Other")
  fill_map <- if (taxonomy == "ucot") UCOT_SECTOR_COLORS else SECTOR_COLORS

  n_total <- nrow(features_full)
  d0 <- features_full |>
    drop_na_levels(cohort) |>
    (\(x) x[!is.na(x[[col]]), , drop = FALSE])()
  n_used <- nrow(d0)

  d <- d0 |>
    dplyr::count(cohort, .value = .data[[col]], name = "n") |>
    dplyr::mutate(.value = factor(.value, levels = levs),
                  cohort = factor(cohort, levels = cohort_levels())) |>
    dplyr::group_by(cohort) |>
    dplyr::mutate(share = n / sum(n)) |>
    dplyr::ungroup()

  lbl <- .pct_labels(d$share, d$.value, fill_map)
  d$.label <- lbl$label
  d$.text_colour <- lbl$colour

  p <- ggplot2::ggplot(d, ggplot2::aes(cohort, n, fill = .value)) +
    ggplot2::geom_col() +
    ggplot2::geom_text(ggplot2::aes(label = .label, colour = .text_colour),
                       position = ggplot2::position_stack(vjust = 0.5),
                       size = 2.4, show.legend = FALSE) +
    ggplot2::scale_colour_identity() +
    ggplot2::scale_x_discrete(drop = FALSE) +
    ggplot2::scale_y_continuous(position = "right", expand = c(0, 0)) +
    ggplot2::scale_fill_manual(values = fill_map, drop = FALSE) +
    ggplot2::labs(
      title = paste0("First post-PhD employment sector by cohort",
                     if (taxonomy == "ucot") " (UCOT-Exp2)" else ""),
      caption = n_caption(n_used, n_total),
      y = NULL, x = NULL, fill = NULL
    ) +
    fig_theme() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
  .save(p, path, h = 4.8)
}

# ---- new analytical figures (2026-07-29) ----

# PhD-program prestige vs first destination.
#
# The rankings covariate (R/22_rankings.R) was built and then consumed by
# nothing. This is the figure it exists for: does training at a higher-ranked
# neuroscience program change where people land?
#
# Uses research.com's world rank, which reaches 89.2% of Arm V (US News reaches
# 74.7% and adds ~1pp). ABSENCE IS NOT A LOW RANK — unranked institutions are
# excluded, not imputed to the bottom, per the note in R/22_rankings.R.
plot_prestige_by_sector <- function(features_full, path,
                                    n_bins = 4L, which = "first") {
  col <- paste0(which, "_sector")
  n_total <- nrow(features_full)
  d0 <- features_full |>
    dplyr::filter(!is.na(researchcom_world_rank_2026), !is.na(.data[[col]]))
  if (nrow(d0) == 0) {
    p <- ggplot2::ggplot() +
      ggplot2::labs(title = "Prestige vs sector — no ranked institutions matched") +
      fig_theme()
    return(.save(p, path))
  }

  d <- d0 |>
    dplyr::mutate(
      rank_band = dplyr::ntile(researchcom_world_rank_2026, n_bins),
      rank_band = factor(rank_band, levels = seq_len(n_bins),
                         labels = paste0("Q", seq_len(n_bins),
                                         c(" (top)", rep("", n_bins - 2), " (bottom)")))
    ) |>
    dplyr::count(rank_band, .value = .data[[col]], name = "n") |>
    dplyr::group_by(rank_band) |>
    dplyr::mutate(share = n / sum(n)) |>
    dplyr::ungroup()

  lbl <- .pct_labels(d$share, d$.value, SECTOR_COLORS)
  d$.label <- lbl$label
  d$.text_colour <- lbl$colour

  p <- ggplot2::ggplot(d, ggplot2::aes(rank_band, share, fill = .value)) +
    ggplot2::geom_col() +
    ggplot2::geom_text(ggplot2::aes(label = .label, colour = .text_colour),
                       position = ggplot2::position_stack(vjust = 0.5),
                       size = 2.4, show.legend = FALSE) +
    ggplot2::scale_colour_identity() +
    ggplot2::scale_fill_manual(values = SECTOR_COLORS, drop = FALSE) +
    ggplot2::scale_y_continuous(labels = scales::percent, position = "right",
                                expand = c(0, 0)) +
    ggplot2::labs(
      title = "First destination by PhD-program rank",
      subtitle = "research.com world rank in neuroscience, quartiles",
      caption = paste(n_caption(nrow(d0), n_total),
                      "Unranked institutions excluded, not imputed."),
      y = NULL, x = NULL, fill = NULL) +
    fig_theme()
  .save(p, path)
}

# Postdoc duration by cohort — "is the neuroscience postdoc getting longer?"
#
# THIS IS A SURVIVAL PROBLEM AND MUST BE ESTIMATED AS ONE. Postdocs still in
# progress are RIGHT-CENSORED, not excluded.
#
# The naive version — median of completed postdocs per cohort — was written
# first and produced a beautiful, completely spurious finding: median duration
# falling from 4.7 years (1995-96) to 1.0 years (2023-24), i.e. "the postdoc has
# collapsed". It is an artifact. A 2023 PhD can only have COMPLETED a postdoc
# if that postdoc was short; the long ones are still running and were dropped by
# the very filter meant to protect against bias. Conditioning on completion
# selects short stints, and does so harder the more recent the cohort.
#
# Kaplan-Meier handles it correctly: an in-progress postdoc contributes exposure
# up to today and is censored. Where a cohort's KM median is not reached — most
# recent cohorts, where fewer than half have finished — NOTHING IS PLOTTED,
# because the honest answer is "we cannot know yet". That refusal is the point
# of the figure.
#
# Unit is the PERSON, not the stint: first postdoc start to last postdoc end,
# so consecutive postdocs read as one training period.
plot_postdoc_duration <- function(features_full, sectored_exp, path) {
  n_total <- nrow(features_full)
  today <- Sys.Date()

  # POST-PhD postdocs only. Without this the clock can start before the degree:
  # a row labelled postdoc that predates phd_year (a mislabelled research post,
  # or a ProQuest degree year later than the LinkedIn dates) produced a 6.9-year
  # median for the 2023-24 cohort — people who have had barely two years since
  # their PhD. Measuring from a start that precedes the PhD is not measuring a
  # postdoc.
  pd <- sectored_exp |>
    dplyr::filter(id %in% features_full$id,
                  subsector == "Academia — postdoc",
                  !is.na(start_date)) |>
    dplyr::inner_join(features_full |> dplyr::select(id, phd_year), by = "id") |>
    dplyr::filter(!is.na(phd_year), lubridate::year(start_date) >= phd_year) |>
    dplyr::group_by(id) |>
    dplyr::summarise(
      start   = min(start_date),
      ongoing = any(is_current),
      end     = suppressWarnings(max(end_date, na.rm = TRUE)),
      .groups = "drop") |>
    dplyr::mutate(
      end     = dplyr::if_else(ongoing | is.infinite(as.numeric(end)), today, end),
      dur_yrs = as.numeric(end - start) / 365.25,
      event   = !ongoing) |>
    # Over 15 years is a parse artifact (a stale open-ended row), not a postdoc.
    dplyr::filter(dur_yrs > 0, dur_yrs <= 15) |>
    dplyr::inner_join(features_full |> dplyr::select(id, cohort), by = "id") |>
    drop_na_levels(cohort)

  if (nrow(pd) < 20 || !requireNamespace("survival", quietly = TRUE)) {
    p <- ggplot2::ggplot() +
      ggplot2::labs(title = "Postdoc duration — insufficient data") + fig_theme()
    return(.save(p, path))
  }

  # KM median per cohort, with the lower confidence bound. survfit returns NA
  # for the median when the curve never crosses 0.5 — exactly the cohorts that
  # must not be plotted, so the NA is propagated rather than filled.
  fit <- survival::survfit(
    survival::Surv(dur_yrs, event) ~ cohort, data = pd)
  tbl <- summary(fit)$table
  d <- tibble::as_tibble(tbl, rownames = "grp") |>
    dplyr::mutate(cohort = sub("^cohort=", "", grp)) |>
    dplyr::select(cohort, n = records, events, median = median,
                  lcl = `0.95LCL`, ucl = `0.95UCL`) |>
    # Three guards, all doing different work:
    #   n >= 20      — a cohort too small to estimate anything from.
    #   !is.na(median) — survfit could not reach 0.5 at all.
    #   events/n >= 0.5 — the median is INTERPOLATED WITHIN observed follow-up
    #     rather than extrapolated past it. Without this last one the 2023-24
    #     cohort reported a median off 36% observed exits, which is a guess
    #     dressed as an estimate. This is the guard that makes the figure honest
    #     about recent cohorts.
    dplyr::filter(n >= 20, !is.na(median), events / n >= 0.5) |>
    dplyr::mutate(cohort = factor(cohort, levels = cohort_levels()))

  if (nrow(d) < 2) {
    p <- ggplot2::ggplot() +
      ggplot2::labs(title = "Postdoc duration — median not reached in any cohort") +
      fig_theme()
    return(.save(p, path))
  }

  n_censored <- sum(!pd$event)
  p <- ggplot2::ggplot(d, ggplot2::aes(cohort, median, group = 1)) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = lcl, ymax = ucl),
                         fill = FIG_PAL[3], alpha = 0.35) +
    ggplot2::geom_line(colour = FIG_PAL[2], linewidth = 0.8) +
    ggplot2::geom_point(colour = FIG_PAL[2], size = 1.5) +
    ggplot2::scale_y_continuous(position = "right", expand = c(0, 0.1)) +
    ggplot2::scale_x_discrete(drop = FALSE) +
    ggplot2::labs(
      title = "How long is the neuroscience postdoc?",
      subtitle = "Kaplan-Meier median years in postdoc, with 95% CI",
      caption = paste0(
        n_caption(nrow(pd), n_total, "people with a postdoc"),
        sprintf(" %d still in post (censored). Cohorts whose median is not yet reached are omitted.",
                n_censored)),
      y = NULL, x = NULL) +
    fig_theme() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
  .save(p, path)
}

# ---- tables ----

write_sample_description <- function(features_full, path) {
  has <- function(col) col %in% names(features_full)
  cnt <- function(col) if (has(col)) sum(!is.na(features_full[[col]])) else NA_integer_

  out <- features_full |>
    dplyr::summarise(
      n                   = dplyr::n(),
      n_with_cohort       = sum(!is.na(cohort)),
      n_with_first_sector = sum(!is.na(first_sector)),
      n_with_current_sector = sum(!is.na(current_sector))
    ) |>
    dplyr::mutate(
      n_with_current_country      = cnt("current_country"),
      n_current_country_us        = if (has("current_country"))
                                      sum(features_full$current_country %in% "US")
                                    else NA_integer_,
      n_with_researchcom_rank     = cnt("researchcom_world_rank_2026"),
      n_with_usnews_rank          = cnt("usnews_global_rank_2020"),
      # The cell any prestige-vs-outcome figure actually runs on: both a rank
      # and a first_sector. Quote THIS, not n.
      n_rank_and_first_sector     = if (has("researchcom_world_rank_2026"))
                                      sum(!is.na(features_full$researchcom_world_rank_2026) &
                                          !is.na(features_full$first_sector))
                                    else NA_integer_
    )
  readr::write_csv(out, path)
  path
}

write_unmapped_industries <- function(sectored_exp, path) {
  out <- sectored_exp |>
    dplyr::filter(sector == "Other", !is.na(industries)) |>
    dplyr::count(industries, sort = TRUE)
  readr::write_csv(out, path)
  path
}
