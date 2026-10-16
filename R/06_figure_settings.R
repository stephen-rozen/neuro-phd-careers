# R/06_figure_settings.R — GLOBAL figure + analysis-window settings.
#
# This file is the ONE place to change how every figure looks and how every
# cohort is binned. Edit the constants here and re-run the pipeline; do not
# hard-code a theme, a palette or a break sequence inside a plot function.
#
# Three groups of settings live here:
#   1. the analysis window   (PHD_YEAR_MIN / PHD_YEAR_MAX)
#   2. the cohort binning    (COHORT_MIN / COHORT_MAX / COHORT_WIDTH)
#   3. the visual theme      (fig_theme(), FIG_PAL)
#
# Groups 1 and 2 live together deliberately. When the analysis window and the
# figure window were allowed to drift apart, figures silently plotted cohorts
# the analysis had already excluded.

# ---- 1. analysis window ----------------------------------------------------

# Upper bound on phd_year for ANALYSIS. 2026 is the current
# year, and a "2026 PhD" is overwhelmingly an in-progress degree rather than a
# completed one: 162 of the 244 Arm-V people previously dated to 2026 had a
# still-CURRENT PhD-student experience row, which derive_phd_year() strategy 2
# read as an end year of today. Capping at the last complete year removes them
# by construction rather than by heuristic.
PHD_YEAR_MAX <- 2025L

# Lower bound. Not a data limit — Arm V reaches back to 1963 — but the point
# below which cohorts are too thin to plot (single digits per 2-year bin).
# 1976, not 1975: 1976-2025 is exactly 50 years, which
# divides evenly by COHORT_WIDTH (5), so 2025 lands in a full 5-year bin
# (2021-2025) instead of sitting alone — see the break-edge note below.
PHD_YEAR_MIN <- 1976L

# ---- 2. cohort binning -----------------------------------------------------

COHORT_MIN   <- PHD_YEAR_MIN
COHORT_MAX   <- PHD_YEAR_MAX
COHORT_WIDTH <- 5L

# Break edges. Runs one full width PAST COHORT_MAX so the final year always has
# a bin of its own to fall in.
#
# The window 1976-2025 is exactly 50 years, which divides evenly by
# COHORT_WIDTH (5), so every bin including the last (2021-2025) is a full
# 5-year bin — no singleton. Getting the edge math wrong is easy and silent:
# an earlier version ran the edges to COHORT_MAX + 1, which put 2025 inside a
# bin labelled with years still to come — the newest cohort being the one
# mislabelled. `pmin(..., COHORT_MAX)` below guards against that regardless of
# whether the window divides evenly.
.cohort_breaks <- function() {
  seq(COHORT_MIN, COHORT_MAX + COHORT_WIDTH, by = COHORT_WIDTH)
}

.cohort_labels <- function() {
  brks  <- .cohort_breaks()
  lo    <- brks[-length(brks)]
  hi    <- pmin(brks[-1] - 1L, COHORT_MAX)
  ifelse(lo == hi, as.character(lo), paste0(lo, "–", hi))
}

# Fixed-break cohort bins. FIXED is the point: the previous implementation took
# breaks from the observed min/max of whatever it was handed, so the bins moved
# whenever the sample moved and no two runs were comparable. Years outside
# [COHORT_MIN, COHORT_MAX] return NA rather than being squeezed into an end bin,
# so an out-of-window person is visibly excluded instead of quietly reassigned.
cohort_bin <- function(year) {
  year <- suppressWarnings(as.integer(year))
  out <- cut(year, breaks = .cohort_breaks(), labels = .cohort_labels(),
             right = FALSE, include.lowest = TRUE)
  out <- as.character(out)
  out[is.na(year) | year < COHORT_MIN | year > COHORT_MAX] <- NA_character_
  out
}

# The bin labels in order — for factor levels, so an empty cohort still holds
# its slot on an axis instead of collapsing the scale.
cohort_levels <- function() .cohort_labels()

# ---- 3. theme --------------------------------------------------------------

# Palette: Okabe & Ito (2008), the standard colour-blind-safe 8-hue set, used
# for every colour in every figure. Position 1 is the
# reserved vermillion used ONLY for neurotech / the bio-augmented marker;
# general series start at position 2.
FIG_PAL <- c(
  "#D55E00",  # vermillion — reserved neurotech marker only
  "#0072B2",  # blue
  "#56B4E9",  # sky blue
  "#009E73",  # bluish green
  "#E69F00",  # orange
  "#CC79A7",  # reddish purple
  "#F0E442",  # yellow
  "#000000"   # black
)

# Stable colours for the four-level mega sector, so the same sector is the same
# colour in every figure regardless of which levels happen to be present.
SECTOR_COLORS <- c(
  "Industry"   = "#0072B2",  # blue
  "Academia"   = "#E69F00",  # orange
  "Government" = "#009E73",  # bluish green
  "Other"      = "#999999"   # grey (Okabe-Ito neutral)
)

# Stable colours for the UCOT-Exp2 workforce-sector axis (R/22_ucot.R). Kept
# apart from SECTOR_COLORS because the level sets differ — UCOT has Nonprofit
# and Unknown, ours does not — and reusing one colour for two different levels
# across the two taxonomies would make a legend lie.
UCOT_SECTOR_COLORS <- c(
  "Academia"   = "#E69F00",  # orange — matches SECTOR_COLORS' Academia
  "Government" = "#009E73",  # bluish green — matches SECTOR_COLORS' Government
  "For-Profit" = "#0072B2",  # blue — matches SECTOR_COLORS' Industry
  "Nonprofit"  = "#CC79A7",  # reddish purple — no equivalent in our scheme
  "Other"      = "#999999",  # grey — matches SECTOR_COLORS' Other
  "Unknown"    = "#F0E442"   # yellow — no equivalent in our scheme (NA is a
                              # real UCOT answer, not dropped like our NA)
)

# ---- font resolution ----
#
# Figures use Source Serif 4 via showtext. Until 2026-07-29 nothing in the
# project registered it: there was no font_add() call anywhere, so
# every figure ever produced silently fell back to `sans` while emitting a
# "font family not found" warning per text grob — 50+ warnings a figure, which
# is also why real warnings were easy to miss in a render log.
#
# Resolution order, all offline: an already-registered family, then a system
# font file, then the best installed serif, then `sans`. Whatever it lands on is
# cached in an option and reported ONCE per session instead of per grob.
FIG_FONT_PREFERRED <- "Source Serif 4"

.font_files <- function(pattern) {
  dirs <- c(file.path(Sys.getenv("SystemRoot", "C:/Windows"), "Fonts"),
            file.path(Sys.getenv("LOCALAPPDATA"), "Microsoft/Windows/Fonts"))
  fs <- unlist(lapply(dirs[dir.exists(dirs)], function(d)
    list.files(d, pattern = pattern, full.names = TRUE, ignore.case = TRUE)))
  fs[grepl("\\.(ttf|otf)$", fs, ignore.case = TRUE)]
}

.register_fig_font <- function() {
  cached <- getOption("neurocareers.fig_font")
  if (!is.null(cached)) return(cached)

  fam <- "sans"
  if (FIG_FONT_PREFERRED %in% sysfonts::font_families()) {
    fam <- FIG_FONT_PREFERRED
  } else {
    src <- .font_files("^SourceSerif")
    if (length(src)) {
      # perl = TRUE is required: the bold pattern uses a negative lookahead to
      # avoid picking BoldItalic, which TRE (the default engine) rejects.
      reg <- function(re) { h <- grep(re, src, value = TRUE, ignore.case = TRUE,
                                      perl = TRUE)
                            if (length(h)) h[1] else NULL }
      ok <- tryCatch({
        sysfonts::font_add(FIG_FONT_PREFERRED,
                           regular = reg("Regular|\\bRoman") %||% src[1],
                           bold = reg("Bold(?!Italic)"), italic = reg("Italic"))
        TRUE
      }, error = function(e) FALSE)
      if (ok) fam <- FIG_FONT_PREFERRED
    }
    if (fam == "sans") {
      # Nearest installed serif. Noto Serif is metric-compatible enough for a
      # figure set and ships with Windows; this keeps figures serif rather than
      # dropping to the sans default the project never intended.
      alt <- .font_files("^NotoSerif-(Regular|Bold|Italic)")
      if (length(alt)) {
        ok <- tryCatch({
          sysfonts::font_add("Noto Serif",
            regular = grep("Regular", alt, value = TRUE)[1],
            bold    = grep("Bold(?!Italic)", alt, value = TRUE, perl = TRUE)[1],
            italic  = grep("Italic", alt, value = TRUE)[1])
          TRUE
        }, error = function(e) FALSE)
        if (ok) fam <- "Noto Serif"
      }
    }
    if (fam != FIG_FONT_PREFERRED) {
      message(sprintf(
        "Figure font: '%s' is not installed; using '%s'. Install Source Serif 4 to match the house style.",
        FIG_FONT_PREFERRED, fam))
    }
  }
  options(neurocareers.fig_font = fam)
  fam
}

# ---- icon (emoji) font resolution ----
#
# Monochrome pictograms for plot_titles_by_group() (R/05_plots.R) — one per
# sector/subsector, requested alongside the job-title lists. showtext
# intercepts ALL text rendering once fig_theme() calls showtext_auto(), so an
# emoji glyph silently renders as an empty box unless its font is registered
# with sysfonts::font_add() first, exactly like Source Serif 4 above — a bare
# family = "Segoe UI Emoji" on an UNregistered font produced tofu boxes
# (checked 2026-07-31); registering it first renders every glyph below as a
# clean monochrome outline (showtext strips color-emoji tables, which is fine
# here — literal colour would break the Okabe-Ito-only rule anyway). Cached
# like .register_fig_font() and never errors if the font is missing: the
# caller falls back to a plain bullet.
ICON_FONT_PREFERRED <- "Segoe UI Emoji"
ICON_FALLBACK_GLYPH <- "•"  # bullet, used when no icon font resolves

.register_icon_font <- function() {
  cached <- getOption("neurocareers.icon_font")
  if (!is.null(cached)) return(cached)

  fam <- NA_character_
  if (ICON_FONT_PREFERRED %in% sysfonts::font_families()) {
    fam <- ICON_FONT_PREFERRED
  } else {
    src <- .font_files("^seguiemj")
    if (length(src)) {
      ok <- tryCatch({
        sysfonts::font_add(ICON_FONT_PREFERRED, regular = src[1])
        TRUE
      }, error = function(e) FALSE)
      if (ok) fam <- ICON_FONT_PREFERRED
    }
  }
  if (is.na(fam)) {
    message("Icon font '", ICON_FONT_PREFERRED,
            "' not found; figures fall back to a plain bullet.")
  }
  options(neurocareers.icon_font = fam)
  fam
}

# One pictogram per sector / subsector — a companion lookup to SECTOR_COLORS,
# same "stable across every figure" rule. Government — intramural research and
# the plain Government sector share the same building glyph on purpose (it's
# the same government), and Other/Other share a folder for the same reason.
SECTOR_EMOJI <- c(
  "Academia"   = "\U0001F393",  # graduation cap
  "Government" = "\U0001F3DB",  # classical building
  "Industry"   = "\U0001F3E2",  # office building
  "Other"      = "\U0001F4C1"   # file folder
)

SUBSECTOR_EMOJI <- c(
  "Academia — clinical trainee"       = "\U0001FA7A",  # stethoscope
  "Academia — faculty"                = "\U0001F393",  # graduation cap
  "Academia — postdoc"                = "\U0001F52C",  # microscope
  "Academia — staff scientist"        = "\U0001F9EA",  # test tube
  "AI/ML"                             = "\U0001F916",  # robot face
  "Consulting"                        = "\U0001F4CA",  # bar chart
  "Defense"                           = "\U0001F6E1",  # shield
  "Education (non-university)"        = "\U0001F4DA",  # books
  "Finance"                           = "\U0001F4B0",  # money bag
  "Government — intramural research"  = "\U0001F3DB",  # classical building
  "Government/Nonprofit"              = "\U0001F91D",  # handshake
  "Healthcare delivery"               = "\U0001F3E5",  # hospital
  "Manufacturing/Industrial"          = "\U0001F3ED",  # factory
  "Medical Devices"                   = "⚙",           # gear
  # Not yet a real value in `subsector` — see the "NOT YET WIRED INTO
  # classify_sectors()" note in R/sector_map.R. Listed anyway so the lookup is
  # ready the day that lands.
  "Neurotech (specific)"              = "\U0001F9E0",  # brain
  "Other"                             = "\U0001F4C1",  # file folder
  "Pharma/Biotech"                    = "\U0001F48A",  # pill
  "Software/Tech (general)"           = "\U0001F4BB"   # laptop
)

`%||%` <- function(a, b) if (is.null(a)) b else a

# Pure cowplot::theme_cowplot() — no added grid, no
# theme() overrides on top of it. Anything a figure needs beyond stock
# theme_cowplot() belongs in that plot function, not here.
fig_theme <- function(base_size = 12) {
  fam <- .register_fig_font()
  showtext::showtext_auto()
  cowplot::theme_cowplot(font_size = base_size, font_family = fam)
}

# Drop NA levels from a plotted column, uniformly and LOUDLY.
#
# Figures used to plot NA as though it were a sector: on the stacked
# sector-by-cohort chart the NA segment averaged 50.7% of every bar, so half of
# each column was "we have no employment data" rendered in the same visual
# grammar as a career outcome. Dropping is right, but the count that was dropped
# has to reach the caption — hence the attribute rather than a silent filter.
drop_na_levels <- function(df, ...) {
  cols <- rlang::ensyms(...)
  keep <- Reduce(`&`, lapply(cols, function(s) !is.na(df[[rlang::as_string(s)]])))
  out <- df[keep, , drop = FALSE]
  attr(out, "n_dropped") <- sum(!keep)
  out
}

# NOTE: the public-card walk-back is NOT here. It briefly was, as
# a FIGURE_CURRENT_BASES constant plus a scope_current_for_figures() helper the
# plot functions called — which left `features_full$current_sector` itself
# carrying the un-validated mix, so anything that wasn't a figure (an ad-hoc
# query, the NSF SDR comparison) silently got the wrong number. It now lives in
# join_sector_features(): the bare `current_sector` IS the dated-only
# definition, and `current_sector_incl_public_card` is the opt-in. See
# DATED_CURRENT_BASES in R/04_link.R.

# Standard caption for a figure whose denominator excludes people with no
# observable employment history. ~47% of Arm V has no dated experience row, so
# every outcome figure needs to say what it actually ran on.
n_caption <- function(n_used, n_total, what = "people") {
  sprintf("n = %s %s with observable employment history (of %s in the analytic sample).",
          format(n_used, big.mark = ","), what, format(n_total, big.mark = ","))
}
