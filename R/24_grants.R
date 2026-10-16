# R/24_grants.R — NIH funding history from the NeuroTree peopleGrant export
#
# `data/neurotree/peopleGrant.csv.gz` (131 MB gzipped, 6,954,331 rows) links
# NeuroTree pids to federal award records. Staged 2026-07-22; brought into
# scope 2026-07-29.
#
# Source: Zenodo record 19739198, CC-BY 3.0 — attribution to academictree.org is
# required on any published finding, plus a citation to
# 10.1038/s41467-018-07034-y or 10.5281/zenodo.4247418. The award records
# themselves are the NIH ExPORTER catalog (and an NSF feed, see below).
#
# ---- what this table is, measured 2026-07-29 ----
#
# It is an AUTOMATED name+location match, exactly like authorPub*, and carries
# its own confidence columns (`score`, `score_name`, `score_loc`) plus
# `score_human` for the rows a human reviewed. It is NOT an authoritative
# person->grant crosswalk, and it is used here the same way ProQuest and
# NeuroTree membership are: a positive signal, never a gate.
#
#   * `dborigin` is one of yearly_ExPORTER_Catalog (4,354,993),
#     weekly_ExPORTER_Catalog (664,070) and **nsf (1,935,268)**. The NSF rows
#     carry the literal string "NULL" in both FULL_PROJECT_NUM and BUDGET_START,
#     so they support no activity code and no year — they are counted in
#     `nt_n_nsf_awards` for audit and excluded from every NIH feature.
#   * `BUDGET_START` comes in TWO shapes: "M/D/YYYY" (3,681,418 NIH rows) and
#     "YYYY-MM-DD" (705,551, all from the 2026 import). A "/"-only parse silently
#     drops the ISO rows and makes the corpus look like it stops in **2021** —
#     it does not. Both shapes are parsed here; see `.nt_grant_year()`.
#   * **Coverage runs through 2025** (2026: 35 rows, a partial year). The 2017
#     import covers budget years 1978-2021 and the 2026 import adds 2022-2025.
#     `importdate` being 2017 on most rows is therefore NOT evidence that funding
#     data stops in 2017. 12.6% of NIH rows have no parseable year at all
#     (BUDGET_START "NULL"/empty), so first/last funded year is missing for
#     people whose every row is undated.
#   * A row is one budget PERIOD, not one award: 5,019,063 NIH rows reduce to
#     1,784,046 APPLICATION_IDs and **341,927 distinct projects**. `n_grants`
#     counts distinct projects (`nih_project_core()`), so a 5-year R01 renewed
#     twice is one grant, not fifteen.

NT_GRANT_FILE <- "peopleGrant.csv.gz"
NT_GRANTS_PATH <- "output/tables/neurotree_grants.csv"
NT_GRANT_CALIBRATION_PATH <- "output/tables/nt_grant_threshold_calibration.csv"

# ---- confidence threshold ----
#
# CHOSEN: 0.90, calibrated against `score_human` (see
# output/tables/nt_grant_threshold_calibration.csv, rebuilt every run).
#
# 60,952 rows carry a human label (`score_human` 1 = confirmed, n = 24,883;
# -1 = rejected, n = 36,069; 0 = unreviewed, the other 6,893,379). Precision and
# recall on that labeled set, measured 2026-07-29:
#
#   thr    kept   precision  recall
#   0.70  45,056    0.545     0.987
#   0.80  34,364    0.703     0.971
#   0.85  30,189    0.770     0.934
#   0.90  23,232    0.926     0.865   <- chosen
#   0.95  18,036    0.960     0.696
#   0.99  14,760    0.978     0.580
#
# 0.90 is the knee: it buys +15.6pp precision over 0.85 for -6.9pp recall, while
# the next step (0.95) buys only +3.4pp for another -16.9pp. It also matches this
# project's standing precision-over-recall posture (see the inclusion tiers).
#
# **The labeled rows are NOT a random sample** — they are 0.9% of the table and
# 40.8% of them are positives, which cannot be the corpus base rate. A human
# reviews rows that looked ambiguous, so these figures are conditional on having
# been reviewed and are best read as a LOWER bound on precision at a given
# threshold in the unreviewed bulk. Do not quote 0.926 as the precision of the
# feature; quote it as the precision on reviewed rows.
NT_GRANT_SCORE_MIN <- 0.90

# Thresholds the calibration table reports. NT_GRANT_SCORE_MIN must be among them.
NT_GRANT_THRESHOLDS <- c(0.50, 0.60, 0.70, 0.75, 0.80, 0.85, 0.90, 0.95, 0.99)

# NIH activity codes. Deliberately narrow and literal — the point is a career
# covariate ("did this person ever hold an R01", "did they hold a training
# award"), not an exhaustive taxonomy of the 200+ codes in ExPORTER.
#
# R01-equivalent: the R01 itself plus the codes NIH treats as R01-equivalent
# research project grants (R37 Merit Award, R35 Outstanding Investigator, R29
# FIRST Award, retired 1990s). `nt_ever_r01` is the STRICT R01 flag;
# `nt_ever_r01_equiv` is the wider one, so a published claim can say which.
NT_GRANT_R01_CODES       <- c("R01")
NT_GRANT_R01_EQUIV_CODES <- c("R01", "R37", "R35", "R29")

# Training / career-development awards: F fellowships (F30/F31/F32/F33 incl.
# the F99/K00 transition), K career-development awards (K01, K08, K22, K23, K25,
# K99), and T32 institutional training grants. A T32 row is ambiguous — the
# PI of a T32 is the program director, not the trainee — so it is FLAGGED
# SEPARATELY in `nt_ever_t32` and kept OUT of `nt_ever_training_award`.
NT_GRANT_TRAINING_CODES <- c("F30", "F31", "F32", "F33", "F99",
                             "K00", "K01", "K08", "K22", "K23", "K25", "K99")
NT_GRANT_T32_CODES      <- c("T32")

# ---- field parsers ----

# BUDGET_START -> integer year. Two stored shapes (see the header note): ISO
# "YYYY-MM-DD" from the 2026 import, and "M/D/YYYY" everywhere else. A year
# outside 1930-2100 is a data error, not a date, and comes back NA (one row
# stores the year as "20").
.nt_grant_year <- function(budget_start) {
  x <- .nt_na(budget_start)
  iso <- !is.na(x) & stringr::str_detect(x, "^[0-9]{4}-")
  y <- rep(NA_integer_, length(x))
  y[iso] <- suppressWarnings(as.integer(stringr::str_sub(x[iso], 1, 4)))
  slash <- !is.na(x) & !iso
  y[slash] <- suppressWarnings(as.integer(stringr::str_replace(x[slash], "^.*/", "")))
  dplyr::if_else(!is.na(y) & y >= 1930L & y <= 2100L, y, NA_integer_)
}

# FULL_PROJECT_NUM is the NIH grant number: [type][activity 3][IC 2][serial 6]
# [-support year][suffixes], e.g. "5R01NS012345-03". Characters 2-4 are the
# activity code. Every NIH row in the export is >= 11 characters and parses.
nih_activity_code <- function(full_project_num) {
  x <- .nt_na(full_project_num)
  dplyr::if_else(is.na(x) | nchar(x) < 4L, NA_character_,
                 stringr::str_to_upper(stringr::str_sub(x, 2, 4)))
}

# The project identity, for counting grants rather than budget periods: drop the
# leading application-type digit (1 new / 2 renewal / 5 non-competing / 3 / 7 ...)
# and everything from the "-support year" onward. "5R01NS012345-03" and
# "2R01NS012345-06" are one project, "R01NS012345".
nih_project_core <- function(full_project_num) {
  x <- .nt_na(full_project_num)
  dplyr::if_else(is.na(x), NA_character_,
                 stringr::str_replace(stringr::str_sub(x, 2), "-.*$", ""))
}

.nt_is_nsf <- function(dborigin) {
  stringr::str_detect(stringr::str_to_lower(dplyr::coalesce(dborigin, "")), "nsf")
}

# ---- reader ----
#
# Memory discipline: `data.table::fread` reads only the eight columns the feature
# needs (never the whole 13-column, 6.95M-row table), the pid filter is applied
# IMMEDIATELY, and the full table is dropped before anything else runs. Peak is
# the eight character columns; the prepared return value is ~58k rows.
#
# The calibration is computed BEFORE the pid filter and returned alongside, so
# the threshold is calibrated on all 60,952 human-labeled rows rather than the
# handful that survive the filter — and the 131 MB file is still read once.
#
# `pids` is the NeuroTree pid vector to keep (`nt_link$nt_pid`). An empty or NULL
# `pids` keeps nothing, which is what makes this safe to wire up before the
# linkage exists.
fetch_people_grants <- function(pids, path = fs::path(NT_DIR, NT_GRANT_FILE)) {
  keep <- c("grantid", "pid", "score", "score_human", "score_name", "score_loc",
            "APPLICATION_ID", "BUDGET_START", "FULL_PROJECT_NUM", "dborigin")
  raw <- data.table::fread(path, select = keep, colClasses = "character",
                           showProgress = FALSE, data.table = FALSE)

  # Every field is quoted in this export, so fread's na.strings never fires on
  # the MySQL "NULL" placeholder — normalize it explicitly, as fetch_neurotree()
  # does for the other tables.
  score      <- suppressWarnings(as.numeric(.nt_na(raw$score)))
  score_human <- suppressWarnings(as.integer(.nt_na(raw$score_human)))

  calibration <- nt_grant_threshold_calibration(score, score_human)

  wanted <- if (is.null(pids)) character() else unique(stats::na.omit(as.character(pids)))
  hit <- raw$pid %in% wanted

  grants <- tibble::tibble(
    nt_pid       = .nt_na(raw$pid[hit]),
    grant_id     = .nt_na(raw$grantid[hit]),
    application_id = .nt_na(raw$APPLICATION_ID[hit]),
    project_num  = .nt_na(raw$FULL_PROJECT_NUM[hit]),
    dborigin     = .nt_na(raw$dborigin[hit]),
    score        = score[hit],
    score_human  = score_human[hit],
    score_name   = suppressWarnings(as.numeric(.nt_na(raw$score_name[hit]))),
    score_loc    = suppressWarnings(as.numeric(.nt_na(raw$score_loc[hit]))),
    budget_year  = .nt_grant_year(raw$BUDGET_START[hit])
  )

  # Source-extent facts worth reporting with any funding finding, measured on
  # the FULL table so they describe the corpus and not our slice of it.
  source_extent <- tibble::tibble(
    n_rows          = nrow(raw),
    n_pids          = dplyr::n_distinct(raw$pid),
    n_nih_rows      = sum(!.nt_is_nsf(raw$dborigin)),
    n_nsf_rows      = sum(.nt_is_nsf(raw$dborigin)),
    max_budget_year = suppressWarnings(max(
      .nt_grant_year(raw$BUDGET_START[!.nt_is_nsf(raw$dborigin)]), na.rm = TRUE)),
    n_human_labeled = sum(!is.na(score_human) & score_human != 0L),
    score_min_used  = NT_GRANT_SCORE_MIN
  )

  rm(raw, score, score_human); invisible(gc(verbose = FALSE))

  grants <- grants |>
    dplyr::mutate(
      is_nsf    = .nt_is_nsf(dborigin),
      is_nih    = !is_nsf & !is.na(project_num),
      activity  = dplyr::if_else(is_nih, nih_activity_code(project_num), NA_character_),
      proj_core = dplyr::if_else(is_nih, nih_project_core(project_num), NA_character_))

  list(grants = grants, calibration = calibration, source_extent = source_extent)
}

# ---- threshold calibration ----
#
# Precision/recall of a `score >= thr` rule against the human labels, for every
# threshold in NT_GRANT_THRESHOLDS. `score_human` is 1 (confirmed) / -1
# (rejected) / 0 (unreviewed); only the non-zero rows are evaluable.
#
# `chosen` marks NT_GRANT_SCORE_MIN so the justification in the CSV is
# self-contained. See the NT_GRANT_SCORE_MIN comment for why the labeled set is
# not a random sample and these numbers are a lower bound.
nt_grant_threshold_calibration <- function(score, score_human,
                                           thresholds = NT_GRANT_THRESHOLDS) {
  lab <- !is.na(score_human) & score_human != 0L & !is.na(score)
  s <- score[lab]
  pos <- score_human[lab] > 0L
  n_pos <- sum(pos)
  purrr::map_dfr(thresholds, function(thr) {
    kept <- s >= thr
    tp <- sum(kept & pos)
    tibble::tibble(
      threshold  = thr,
      n_labeled  = length(s),
      n_positive = n_pos,
      n_kept     = sum(kept),
      true_pos   = tp,
      false_pos  = sum(kept) - tp,
      precision  = if (sum(kept) > 0) tp / sum(kept) else NA_real_,
      recall     = if (n_pos > 0) tp / n_pos else NA_real_,
      f1 = if (sum(kept) > 0 && n_pos > 0 && tp > 0) {
        p <- tp / sum(kept); r <- tp / n_pos; 2 * p * r / (p + r)
      } else NA_real_,
      chosen = dplyr::near(thr, NT_GRANT_SCORE_MIN))
  })
}

write_nt_grant_threshold_calibration <- function(calibration,
                                                 path = NT_GRANT_CALIBRATION_PATH) {
  readr::write_csv(calibration, path)
  path
}

# ---- per-person features ----

NT_GRANTS_SCHEMA <- tibble::tibble(
  nt_pid = character(), nt_n_grants = integer(), nt_n_grant_periods = integer(),
  nt_ever_pi = logical(), nt_first_grant_year = integer(),
  nt_last_grant_year = integer(), nt_ever_r01 = logical(),
  nt_ever_r01_equiv = logical(), nt_ever_training_award = logical(),
  nt_ever_t32 = logical(), nt_grant_activity_codes = character(),
  nt_grant_score_max = double(), nt_n_nsf_awards = integer()
)

# One row per NeuroTree pid that has at least one award row clearing the score
# threshold. A pid absent from the output has no such row — which at the seam
# means "no NIH award found for a person we DID look up", distinct from the
# unlinked case (no nt_pid at all). See NT_GRANT_SEAM_COLS.
#
# `nt_ever_pi`: peopleGrant matches on the ExPORTER PI NAME, so every row is a
# PI row and there is no role column to read. `nt_ever_pi` is therefore
# identical to `nt_n_grants > 0` by construction — it is kept because it is the
# honest name for what the flag means ("named PI on >= 1 NIH award"), NOT
# because it carries information beyond the count. Do not present it as
# evidence of PI status distinct from holding a grant.
build_person_grants <- function(grants, score_min = NT_GRANT_SCORE_MIN) {
  if (nrow(grants) == 0) return(NT_GRANTS_SCHEMA)

  nsf <- grants |>
    dplyr::filter(is_nsf, !is.na(score), score >= score_min) |>
    dplyr::group_by(nt_pid) |>
    dplyr::summarise(nt_n_nsf_awards = dplyr::n_distinct(grant_id), .groups = "drop")

  nih <- grants |>
    dplyr::filter(is_nih, !is.na(score), score >= score_min)

  # An NSF-only slice still produces rows (with zeroed NIH features), so the
  # empty-NIH case takes the same coalescing path rather than a separate return.
  out <- if (nrow(nih) == 0) {
    NT_GRANTS_SCHEMA |> dplyr::select(-nt_n_nsf_awards)
  } else nih |>
    dplyr::group_by(nt_pid) |>
    dplyr::summarise(
      nt_n_grants        = dplyr::n_distinct(proj_core),
      nt_n_grant_periods = dplyr::n_distinct(application_id),
      nt_first_grant_year = .min_or_na(budget_year),
      nt_last_grant_year  = .max_or_na(budget_year),
      nt_ever_r01            = any(activity %in% NT_GRANT_R01_CODES),
      nt_ever_r01_equiv      = any(activity %in% NT_GRANT_R01_EQUIV_CODES),
      nt_ever_training_award = any(activity %in% NT_GRANT_TRAINING_CODES),
      nt_ever_t32            = any(activity %in% NT_GRANT_T32_CODES),
      nt_grant_activity_codes = paste(sort(unique(stats::na.omit(activity))),
                                      collapse = ","),
      nt_grant_score_max = max(score, na.rm = TRUE),
      .groups = "drop") |>
    dplyr::mutate(nt_ever_pi = nt_n_grants > 0L)

  out |>
    dplyr::full_join(nsf, by = "nt_pid") |>
    dplyr::mutate(
      dplyr::across(c(nt_n_grants, nt_n_grant_periods, nt_n_nsf_awards),
                    ~ as.integer(dplyr::coalesce(., 0L))),
      dplyr::across(c(nt_ever_pi, nt_ever_r01, nt_ever_r01_equiv,
                      nt_ever_training_award, nt_ever_t32),
                    ~ dplyr::coalesce(., FALSE)),
      nt_grant_activity_codes = dplyr::coalesce(nt_grant_activity_codes, NA_character_)) |>
    dplyr::select(dplyr::all_of(names(NT_GRANTS_SCHEMA))) |>
    dplyr::arrange(dplyr::desc(nt_n_grants), nt_pid)
}

# min/max over a possibly all-NA integer vector, without the -Inf/warning dance.
.min_or_na <- function(x) if (all(is.na(x))) NA_integer_ else as.integer(min(x, na.rm = TRUE))
.max_or_na <- function(x) if (all(is.na(x))) NA_integer_ else as.integer(max(x, na.rm = TRUE))

# ---- seam to the ProQuest store (which writes inclusion_table.csv) ----

write_neurotree_grants <- function(person_grants, path = NT_GRANTS_PATH) {
  readr::write_csv(person_grants, path)
  path
}

# Same contract as ensure_neurotree_link_stub(): guarantee the file exists so the
# ProQuest store's format="file" target is satisfied before this store has ever
# run, and never clobber a real table.
ensure_neurotree_grants_stub <- function(path = NT_GRANTS_PATH) {
  if (!file.exists(path)) readr::write_csv(NT_GRANTS_SCHEMA, path)
  path
}

.read_neurotree_grants <- function(nt_grants_csv) {
  readr::read_csv(nt_grants_csv, show_col_types = FALSE,
                  col_types = readr::cols(
                    nt_pid = readr::col_character(),
                    nt_n_grants = readr::col_integer(),
                    nt_n_grant_periods = readr::col_integer(),
                    nt_ever_pi = readr::col_logical(),
                    nt_first_grant_year = readr::col_integer(),
                    nt_last_grant_year = readr::col_integer(),
                    nt_ever_r01 = readr::col_logical(),
                    nt_ever_r01_equiv = readr::col_logical(),
                    nt_ever_training_award = readr::col_logical(),
                    nt_ever_t32 = readr::col_logical(),
                    nt_grant_activity_codes = readr::col_character(),
                    nt_grant_score_max = readr::col_double(),
                    nt_n_nsf_awards = readr::col_integer()))
}

# ---- core-store side of the seam ----
#
# Attach the funding columns to a person-level feature table, read off
# inclusion_table.csv. Counterpart of backfill_phd_year_from_seam(): the core
# store learns the funding history without depending on either the NeuroTree or
# the ProQuest store.
#
# Attached ONTO `features` rather than into a parallel target on purpose — an
# extra `features_*` variant that no figure consumes is exactly what
# `features_full_glassdoor` was before it was removed. No-op (all-NA columns)
# while the seam is still the stub.
#
# The three-state contract of NT_GRANT_SEAM_COLS carries through unchanged: a
# person absent from the seam, or present with nt_grant_known FALSE, gets NA —
# never 0.
attach_grant_features <- function(features, inclusion_csv) {
  inc <- .read_inclusion_table(inclusion_csv)
  cols <- intersect(NT_GRANT_SEAM_COLS, names(inc))
  if (length(cols) == 0) {
    features[NT_GRANT_SEAM_COLS] <- list(NA, NA_integer_, NA, NA_integer_,
                                         NA_integer_, NA, NA)
    return(features)
  }
  features |>
    dplyr::left_join(inc |> dplyr::distinct(id, .keep_all = TRUE) |>
                       dplyr::select(id, dplyr::all_of(cols)),
                     by = "id") |>
    dplyr::mutate(nt_grant_known = dplyr::coalesce(nt_grant_known, FALSE))
}

# ---- QC ----
#
# Coverage of the funding feature over whatever id set is passed in, stated as
# the three states that actually exist. Quote all three: "12.8% of Arm V has a
# funded-grant record" is only interpretable next to "42.8% of Arm V has a
# NeuroTree pid at all".
nt_grant_coverage <- function(nt_link, person_grants, source_extent = NULL) {
  linked <- unique(stats::na.omit(nt_link$nt_pid))
  with_grants <- intersect(linked, person_grants$nt_pid[person_grants$nt_n_grants > 0])
  pg <- person_grants |> dplyr::filter(nt_pid %in% with_grants)
  out <- tibble::tibble(
    n_linked_pids       = length(linked),
    n_with_nih_grant    = length(with_grants),
    pct_with_nih_grant  = if (length(linked) > 0) length(with_grants) / length(linked) else NA_real_,
    n_ever_r01          = sum(pg$nt_ever_r01),
    n_ever_training     = sum(pg$nt_ever_training_award),
    n_ever_t32          = sum(pg$nt_ever_t32),
    median_n_grants     = suppressWarnings(stats::median(pg$nt_n_grants)),
    n_undated           = sum(is.na(pg$nt_first_grant_year)),
    score_min_used      = NT_GRANT_SCORE_MIN)
  if (!is.null(source_extent)) out <- dplyr::bind_cols(out, source_extent |>
                                 dplyr::select(-dplyr::any_of("score_min_used")))
  out
}
