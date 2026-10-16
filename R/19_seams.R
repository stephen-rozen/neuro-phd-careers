# R/19_seams.R — stub guards for the profile-acquisition seams.

# `format = "file"` targets error when the path is missing, but these collected
# CSVs only appear once their (billable, out-of-graph) pull has run. Same
# stub-guard contract as ensure_neurotree_link_stub(): create a header-only file
# so the graph stays green offline, and let the real pull append to it.
ensure_serp_collected_stub <- function(path,
                                       schema_from = c(
                                         "output/tables/serp_full_pull_collected.csv",
                                         "output/tables/serp_allyears_pull_collected.csv",
                                         "data/LinkedIn people profiles.csv")) {
  if (file.exists(path)) return(path)
  src <- schema_from[file.exists(schema_from)][1]
  if (is.na(src)) stop("No schema source available to stub ", path, call. = FALSE)
  hdr <- names(readr::read_csv(src, n_max = 0, col_types = readr::cols(.default = "c")))
  stub <- as.data.frame(stats::setNames(
    replicate(length(hdr), character(0), simplify = FALSE), hdr))
  readr::write_csv(stub, path)
  message("stubbed empty ", path, " (schema from ", basename(src), ")")
  path
}

# Stub for the rejects file (see ensure_serp_collected_stub for the contract).
SERP_REJECT_COLS <- c("id", "route", "nt_pid", "nt_name", "nt_institution",
                      "nt_year", "li_name", "reject_reason")
ensure_serp_rejects_stub <- function(path = "output/tables/serp_medium_rejects.csv") {
  if (file.exists(path)) return(path)
  stub <- as.data.frame(stats::setNames(
    replicate(length(SERP_REJECT_COLS), character(0), simplify = FALSE), SERP_REJECT_COLS))
  readr::write_csv(stub, path)
  message("stubbed empty ", path)
  path
}
