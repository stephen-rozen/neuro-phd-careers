# R/04_link.R — join experience to company table, assign sectors.

# Best fuzzy match index into `table_norm` for each of `query_norm`, or NA.
#
# Memory-safe replacement for a stringdist cross-join: amatch() keeps only the
# running best per query, so nothing proportional to length(query) x
# length(table) is ever allocated. Two further bounds:
#   * blocked on the first character — Jaro-Winkler weights a common prefix
#     heavily, so strings differing in character 1 cannot clear max_dist = 0.1
#     anyway, and blocking cuts the work ~26x at no recall cost;
#   * chunked within a block, so peak memory is chunk x block.
.best_company_match <- function(query_norm, table_norm, max_dist = 0.1,
                                chunk = 1000L) {
  out <- rep(NA_integer_, length(query_norm))
  if (!length(query_norm) || !length(table_norm)) return(out)
  qk <- substr(query_norm, 1, 1)
  tk <- substr(table_norm, 1, 1)
  for (k in unique(qk)) {
    qi <- which(qk == k)
    ti <- which(tk == k)
    if (!length(ti)) next
    tt <- table_norm[ti]
    for (s in seq(1L, length(qi), by = chunk)) {
      sel <- qi[s:min(s + chunk - 1L, length(qi))]
      m <- stringdist::amatch(query_norm[sel], tt, method = "jw",
                              maxDist = max_dist, nomatch = NA_integer_)
      out[sel] <- ti[m]          # NA index stays NA
    }
  }
  out
}

link_companies <- function(experience_long, companies) {
  co <- companies |>
    dplyr::transmute(
      company_id     = id,
      company_name   = name,
      industries,
      company_size,
      company_country = country_code
    )

  # Direct join on company_id
  direct <- experience_long |>
    dplyr::left_join(co, by = "company_id")

  # For unmatched, try a fuzzy match on normalized name. Every experience row is
  # RETAINED regardless of match outcome — an unmatched row keeps NA company
  # columns and still carries its title into sector classification. (The old
  # inner-join residual silently dropped ~89% of experience rows.)
  need_fuzzy <- is.na(direct$company_name) & !is.na(direct$company_raw)
  if (!any(need_fuzzy) || !requireNamespace("fuzzyjoin", quietly = TRUE)) {
    return(direct)
  }

  norm <- function(x) stringr::str_to_lower(stringr::str_replace_all(x,
    "\\b(inc|llc|ltd|corp|corporation|co\\.)\\b|[[:punct:]]", "")) |>
    stringr::str_squish()

  co_n <- co |>
    dplyr::mutate(name_norm = norm(company_name)) |>
    dplyr::filter(!is.na(name_norm), name_norm != "")

  # Fuzzy-match each DISTINCT unmatched name once, then map back to rows — same
  # result as row-wise matching (best candidate per name), far less work, and it
  # can't collapse repeat stints at the same company the way a per-(id, name)
  # slice did.
  lookup <- direct |>
    dplyr::filter(need_fuzzy) |>
    dplyr::distinct(company_raw) |>
    dplyr::mutate(name_norm = norm(company_raw)) |>
    dplyr::filter(name_norm != "")

  # NOT fuzzyjoin::stringdist_inner_join here — it materialises the full
  # query x table cross-product before filtering. That was survivable against the
  # 1,000-row sample company table (176k distinct unmatched names x 1,000 =
  # 176M pairs), but the real company pull took the table to 11,935 rows and the
  # same call becomes 176,112 x 11,935 = 2.1 BILLION pairs, which failed with
  # "cannot allocate vector of size 15.2 Gb" (2026-07-27).
  #
  # stringdist::amatch() returns only the BEST match per query — the same thing
  # the slice_min() below was reducing to — without ever building the matrix.
  # Blocked on the first character and chunked, so peak memory is bounded by
  # chunk x block rather than by the product of both inputs.
  m_idx <- .best_company_match(lookup$name_norm, co_n$name_norm, max_dist = 0.1)
  fz <- tibble::tibble(
    company_raw        = lookup$company_raw,
    fz_company_id      = co_n$company_id[m_idx],
    fz_company_name    = co_n$company_name[m_idx],
    fz_industries      = co_n$industries[m_idx],
    fz_company_size    = co_n$company_size[m_idx],
    fz_company_country = co_n$company_country[m_idx]
  ) |>
    dplyr::filter(!is.na(fz_company_id))

  direct |>
    dplyr::left_join(fz, by = "company_raw") |>
    dplyr::mutate(
      .hit = is.na(company_name) & !is.na(fz_company_id),
      company_id      = dplyr::if_else(.hit, fz_company_id, company_id),
      company_name    = dplyr::if_else(.hit, fz_company_name, company_name),
      industries      = dplyr::if_else(.hit, fz_industries, industries),
      company_size    = dplyr::if_else(.hit, fz_company_size, company_size),
      company_country = dplyr::if_else(.hit, fz_company_country, company_country)
    ) |>
    dplyr::select(-dplyr::starts_with("fz_"), -.hit)
}

# Assign each experience row a `subsector` (fine-grained) and a `sector`
# (Academia / Government / Industry / Other). See R/sector_map.R for the two
# levels and how they relate.
#
# NA is a real answer here. A row whose employer never
# resolved to a company record carries `industries = NA`, and it used to be
# coalesced to "Other" — so "Other" meant BOTH "an industry we don't classify"
# and "we have no idea who this employer is". Measured on Arm V: 26,230 of the
# 27,733 "Other" rows (94.6%) were the second kind. They now stay NA, and
# "Other" means only the first.
#
# THE EMPLOYER NAME IS CONSULTED IN TWO PLACES, for two different failures of
# the company_id join (2026-07-31). Both use is_academic_employer_name()
# (R/sector_map.R); `company_name_basis` records which fired. See METHODS.md,
# "Employer-name overrides".
#
#   join_override     — the join RESOLVED but to the wrong company. LinkedIn
#     aliases unrelated employers onto generic "sink" company_ids, so a row can
#     inherit an `industries` string belonging to a company it has nothing to
#     do with. Where the employer's own name reads unambiguously academic and
#     the industries-derived subsector says otherwise, the name wins.
#     `subsector_join_only` keeps the un-overridden answer for the audit.
#
#   unresolved_rescue — the join NEVER RESOLVED, so `industries` is NA and the
#     lookup has nothing to read. This is the far larger failure: 67.4% of
#     Arm-V experience rows have no company record, and it is why 55.1% of them
#     had no subsector. On these rows the name is the only evidence available.
#
# The rescue does NOT resurrect the "unknown is not Other" decision below. A
# row whose employer is unresolved AND whose name says nothing still goes to
# NA; only the ones that name an institution type are recovered.
#
# TWO SETS OF COLUMNS, AND THE RESCUE IS OFF BY DEFAULT (the same choice, for
# the same reason, as the public-card branch in join_sector_features()). `subsector` / `sector` carry the override only
# and are byte-identical to what they were before the rescue existed;
# `subsector_incl_name_rescue` / `sector_incl_name_rescue` carry both.
#
# WHY, because at row level the rescue is plainly the more accurate answer and
# it would be easy to conclude it should just be the default: the missingness
# it fixes is NOT random. Unresolved employers skew academic — universities
# have thin LinkedIn company pages — so a rule that recognises only academic
# names recovers academic missingness and leaves the industry side untouched.
# The classified denominator grows almost entirely on one side: switching it
# on moves the Academia share of `current_sector` up by several points and
# breaks agreement with the NSF Survey of Doctorate Recipients benchmark,
# which the default columns match. That swing is a classification artifact,
# not a finding — the mirror image of what including the public card did, and
# it fails the same check.
#
# THE FIX IS A SYMMETRIC RULE, not a better academic one: name rules for
# pharma/biotech, hospital and tech employers so both sides of the missingness
# are recovered together. Until that exists, the bare columns are the ones
# to report.
classify_sectors <- function(linked_exp) {
  src <- "R/sector_map.R"
  source(src, local = TRUE)

  apply_lookup <- function(industries) {
    out <- rep(NA_character_, length(industries))
    lo  <- stringr::str_to_lower(dplyr::coalesce(industries, ""))
    # Market research is commercial. Claimed first so the bare-"research" rule
    # below can never take it into the academic bucket.
    out[stringr::str_detect(lo, MARKET_RESEARCH_RE)] <- "Consulting"
    for (i in seq_len(nrow(subsector_lookup_table))) {
      hit <- stringr::str_detect(lo, subsector_lookup_table$pattern[i])
      out[is.na(out) & hit] <- subsector_lookup_table$subsector[i]
    }
    # Known industry string, none of our patterns -> genuinely "Other".
    # Absent industry string -> stays NA.
    out[is.na(out) & !is.na(industries)] <- "Other"
    out
  }

  # The subsector rule itself, factored out so it can be evaluated twice — once
  # on the industries-derived value the company_id join produced, and once on
  # the name-corrected one — and the two compared in the audit.
  resolve_subsector <- function(sub_industry, sub_title, intramural) {
    # A bare "research scientist"-type title only reads as academia when the
    # employer's industry is academic or unknown — a research scientist at a
    # pharma/tech company stays in that industry's subsector.
    sub_title <- dplyr::if_else(
      sub_title == "Academia — staff scientist" &
        !is.na(sub_industry) &
        sub_industry != "Academia — staff scientist",
      NA_character_, sub_title)
    dplyr::case_when(
      # Training and faculty titles win everywhere, including inside
      # government labs: an NIH postdoc is a postdoc, not an intramural
      # staff scientist.
      sub_title %in% c("Academia — postdoc", "Academia — faculty",
                       "Academia — clinical trainee") ~ sub_title,
      intramural                                      ~ "Government — intramural research",
      !is.na(sub_title)                               ~ sub_title,
      TRUE                                            ~ sub_industry
    )
  }

  linked_exp |>
    dplyr::mutate(
      sub_from_industry_join = apply_lookup(industries),
      sub_from_title         = classify_academic_title(title_raw),
      .intramural            = is_intramural_gov(company_name) |
                               is_intramural_gov(company_raw),
      # The company_id join is low-confidence exactly where the employer's own
      # name declares an institution type the joined `industries` contradicts.
      # COMPANY_JOIN_TRUSTED_SUBSECTORS holds the two the name may not overrule.
      company_join_low_confidence =
        is_academic_employer_name(company_raw) &
        !.intramural &
        !is.na(sub_from_industry_join) &
        !stringr::str_starts(sub_from_industry_join, "Academia") &
        !sub_from_industry_join %in% COMPANY_JOIN_TRUSTED_SUBSECTORS,
      # The OTHER half of the same problem: the employer never resolved to a
      # company record at all, so there is no `industries` for the lookup to
      # read and the row falls to NA. Where the name declares an institution
      # type, the name is the only evidence there is — and it is good evidence.
      company_unresolved_name_rescue =
        is_academic_employer_name(company_raw) &
        !.intramural &
        is.na(sub_from_industry_join),
      # Which of the two name-driven branches fired, if either. Kept as a
      # single column so the audit CSV and any figure can tell an override
      # (join contradicted the name) from a rescue (no join at all) without
      # re-deriving the conditions.
      company_name_basis = dplyr::case_when(
        company_join_low_confidence     ~ "join_override",
        company_unresolved_name_rescue  ~ "unresolved_rescue",
        TRUE                            ~ NA_character_
      ),
      # The OVERRIDE feeds the default columns; the RESCUE does not. See the
      # "two sets of columns" note above.
      sub_from_industry = dplyr::if_else(
        company_join_low_confidence,
        "Academia — staff scientist", sub_from_industry_join),
      sub_from_industry_rescued = dplyr::if_else(
        company_unresolved_name_rescue,
        "Academia — staff scientist", sub_from_industry),
      subsector = resolve_subsector(sub_from_industry, sub_from_title,
                                    .intramural),
      subsector_incl_name_rescue = resolve_subsector(sub_from_industry_rescued,
                                                     sub_from_title, .intramural),
      # What the un-corrected join would have said. Identical to `subsector`
      # wherever the override did not fire, and on rows where it fired but a
      # faculty/postdoc title had already won anyway.
      subsector_join_only = resolve_subsector(sub_from_industry_join,
                                              sub_from_title, .intramural),
      sector = collapse_to_sector(subsector),
      sector_incl_name_rescue = collapse_to_sector(subsector_incl_name_rescue)
    ) |>
    dplyr::select(-.intramural)
}

# Every row where the company_raw name signal actually changed the answer,
# worst-first by how many rows a given (employer, sink id) pair moved. Same
# contract as neurotech_companies.csv / pq_feeder_review.csv: the name rule is
# inspectable, not silently trusted.
#
# BOTH branches are written, distinguished by `company_name_basis` — the
# rescues are the larger set and the ones a reviewer is most likely to want to
# spot-check, so excluding them would defeat the purpose of the file.
#
# The comparison is `subsector_incl_name_rescue` against `subsector_join_only`
# (what the raw join said), because that is the only pairing that shows BOTH
# branches: the rescue leaves the default `subsector` untouched by design, so a
# diff against it would show every rescue as a no-op. Rescue rows have
# `subsector_join_only` NA, hence the NA-safe inequality.
write_company_join_overrides <- function(sectored_exp, path) {
  out <- sectored_exp |>
    dplyr::filter(
      !is.na(company_name_basis),
      !dplyr::coalesce(subsector_incl_name_rescue == subsector_join_only,
                       FALSE)) |>
    dplyr::count(company_name_basis, company_raw, company_id, company_name,
                 industries, subsector_join_only,
                 subsector_incl_name_rescue,
                 sector_join_only = collapse_to_sector(subsector_join_only),
                 sector_incl_name_rescue =
                   collapse_to_sector(subsector_incl_name_rescue),
                 name = "n_rows", sort = TRUE)
  readr::write_csv(out, path)
  path
}

# Sectors that do NOT count as an industry exit, for time-to-industry. Derived
# from the mega sector rather than listed by hand, so a new subsector can't
# silently become an "industry job". Academia (including clinical residency,
# which is supervised training, not an industry exit), Government and Other all
# fail to count; NA fails too, because an unknown employer is not evidence of
# anything.
is_industry_exit <- function(sector) !is.na(sector) & sector == "Industry"

# THE current-job rule, in one place: one experience row per person, tagged with
# which branch supplied it. Both the feature table and any figure that needs
# "where does this person work now" go through here, so they cannot drift apart
# — they did briefly on 2026-07-31, when the feature table gained the
# public-card branch and plot_neurotech_share() kept an `is_current`-only
# numerator against the grown denominator.
#
#   is_current            the person's own flag, latest-started such row
#   latest_dated          no row claims to be current; most recently started
#   public_card_top_entry no DATED row at all; the top unredacted array entry
#
# The third branch is the logged-out payload — see METHODS.md, "Date-less profiles".
current_job_rows <- function(sectored_exp, ids = NULL) {
  ex <- if (is.null(ids)) sectored_exp else
    dplyr::filter(sectored_exp, id %in% ids)
  # EXPLICIT TIE-BREAK, same reason as first_post's in join_sector_features():
  # `slice_max(with_ties = FALSE)` takes the first row in the incoming order
  # among equal start dates, so two jobs starting the same month were being
  # separated by whatever order the parser emitted (it moved 2 people when
  # exp_order was added). Ascending exp_order means the tie goes to the card
  # HIGHER on the profile, which for a current job is the right way round.
  ex    <- dplyr::arrange(ex, id, exp_order)
  dated <- dplyr::filter(ex, !is.na(start_date))

  flagged <- dated |>
    dplyr::filter(is_current) |>
    dplyr::group_by(id) |>
    dplyr::slice_max(start_date, n = 1, with_ties = FALSE) |>
    dplyr::ungroup() |>
    dplyr::mutate(current_job_basis = "is_current")

  fallback <- dated |>
    dplyr::filter(!id %in% flagged$id) |>
    dplyr::group_by(id) |>
    dplyr::slice_max(start_date, n = 1, with_ties = FALSE) |>
    dplyr::ungroup() |>
    dplyr::mutate(current_job_basis = "latest_dated")

  public <- ex |>
    dplyr::filter(!id %in% dated$id, !is_redacted_row %in% TRUE) |>
    dplyr::group_by(id) |>
    dplyr::slice_min(exp_order, n = 1, with_ties = FALSE) |>
    dplyr::ungroup() |>
    dplyr::mutate(current_job_basis = "public_card_top_entry")

  dplyr::bind_rows(flagged, fallback, public)
}

# The bases whose sector assignment is comparable to the rest of the pipeline,
# and therefore what the BARE `current_sector` / `current_subsector` /
# `current_ucot_sector` columns are built from.
#
# `public_card_top_entry` is excluded. Those
# rows carry no job title — `title_raw` repeats the company name on 98.5% of
# them — so classify_academic_title() fires on 0.6% of them against 37.8% of
# dated rows, and 68.9% of their unresolved employers are universities. Academia
# is deleted from them mechanically, which shifted the sector shares by
# double digits and broke the agreement with the NSF Survey of Doctorate
# Recipients benchmark that the dated-only definition holds.
#
# THE PUBLIC-CARD ANSWERS ARE NOT DISCARDED — they ride the parallel
# `*_incl_public_card` columns, and `current_job_basis` is populated for
# everyone. Opting in is naming that column; the bare name always means the
# validated, dated-only definition, so an ad-hoc query gets the publishable
# number by default rather than by remembering a rule.
DATED_CURRENT_BASES <- c("is_current", "latest_dated")

# Post-PhD experience rows (see METHODS.md, "First post-PhD position"): earliest
# experience with start_date >= phd_year onward. Postdocs count as the first
# position. Pre-PhD jobs (grad RA-ships, internships) never enter this set.
# Shared by join_sector_features() (first_sector/date) and
# first_post_phd_job() (first employer NAME) so both quote the same job.
post_phd_experience_rows <- function(sectored_exp, features) {
  sectored_exp |>
    dplyr::filter(!is.na(start_date)) |>
    dplyr::inner_join(features |> dplyr::select(id, phd_year), by = "id") |>
    dplyr::filter(!is.na(phd_year), lubridate::year(start_date) >= phd_year) |>
    # TIE-BREAK IS EXPLICIT (2026-07-31). Two jobs can share a start month, and
    # `first()` below then took whatever row order the parse happened to emit —
    # so adding `exp_order` to parse_experience() silently moved 7 people's
    # first_sector purely by reordering rows. LinkedIn lists the array
    # most-recent-first, so among equal start dates the EARLIEST job is the one
    # furthest down the profile: descending exp_order.
    dplyr::arrange(id, start_date, dplyr::desc(exp_order))
}

# One row per id: the first post-PhD job itself (company_name, sector, ...),
# not just its date/sector summarised. Used for "top first employer" figures.
first_post_phd_job <- function(sectored_exp, features) {
  post_phd_experience_rows(sectored_exp, features) |>
    dplyr::group_by(id) |>
    dplyr::slice_head(n = 1) |>
    dplyr::ungroup()
}

join_sector_features <- function(features, sectored_exp) {
  dated <- sectored_exp |>
    dplyr::filter(!is.na(start_date)) |>
    dplyr::inner_join(features |> dplyr::select(id, phd_year), by = "id")

  post <- post_phd_experience_rows(sectored_exp, features)

  first_post <- post |>
    dplyr::group_by(id) |>
    dplyr::summarise(
      first_subsector    = dplyr::first(subsector),
      first_sector       = dplyr::first(sector),
      # Opt-in, off by default — see the two-sets-of-columns note on
      # classify_sectors(). Same job, same row, name-rescue applied.
      first_subsector_incl_name_rescue = dplyr::first(subsector_incl_name_rescue),
      first_sector_incl_name_rescue    = dplyr::first(sector_incl_name_rescue),
      first_sector_date  = dplyr::first(start_date),
      n_employers_post   = dplyr::n_distinct(dplyr::coalesce(company_id, company_raw)),
      n_sector_switches  = sum(diff(as.integer(factor(sector))) != 0, na.rm = TRUE),
      .groups = "drop"
    )

  # Time to first industry job: earliest post-PhD job in the Industry sector,
  # wherever it falls in the sequence — a postdoc-then-industry path counts,
  # rather than being censored because the FIRST job wasn't industry.
  first_industry <- post |>
    dplyr::filter(is_industry_exit(sector)) |>
    dplyr::group_by(id) |>
    dplyr::summarise(first_industry_date = min(start_date), .groups = "drop")

  # CURRENT JOB: the person's own `is_current` flag decides,
  # falling back to the most recently started dated job only when no row claims
  # to be current.
  #
  # This replaces "the latest-STARTED dated job", which was not the same thing:
  # measured on Arm V, 535 of 4,705 people (11.4%) had their "current" sector
  # taken from a job that had already ended, and 128 got a different answer than
  # the is_current rule gives. `current_job_basis` records which rule fired, so a
  # fallback answer is never mistaken for an asserted one.
  # The `public_card_top_entry` branch is deliberately NOT
  # extended to first_sector or time-to-industry: those need a start date, which
  # these rows do not have. `has_employment_history` stays FALSE for them too,
  # so the survival risk set is unchanged by it.
  #
  # TWO SETS OF COLUMNS, and the BARE NAME IS THE VALIDATED ONE. See
  # DATED_CURRENT_BASES: `current_sector` / `current_subsector` cover the dated
  # bases only, `*_incl_public_card` cover all three. This was briefly the other
  # way round — the walk-back lived in the figure layer, so `features_full`
  # itself still carried the un-validated mix and an ad-hoc query (or the NSF
  # comparison) silently got the wrong academic share. The default a reader
  # reaches for has to be the publishable one.
  current <- current_job_rows(sectored_exp, ids = features$id) |>
    dplyr::transmute(
      id, current_job_basis,
      current_subsector_incl_public_card = subsector,
      current_sector_incl_public_card    = sector,
      .dated = current_job_basis %in% DATED_CURRENT_BASES,
      current_subsector = replace(subsector, !.dated, NA),
      current_sector    = replace(sector,    !.dated, NA),
      # The name-rescue opt-in. Gated on DATED_CURRENT_BASES exactly like the
      # bare columns, so this varies ONE thing against the published default —
      # combining it with the public-card branch would confound two artifacts.
      current_subsector_incl_name_rescue =
        replace(subsector_incl_name_rescue, !.dated, NA),
      current_sector_incl_name_rescue =
        replace(sector_incl_name_rescue,    !.dated, NA)) |>
    dplyr::select(-.dated)

  features |>
    dplyr::left_join(first_post, by = "id") |>
    dplyr::left_join(current, by = "id") |>
    dplyr::left_join(first_industry, by = "id") |>
    dplyr::mutate(
      # Months, not years. The year-difference version was too
      # coarse to see anything: it put the median at a flat 2.0 years and
      # collapsed every within-year transition to zero.
      time_to_first_industry_mo =
        as.numeric(lubridate::interval(
          lubridate::make_date(phd_year, 1L, 1L), first_industry_date) /
          months(1)),
      time_to_first_industry_yrs = time_to_first_industry_mo / 12,
      # TRUE where the person has any dated employment at all — i.e. where an
      # absence of industry employment is EVIDENCE rather than a gap in the
      # payload. The survival figure's risk set is built on this; without it,
      # people with no experience data are silently counted as "never left
      # academia" (3,412 of Arm V, measured 2026-07-29).
      has_employment_history = id %in% dated$id
    ) |>
    dplyr::select(-first_industry_date)
}

# UCOT-Exp2 twin of join_sector_features()'s first/current sector columns
# Mirrors the same "first post-PhD position" / "is_current
# with latest-dated fallback" rules on `ucot_sector` instead of `sector`, so the
# two classifications are readable as two answers to the same question rather
# than two different questions. Deliberately narrower than
# join_sector_features(): no subsector, no switch counts, no time-to-industry —
# those are project-local concepts UCOT does not define an equivalent for.
#
# Takes `sectored_exp_ucot` (= attach_ucot(sectored_exp), carrying `ucot_sector`
# per experience row), not `sectored_exp` itself.
join_ucot_sector_features <- function(features, sectored_exp_ucot) {
  dated <- sectored_exp_ucot |>
    dplyr::filter(!is.na(start_date)) |>
    dplyr::inner_join(features |> dplyr::select(id, phd_year), by = "id")

  post <- dated |>
    dplyr::filter(!is.na(phd_year), lubridate::year(start_date) >= phd_year) |>
    # Same explicit tie-break as join_sector_features().
    dplyr::arrange(id, start_date, dplyr::desc(exp_order))

  first_post <- post |>
    dplyr::group_by(id) |>
    dplyr::summarise(first_ucot_sector = dplyr::first(ucot_sector), .groups = "drop")

  # Same three-branch rule as join_sector_features(), from the same function, so
  # the two classifications answer the same question over the same denominator.
  # Same split as join_sector_features(), so the two classifications stay
  # comparable on both the default and the opt-in column.
  current <- current_job_rows(sectored_exp_ucot, ids = features$id) |>
    dplyr::transmute(
      id,
      current_ucot_sector_incl_public_card = ucot_sector,
      current_ucot_sector = replace(
        ucot_sector, !current_job_basis %in% DATED_CURRENT_BASES, NA))

  features |>
    dplyr::left_join(first_post, by = "id") |>
    dplyr::left_join(current, by = "id")
}
