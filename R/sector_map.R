# R/sector_map.R — taxonomy for employment classification
#
# TWO LEVELS:
#
#   sector    — the four-level headline: Academia / Government / Industry / Other.
#               This is what the figures and any published claim run on.
#   subsector — the fine-grained label (Pharma/Biotech, Defense, Software/Tech,
#               Academia — postdoc, ...). Detail, drill-down, and the thing that
#               gets revised when the taxonomy is refined.
#
# `sector` is DERIVED from `subsector` by collapse_to_sector(), so the two can
# never disagree. Adding a subsector means adding it to SUBSECTOR_TO_SECTOR as
# well — the collapse is a lookup, not a regex, precisely so that a new
# subsector fails loudly rather than defaulting into "Industry".
#
# NOTE ON NAMING: "subsector" is provisional. If the project adopts the
# Unified Career Outcomes Taxonomy for this level (see R/22_ucot.R), it maps to
# UCOT's "career type" and should be renamed with it. The mapping lives in one table below to make that cheap.

neuro_patterns <- c(
  "neurosci", "neurobio", "neuro-?engineer",
  "computational neuroscience",
  "systems neuroscience", "behavioral neuroscience"
)

# Keys are regex matched (case-insensitive) against the `industries` field.
# First match wins — order matters; put specific before general.
subsector_lookup_table <- tibble::tribble(
  ~pattern,                                                                         ~subsector,
  # NOTE: there is NO neurotech row here, deliberately. See is_neurotech_company()
  # below — this table matches `industries`, and no LinkedIn industry string ever
  # says "neuro".
  "pharmaceutical|biotech|drug discovery",                                          "Pharma/Biotech",
  "medical (device|equipment)|medtech",                                             "Medical Devices",
  # Split across two rows purely to keep the lines readable - both map to the
  # same subsector, so the order between them does not matter.
  "hospital|health (care|services)|clinical|medical practice",                      "Healthcare delivery",
  "veterinar|diagnostic|physician|nursing",                                         "Healthcare delivery",
  # THIS ROW BARELY FIRES (measured 2026-07-31): 14 of 472,622 experience rows,
  # all the literal LinkedIn industry string "Artificial Intelligence"; 1
  # person in current_subsector, 0 in first_subsector. Same failure mode as
  # the pre-fix neurotech rule (below) — the category
  # exists in LinkedIn's industry vocabulary but almost no employer's
  # `industries` string uses it. `is_ai_ml_title()` below is the fix, matched
  # on the ROLE instead, same move as classify_academic_title(). It is NOT
  # wired into `subsector` (would need a core-store rebuild); figures call it
  # directly, same contract as is_neurotech_company().
  "artificial intelligence|machine learning|\\bai\\b",                              "AI/ML",
  # "technology, information" catches LinkedIn's "Technology, Information and
  # Internet" / "... and Media", which "information technology" does not.
  "software|information (technology|services)|technology, information|saas",        "Software/Tech (general)",
  "consult",                                                                        "Consulting",
  # "financ" (not "finance") so "Financial Services" matches.
  "venture capital|investment|financ|banking|accounting",                           "Finance",
  "government|nonprofit|non[- ]?profit|public policy|international affairs",        "Government/Nonprofit",
  # "market research" is EXCLUDED here and handled below as Consulting. It used
  # to fall into the academic-research bucket on the bare word "research",
  # putting 49 Arm-V rows of commercial market research into academia.
  "higher education|university",                                                    "Academia — staff scientist",
  "research services|nanotechnology research|space research|^research$",            "Academia — staff scientist",
  # The three rows below are LAST on purpose: they are broad, so every row above
  # must get first refusal. Defense precedes Manufacturing so that "Defense and
  # Space Manufacturing" reads as Defense, not as a factory.
  "defen[cs]e|armed forces|military",                                               "Defense",
  "manufactur|industrial|\\bmining\\b|\\bmetals?\\b|packaging",                     "Manufacturing/Industrial",
  "education|e-?learning|school|training",                                          "Education (non-university)"
)

# Market research is commercial, not academic. Checked BEFORE the lookup so the
# bare-"research" rule above can never claim it.
MARKET_RESEARCH_RE <- "market research"

# ---- intramural government research ---------------------------------------
#
# Matched on the EMPLOYER NAME, not on `industries` — this is the whole reason
# it needs its own rule. NIH, NSF, NINDS, NIDA and the VA all carry LinkedIn
# industry "Research Services" or "Government Administration", so an
# industry-driven rule sends them either to Academia (1,533 Arm-V rows measured
# 2026-07-29) or to a generic Government bucket. Neither is right: an intramural
# staff scientist at NINDS is doing a recognisably different job from a
# university staff scientist and from a policy analyst, and "did neuro PhDs go
# to government labs" is a question a reader will ask.
#
# Deliberately NARROW. Only employers that are unambiguously government research
# bodies appear here; a university operating a federal contract does not.
INTRAMURAL_GOV_RE <- paste0(
  "national institutes? of health|\\bnih\\b|",
  "national institute (of|on) |",
  "\\b(ninds|nimh|nida|niaaa|nichd|nia|nei|nidcd|niehs|ncats|nci)\\b|",
  "national science foundation|\\bnsf\\b|",
  "food and drug administration|\\bfda\\b|",
  "centers for disease control|\\bcdc\\b|",
  "veterans (affairs|health)|\\bva medical|\\bva healthcare|",
  "walter reed|army research|naval research|air force research|",
  "national laborator|sandia|los alamos|oak ridge|lawrence livermore|",
  "howard hughes medical institute|\\bhhmi\\b"
)

# HHMI is a private foundation, not a federal body, and sits here only because
# its intramural-style staff-scientist positions are the same job. Flagged so
# the choice is visible if a reviewer asks.
is_intramural_gov <- function(company_name) {
  stringr::str_detect(stringr::str_to_lower(dplyr::coalesce(company_name, "")),
                      INTRAMURAL_GOV_RE)
}

# ---- academic employer NAME (company_id join-confidence guard) --------------
#
# Matched on the EMPLOYER NAME, same move as is_intramural_gov() above and for a
# related reason — except here the unreliable thing is not the industry
# vocabulary but the JOIN. LinkedIn's `experience.company_id` silently aliases
# unrelated employers onto a fixed set of generic "sink" ids, and every row
# landing on a sink inherits that sink's `industries`. Two measured examples
# (2026-07-31):
#
#   company_id "university-of-delhi"    (industries "Biotechnology Research")
#     collects 461 rows whose company_raw is University of Leeds / Adelaide /
#     Memphis / Guelph / Sheffield — 330 of them classified Industry,
#     Pharma/Biotech.
#   company_id "university-of-mianwali" (industries "Government Administration")
#     collects 304 rows whose company_raw is University of Miyazaki / Maine /
#     Miami / Sulaimani — 249 classified Government, Government/Nonprofit.
#
# The sinks are not the whole story, which is why this is a name rule and not a
# denylist of bad ids: plenty of correctly-joined universities carry a nonsense
# `industries` of their own (UCLA is "Information Services", Boston University
# is "Public Relations and Communications"). Both failures have the same shape —
# the employer's own name says "university" and the industries-derived subsector
# says something else — and one rule fixes both.
#
# DELIBERATELY NARROW, same philosophy as is_intramural_gov() and
# is_neurotech_company(). Only tokens that name an institution type outright.
# "academy" is NOT here: it would take the National Academy of Sciences and the
# New York Academy of Sciences (learned societies, correctly Government/
# Nonprofit) into Academia. Publishers and test boards that merely borrow a
# campus word are excluded explicitly.
ACADEMIC_NAME_RE <- paste0(
  "\\buniversi|",                      # university/universities/universidad/universität/université
  "\\bcollege\\b|",
  "institute of technology|\\bpolytechnic\\b|",
  "school of medicine|medical school|",
  "school of public health|graduate school"
)

# NAMED institutions the type-token regex above cannot reach, because their
# usual written form contains no institution-type word at all. Kept in a
# SEPARATE constant on purpose: the list above is a rule, this one is an
# enumeration, and the two rot differently — a new campus needs a line here,
# a new spelling of "university" does not.
#
# Every entry is anchored (`\\b`, or a multi-word phrase) rather than a bare
# substring, because the failure mode for acronyms is collision, not recall:
# unanchored "mit" matches "summit", "smith" and "transmitter". Acronyms whose
# expansion is ambiguous outside a university context are deliberately ABSENT —
# "UCL" is also the ulnar collateral ligament and appears in sports-medicine
# employer names; it costs 7 rows to leave out and is not worth the collision.
#
# Measured on Arm V (2026-07-31): adds 791 rows / 187 distinct employer names
# on top of ACADEMIC_NAME_RE, taking name-rule coverage of the unresolved-
# employer rows from 67.1% to 72.4%. The full added list was audited by hand;
# it is UC campuses, Caltech, MIT, SUNY, UT Southwestern, and the freestanding
# research institutes below, with no false positives in the top 40 by row count.
ACADEMIC_NAME_NAMED_RE <- paste0(
  # University of California campuses — written as "UC <campus>" or as a
  # single acronym far more often than as "University of California, X".
  "\\buc ?(berkeley|davis|irvine|los angeles|merced|riverside|san diego|",
  "san francisco|santa barbara|santa cruz)\\b|",
  "\\bucla\\b|\\bucsf\\b|\\bucsd\\b|\\bucsb\\b|",
  # Other campuses whose acronym is unambiguous in an employer field.
  "\\bcaltech\\b|\\bmit\\b|\\bcuny\\b|\\bsuny\\b|\\bnyu\\b|\\busc\\b|",
  "\\but (southwestern|health|austin|md anderson)|",
  # Freestanding non-profit research institutes. These are Academia for this
  # analysis — a postdoc at Cold Spring Harbor is doing the same job as a
  # postdoc at a university, and the title rule already routes the trainees.
  # They are NOT in INTRAMURAL_GOV_RE: none is a federal body.
  "\\bmax planck\\b|\\bcold spring harbor\\b|\\bscripps research\\b|",
  "\\ballen institute\\b|\\bkennedy krieger\\b|\\bsalk institute\\b|",
  "\\bjackson laboratory\\b|\\bbroad institute\\b|\\bwhitehead institute\\b"
)

ACADEMIC_NAME_EXCLUDE_RE <- paste0(
  "university press|college board|\\bventure capital\\b|consulting group"
)

is_academic_employer_name <- function(company_raw) {
  lo <- stringr::str_to_lower(dplyr::coalesce(company_raw, ""))
  (stringr::str_detect(lo, ACADEMIC_NAME_RE) |
     stringr::str_detect(lo, ACADEMIC_NAME_NAMED_RE)) &
    !stringr::str_detect(lo, ACADEMIC_NAME_EXCLUDE_RE)
}

# Industry-derived subsectors the name signal is NOT allowed to overrule, even
# when the employer name reads academic.
#
#   Healthcare delivery — "Vanderbilt University Medical Center" is an academic
#     medical centre AND a health system, and the project has already settled
#     that a practising clinician there is Healthcare delivery -> Industry while
#     a trainee is caught by classify_academic_title(). Overriding would
#     silently reopen that call for 5,112 rows.
#   Defense — the ambiguous cases are exactly the ones the name would catch:
#     the Johns Hopkins University Applied Physics Laboratory (a university
#     FFRDC, deliberately placed under Government) and Marine Corps University
#     (a service school, not a campus).
COMPANY_JOIN_TRUSTED_SUBSECTORS <- c("Healthcare delivery", "Defense")

# ---- neurotech -------------------------------------------------------------
#
# MATCHED ON THE EMPLOYER NAME, like is_intramural_gov() and for the same
# reason. `Neurotech (specific)` used to sit at the top of
# subsector_lookup_table, matched against `industries` — and it NEVER FIRED,
# not once. Measured 2026-07-30: of 11,935 company records, **zero** have
# "neuro" anywhere in `industries`, because LinkedIn's industry vocabulary has
# no neurotech category at all. fig_neurotech_share.png was therefore a flat
# line at exactly 0% for its whole life, which read as "neuro PhDs don't go to
# neurotech" rather than "this rule cannot match".
#
# THREE CONDITIONS, ALL REQUIRED. The name signal alone is far too loose — it
# hits 424 of 11,935 companies, most of them neuropsychology practices,
# hospitals, learned societies and university institutes:
#
#   1. the NAME carries a neuro signal;
#   2. `industries` is COMMERCIAL. This is what keeps academia out. Note the
#      absence of "research services" and "higher education" — those are the
#      industry strings Max Planck institutes and university neuroscience
#      centres carry, and they are already correctly routed to Academia;
#   3. `organization_type` is not Nonprofit / Educational / Government Agency.
#      This is the clean gate that a name regex cannot be: it removes the
#      American Academy of Neurology and the Paris Brain Institute without
#      needing to enumerate them.
#
# Measured 2026-07-30: 88 companies — Neuralink, NeuroPace, Blackrock Neurotech,
# Precision Neuroscience, Neurocrine, Neurona, Motif Neurotech, ClearPoint Neuro
# — with the clinics, societies and institutes excluded. Audit the full list in
# output/tables/neurotech_companies.csv before quoting any share.
#
# NOT YET WIRED INTO classify_sectors() (2026-07-30). Doing so needs
# `organization_type` carried through link_companies(), which is a core-store
# rebuild; the figure applies this predicate directly against `companies`
# instead. So `subsector == "Neurotech (specific)"` is still absent from the
# store — read the figure, not the column, until that lands.
NEUROTECH_NAME_RE <- "neuro|brain|neural|cortex|synap"

NEUROTECH_COMMERCIAL_IND_RE <- paste0(
  "medical (device|equipment)|^medical device|biotechnology|",
  "pharmaceutical|software|semiconductor|nanotechnolog|computer hardware|",
  "information technology|technology, information|appliances, electrical"
)

NEUROTECH_ORG_BLOCK_RE <- "nonprofit|educational|government agency"

# organization_type may be absent (NA) — a missing value does NOT block, because
# the commercial-industry gate has already done most of the work and 1,320 of
# our company records carry no organization_type at all.
is_neurotech_company <- function(company_name, industries,
                                 organization_type = NA_character_) {
  lower <- function(x) stringr::str_to_lower(dplyr::coalesce(x, ""))
  ot <- lower(organization_type)
  stringr::str_detect(lower(company_name), NEUROTECH_NAME_RE) &
    stringr::str_detect(lower(industries), NEUROTECH_COMMERCIAL_IND_RE) &
    !stringr::str_detect(ot, NEUROTECH_ORG_BLOCK_RE)
}

# ---- title-driven classification -------------------------------------------
#
# Titles override industry when the role itself names the sector. A
# "Postdoctoral Fellow" at a hospital is still an academic postdoc.
#
# CLINICAL RESIDENCY IS ACADEMIA. A residency or clinical
# fellowship is supervised training at a teaching hospital, not industry
# employment — treating it as an industry exit made "time to first industry job"
# fire on MD/PhDs entering their residency.
classify_academic_title <- function(title) {
  t <- stringr::str_to_lower(dplyr::coalesce(title, ""))
  dplyr::case_when(
    stringr::str_detect(t, "postdoc|post[- ]?doctoral")                     ~ "Academia — postdoc",
    stringr::str_detect(t, "\\bresident\\b|residency|clinical fellow|house officer|attending physician") ~ "Academia — clinical trainee",
    stringr::str_detect(t, "assistant professor|associate professor|^professor|faculty|lecturer") ~ "Academia — faculty",
    stringr::str_detect(t, "staff scientist|research scientist|research associate|principal investigator") ~ "Academia — staff scientist",
    TRUE                                                                   ~ NA_character_
  )
}

# ---- AI/ML role detection ---------------------------------------------------
#
# Matched on TITLE TEXT, not `industries` — see the note on the AI/ML row of
# subsector_lookup_table for why. Deliberately broad (bare "ai"/"ml" tokens
# included) to catch "AI Researcher" / "ML Scientist"-style titles that don't
# spell out the full phrase; this lets some free-text noise through (gig-work
# titles like "AI Trainer"), the same tradeoff the job-titles figures already
# accept for LinkedIn's free text. `output/tables/ai_ml_titles_matched.csv`
# (written by write_ai_ml_titles(), R/05_plots.R) lists every matched string
# so it stays auditable rather than trusted, same contract as
# neurotech_companies.csv.
AI_ML_TITLE_RE <- paste0(
  "artificial intelligence|machine learning|\\bml\\b|deep learning|",
  "\\bllm\\b|large language model|\\bai\\b"
)

is_ai_ml_title <- function(title) {
  stringr::str_detect(stringr::str_to_lower(dplyr::coalesce(title, "")),
                      AI_ML_TITLE_RE)
}

# ---- subsector -> sector ---------------------------------------------------
#
# An exhaustive LOOKUP, not a regex. A subsector missing from this table
# collapses to NA and trips the check in collapse_to_sector(), which is the
# point: a new subsector must be assigned a sector deliberately rather than
# defaulting into "Industry" and quietly inflating the headline number.
SUBSECTOR_TO_SECTOR <- c(
  "Academia — postdoc"                = "Academia",
  "Academia — faculty"                = "Academia",
  "Academia — staff scientist"        = "Academia",
  "Academia — clinical trainee"       = "Academia",
  "Government — intramural research"  = "Government",
  "Government/Nonprofit"              = "Government",
  "Defense"                           = "Government",
  "Neurotech (specific)"              = "Industry",
  "Pharma/Biotech"                    = "Industry",
  "Medical Devices"                   = "Industry",
  "Healthcare delivery"               = "Industry",
  "AI/ML"                             = "Industry",
  "Software/Tech (general)"           = "Industry",
  "Consulting"                        = "Industry",
  "Finance"                           = "Industry",
  "Manufacturing/Industrial"          = "Industry",
  "Education (non-university)"        = "Other",
  "Other"                             = "Other"
)

# Defense sits under Government rather than Industry: the subsector is matched
# off LinkedIn industries like "Defense and Space Manufacturing", which in this
# corpus is dominated by service labs and FFRDCs rather than by defence
# contractors. If that stops being true, move it here and nowhere else.
#
# Healthcare delivery sits under Industry, NOT Academia, even though clinical
# TRAINING is academic. The distinction is the title: a residency is caught by
# classify_academic_title() and lands in "Academia — clinical trainee"; a
# practising clinician at a health system is industry employment.
collapse_to_sector <- function(subsector) {
  out <- unname(SUBSECTOR_TO_SECTOR[subsector])
  unmapped <- unique(subsector[!is.na(subsector) & is.na(out)])
  if (length(unmapped)) {
    warning("subsector(s) with no sector mapping, collapsing to NA: ",
            paste(unmapped, collapse = ", "), call. = FALSE)
  }
  out
}
