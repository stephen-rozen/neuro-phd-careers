# Methods

This document explains the definitions and judgment calls behind the
pipeline: what was chosen, why, and what it costs. The code is the authority;
this is the reasoning. Section names match the cross-references in the code
comments.

The project is in a preliminary state. Descriptive counts below are given
to show the scale of each step; no career-outcome results are reported here.

---

## Analytic sample

Collection is deliberately wide and inclusion is decided afterwards. Roughly
95,000 profiles with any neuroscience or neighbouring-field signal were
collected, and inclusion then sorts them into evidence tiers
(`R/12_inclusion.R::build_inclusion()`):

- **Tier A (counted):** neuroscience appears in the person's doctoral education
  record, or their matched dissertation is classified as neuroscience by
  ProQuest's subject taxonomy.
- **Tier B (held):** weaker or indirect evidence. Examples are a neuroscience
  self-description in a headline with nothing in the education record,
  neuroscience only in job history, or a feeder field (biomedical engineering,
  physiology, cognitive psychology, pharmacology, and similar). Tier B people
  count only if a dissertation record corroborates them, which moves them to
  Tier A; otherwise they are excluded.
- **Tier C (excluded):** no neuroscience signal, or not yet a PhD.

The analytic sample ("Arm V") is **Tier A people who are also matched to a
ProQuest dissertation**, roughly 8,000 people. Requiring the dissertation
match means that both "this is the right person" and "their PhD was in
neuroscience" are checked against a record that does not come from the
profile.

What this costs:

- Tier A people whose dissertation could not be found are left out, even when
  there is no particular reason to doubt them.
- The sample inherits the coverage of the ProQuest export. It over-represents
  whichever schools and years that corpus covers well, and is overwhelmingly
  US in practice because the corpus is. That US skew comes from corpus
  coverage; the sample definition has no country condition, so it should not
  be described as a US sample by design.

Nothing is deleted. Excluded tiers stay in the stores, and any group can be
brought back by changing one predicate.

### Why NeuroTree membership is not Tier A

NeuroTree records who trained with whom, and it tags people with research
areas. That looked like strong evidence of a neuroscience PhD, but area tags
propagate down advisor lineages: an advisor listed under both neuroscience and
another field passes the neuroscience tag to trainees who worked entirely in
the other field. When measured, about 96% of the people a "NeuroTree member =
Tier A" rule would have added had no independent neuroscience evidence. So
NeuroTree counts toward Tier A only when it leads to a dissertation that
ProQuest's taxonomy classifies as neuroscience.

## Record linkage

Matching profiles to dissertations (`R/07_proquest.R`) and to NeuroTree
(`R/18_neurotree.R`) uses the same rules, chosen for precision over recall:

- **Name key.** Both sides are reduced to first token + last token, so a
  middle name present on only one side never blocks a match.
- **Surnames must match exactly.** First names may differ within a
  Jaro-Winkler distance of 0.12 (to allow nicknames) only if first initials
  agree. Surnames are never fuzzy-matched, so a near-miss like Chen/Chan
  cannot produce a false link.
- **Institution is a hard blocking key.** Candidates must share a normalized
  institution. Institution strings are normalized by hand-curated rules and
  supplemented by a crosswalk to the Research Organization Registry (ROR;
  `R/20_ror.R`). The crosswalk can merge spellings of the same institution
  but never splits groups the normalizer already joins.
- **Middle-initial disconfirmation.** When both sides give a middle name and
  the initials disagree, the match is rejected.
- **Year window.** Dissertation year must be consistent with the education
  dates on the profile.

Every accepted fuzzy match is written to a review table for hand audit.

**Known limitation.** Exact surname matching misses anyone whose surname
changed between the dissertation and the profile. That is not random: it
falls disproportionately on women who changed their name at marriage. It is a
selection effect that the sample inherits and that will be stated with any
result.

NeuroTree is also linked to ProQuest directly on normalized dissertation
title, which gives a second route from a person to their dissertation.

## PhD year and cohorts

Where a dissertation is matched, its awarded-degree year is used. NeuroTree's
graduation year fills remaining gaps, and the profile-derived estimate is the
last resort. On people with both, the ProQuest and profile years agreed
exactly about 79% of the time and always within one year. The profile
estimate is the one that makes outright errors (it dates people still
enrolled to a completed degree), so the record is preferred.

Years after 2025 are excluded: a "2026 PhD" is almost always a degree still in
progress. Cohorts are fixed five-year bins defined in one place
(`R/06_figure_settings.R`), so figures stay comparable across rebuilds.

## Date parsing

Profile dates arrive as "Mar 2024" or as a bare year when the member left
the month blank. Bare years are anchored to January on a start date and
December on an end date, and every date carries a precision flag (`month`,
`year`, `current`). That way an assumed month can always be told apart from
a real one (`R/01_parse.R::.parse_li_date()`).

## Date-less profiles

A large share of profiles have no dated job history at all. These are
public-view payloads: the top job card is shown in full and every later entry
is masked. A late-2025 platform restriction on public profile fields is the
main cause, and re-fetching the same profiles through two different access
routes recovered no dates.

The pipeline treats this as missing data, not as evidence:

- People with no dated history are excluded from the risk set of every
  time-to-event analysis. Censoring them would read "no data" as "stayed in
  academia."
- Their top job card is used for a separate *opt-in* "current position"
  column. It is not used for the default one, for the reason given under
  [Current position](#current-position).

## Sector classification

Each job is assigned a **subsector** (Pharma/Biotech, Medical devices,
Software/Tech, Consulting, Government intramural research, Academia
postdoc / faculty / staff scientist / clinical trainee, and so on). The
four-level **sector** (Academia / Government / Industry / Other) is derived
from the subsector through an exhaustive lookup table (`R/sector_map.R`). A
subsector with no mapping produces a warning and NA rather than silently
defaulting into Industry.

Main judgment calls:

- **Government intramural research** (NIH, NSF, FDA, VA, national labs) is
  matched on the employer *name*. Their industry label ("Research Services")
  cannot distinguish them from university labs.
- **Clinical residency is Academia**, not an industry exit. It is supervised
  training, the medical equivalent of a postdoc. A practising clinician
  remains in healthcare delivery.
- **Job titles override industry** when the title itself names the sector
  ("Postdoctoral Fellow" at a hospital is an academic postdoc). A generic
  title like "Research Scientist" does not override a pharma employer.
- **Unknown is not "Other."** A job whose employer could not be identified is
  NA. "Other" means only a known industry outside the taxonomy.
- **Neurotech and AI/ML** are identified from employer name and job title
  respectively, because the platform's industry vocabulary has no neurotech
  category and almost never uses its AI category.

As a cross-check, every classification is also mapped to the Unified Career
Outcomes Taxonomy (UCOT-Exp2; Stayart et al. 2020, *F1000Research* 9:8,
doi:10.12688/f1000research.21641.1) in `R/22_ucot.R`. The UCOT version is
produced as a parallel set of figures rather than replacing the project
taxonomy, because the two group some roles differently.

## Employer-name overrides

The link from a job to an employer record is sometimes wrong (unrelated
employers aliased onto one record) and often missing (small employers,
universities with thin company pages). Two name-based rules address this
(`R/04_link.R`):

- **Join override:** if the employer name is unambiguously academic but the
  linked record says otherwise, the name wins. Applied by default.
- **Name rescue:** if no employer record was linked, an academic-sounding
  name assigns an academic subsector. This is **off by default** and kept in
  separate `*_incl_name_rescue` columns. It recovers only academic
  missingness, so turning it on shifts the sector shares toward academia, and
  that shift comes from the rule rather than from the data. A symmetric rule
  that also recognizes industry employers by name is needed first.

## Current position

The current job is the most recently started job the person marks as
current. If none is marked, it falls back to the most recently started dated
job (`R/04_link.R::current_job_rows()`). The rule that fired is recorded per
person.

The default current-sector columns use dated jobs only. Adding the top job
card of date-less profiles would raise coverage considerably. But those cards
carry almost no title information, so the academic-title rules rarely fire on
them, and including them moves the sector shares by double digits. The
dated-only definition agrees with the NSF benchmark (see
[Validation](#validation)); the mixed one does not. The public-card version
is kept as an opt-in column for sensitivity analysis.

## First post-PhD position

The first position is the earliest job starting in or after the PhD year.
Postdocs count: a postdoc is a job, and the pipeline does not try to look
past it to a "real" first job. Time to first industry job is measured in
months to the first industry job anywhere in the sequence, so a
postdoc-then-industry path counts as an industry transition.

## Survival analyses

Two corrections matter here:

- **Risk set.** Only people with an observable dated employment history are
  at risk. Including date-less profiles as censored observations biased the
  industry-transition curve downward.
- **Length bias in postdoc duration.** Among recent cohorts, only short
  postdocs have had time to finish, so a median over completed postdocs
  manufactures an apparent collapse in postdoc length. The pipeline uses
  Kaplan-Meier estimates with ongoing postdocs censored and reports a cohort
  median only when at least half of its postdocs have an observed end.

## Program prestige

Programs are ranked using research.com's world neuroscience ranking, with
US News as a secondary source (`R/22_rankings.R`). An institution with no
ranking is NA, not imputed to the bottom. Institutions whose normalized names
collide (for example, two different Boston schools) are disambiguated
explicitly or dropped; one school is never given another school's rank.

## Publications and NIH funding

Both come from NeuroTree exports and are covariates only. They never affect
who is included, since that would make the sample depend on an outcome.

- **Publications** (`R/23_pubs.R`) come from automated author matching.
  Candidate rows are accepted above a score threshold, which is reported as
  unvalidated: the human review labels in the export are written back into
  the score itself, so they cannot be used to calibrate it. Citation counts
  are partial and are treated as lower bounds.
- **NIH grants** (`R/24_grants.R`) come from a name-and-location match to
  NIH award records. The threshold is calibrated against the export's human
  review labels, which are not a random sample, so the resulting precision is
  stated as precision on reviewed rows. Budget periods are collapsed to
  distinct projects.
- Both features are three-state: a person with no NeuroTree link has
  *unknown* funding and publications, not zero.

## Country labels

PhD-institution country comes from the ROR crosswalk and is a label, not a
filter. It takes three values: US, non-US, and unknown when ROR could not
place the institution. A group of spellings gets a country only when one
country holds at least 80% of its resolved weight. Known errors remain for
US institutions that have an overseas branch or a foreign namesake. The
aggregate is reliable, but individual non-US labels should not be trusted.
Puerto Rico is treated as US.

## Dissertation subfield

Subfield labels come from ProQuest subject terms through a fixed mapping
(`R/08_classify.R::subfield_from_subject_terms()`). An earlier bag-of-words
classifier agreed with the subject terms barely above chance and was
removed.

A second, experimental pair of labels (content area and primary methodology,
four categories each) is produced by a large language model (Anthropic's
Claude Haiku via its API) from dissertation titles and keywords. This runs
once, by hand. Results are cached and the pipeline reads only the cache, so
rebuilds are deterministic and make no network calls. These labels are
exploratory and are not yet validated against hand coding.

## Missing data in figures

NA is never plotted as a category. Every outcome figure reports its effective
N in the caption, so readers can see how much was excluded.

## Validation

Current-sector shares are compared with NSF's Survey of Doctorate Recipients,
which reports sector of employment for neuroscience doctorates. This checks
the aggregate sector split only; it cannot validate individual
classifications or cohort trends.

A missingness audit checks whether the data can support cohort comparisons.
Missingness depends on cohort, collection batch and employer type. For that
reason, any cohort-trend claim will be reported with an explicit uncertainty
envelope, and a difference inside that envelope will not be treated as a
finding.

## Limitations

- Every career statistic is conditional on having a usable public employment
  history, which roughly half the verified sample has.
- The sample frame is set by ProQuest's corpus coverage.
- Exact surname matching under-samples people whose surname changed.
- Sector is inferred from employer metadata and job titles, not reported by
  the person.
- Collection batch and PhD cohort overlap, which limits cohort comparisons.
- NeuroTree-derived features (grants, publications) are available for fewer
  than half of the sample, and that coverage itself varies by cohort.
