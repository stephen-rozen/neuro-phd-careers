# R/07_proquest.R — ProQuest dissertation linkage
#
# Match ProQuest dissertation records back to LinkedIn people on
# name + PhD year + institution. Reuses the fuzzy-match pattern from
# R/04_link.R (Jaro-Winkler via fuzzyjoin).

# ---- TDM Studio data reader ----

# Reverse-map: subject label (lowercased) → 4-digit ProQuest code, applied to the
# dissertation's PRIMARY (first) subject label only. A convenience/reference
# column — neuro classification keys on the label sets below, not on this code,
# since the TDM export ships labels with no numeric codes and most subjects have
# no entry here (→ NA).
PQ_LABEL_TO_CODE <- c(
  "neurosciences"              = "0317",
  "neurology"                  = "0317",
  "neurobiology"               = "0317",
  "physiological psychology"   = "0989",
  "psychobiology"              = "0349",
  "cognitive psychology"       = "0633",
  "biomedical engineering"     = "0541",
  "engineering, biomedical"    = "0541",
  "bioengineering"             = "0202",
  "molecular biology"          = "0307",
  "physiology"                 = "0719",
  "anatomy & physiology"       = "0719",
  "animal physiology"          = "0719",
  "pharmacology"               = "0419"
)

parse_pq_list <- function(x) {
  x <- dplyr::coalesce(x, "[]")
  x <- stringr::str_replace(x, "^\\['?", "")
  x <- stringr::str_replace(x, "'?\\]$", "")
  stringr::str_split(x, "',\\s*'")
}

# Code from the PRIMARY (first) subject label only — no more "first recognized
# anywhere", which over-attributed feeders (e.g. ['History','Molecular biology']
# → 0307). NA when the primary label isn't in PQ_LABEL_TO_CODE.
.map_subject_code <- function(subject_terms_raw) {
  primary <- stringr::str_to_lower(stringr::str_squish(
    .first_subject_label(subject_terms_raw)))
  unname(PQ_LABEL_TO_CODE[primary])
}

# All subject labels collapsed, for downstream "filter later" on true subject.
.collapse_pq_list <- function(x_raw) {
  purrr::map_chr(parse_pq_list(x_raw), function(v) {
    v <- stringr::str_squish(v)
    paste(v[v != ""], collapse = "; ")
  })
}

.first_subject_label <- function(subject_terms_raw) {
  terms_list <- parse_pq_list(subject_terms_raw)
  purrr::map_chr(terms_list, function(terms) {
    t <- stringr::str_squish(terms)
    t <- t[t != ""]
    if (length(t) > 0) t[[1]] else NA_character_
  })
}

# First author verbatim (with its "Last, First Middle" comma intact) — the raw
# form the name key needs. .parse_pq_author reuses it for the display name.
.first_author_raw <- function(authors_raw) {
  purrr::map_chr(parse_pq_list(authors_raw), function(a) {
    a <- stringr::str_squish(a)
    a <- a[a != ""]
    if (length(a) > 0) a[[1]] else NA_character_
  })
}

.parse_pq_author <- function(authors_raw) {
  first_author <- .first_author_raw(authors_raw)
  parts <- stringr::str_split_fixed(first_author, ",\\s*", 2)
  last <- parts[, 1]
  first <- parts[, 2]
  dplyr::if_else(first == "" | is.na(first), last, paste(first, last))
}

# Durable ProQuest document URL from the GOID (no URL column ships in the TDM
# export). docview/{GOID} is the stable PQDT permalink form.
pq_url <- function(goid) {
  g <- as.character(goid)
  dplyr::if_else(is.na(g) | g == "", NA_character_,
                 paste0("https://www.proquest.com/docview/", g))
}

# The TDM export ships no abstract field. `data/armv_abstracts.csv` is a
# hand-run supplement: abstracts pulled from the raw XML
# inside TDM Studio's Jupyter environment (the export ships metadata only,
# abstract text lives in the per-document XML's `AbsText` node) and scoped to
# the ~8.1k Arm V GOIDs only, not the full ~339k corpus. GOID-keyed, two
# columns (`GOID`, `Abstract`). Stub-safe: absent file -> empty tibble, so
# `abstract` stays NA_character_ for everyone, exactly like before this file
# existed.
read_armv_abstracts <- function(path = "data/armv_abstracts.csv") {
  if (!fs::file_exists(path)) {
    return(tibble::tibble(goid = character(), abstract = character()))
  }
  readr::read_csv(path, show_col_types = FALSE) |>
    janitor::clean_names() |>
    dplyr::transmute(goid = as.character(goid), abstract = abstract)
}

.parse_pq_keywords <- function(kw_raw) {
  purrr::map_chr(parse_pq_list(kw_raw), function(k) {
    k <- stringr::str_squish(k)
    k <- k[k != ""]
    paste(k, collapse = "; ")
  })
}

.clean_pq_institution <- function(x) {
  x <- dplyr::coalesce(x, "")
  stringr::str_replace(x, "\\s*\\(.*\\)$", "")
}

DOC_REGEX <- paste0(
  "(?i)(Ph\\.?D|Doctor|DPhil|D\\.Phil|Psy\\.?D|Ed\\.?D|Sc\\.?D",
  "|^Dr\\.|Fil\\.dr|Tekn\\.dr|^D\\.[A-Z])"
)

fetch_proquest <- function(path = "data/extended.csv/extended.csv",
                           abstracts_path = "data/armv_abstracts.csv") {
  raw <- readr::read_csv(path, show_col_types = FALSE) |>
    janitor::clean_names()

  raw <- raw |>
    dplyr::filter(
      stringr::str_detect(degree_type, DOC_REGEX) |
        stringr::str_detect(degree, DOC_REGEX)
    )

  abstracts <- read_armv_abstracts(abstracts_path)

  raw |>
    dplyr::transmute(
      pq_id        = as.character(goid),
      goid         = as.character(goid),
      author_name  = .parse_pq_author(authors),
      author_lastfirst = .first_author_raw(authors),
      pq_url       = pq_url(goid),
      title        = title,
      institution  = .clean_pq_institution(school_name),
      degree_year  = as.integer(degree_date),
      subject_code = .map_subject_code(subject_terms),
      subject      = .first_subject_label(subject_terms),
      subject_all  = .collapse_pq_list(subject_terms),
      keywords     = .parse_pq_keywords(paper_keywords),
      department   = department
    ) |>
    dplyr::left_join(abstracts, by = "goid") |>
    dplyr::select(-goid)
}

# ---- name / institution normalization ----

norm_name <- function(x) {
  x <- stringr::str_to_lower(dplyr::coalesce(x, ""))
  x <- stringi::stri_trans_general(x, "Latin-ASCII")  # fold accents: müller -> muller
  x <- stringr::str_replace_all(x, "\\b(jr|sr|ii|iii|iv)\\b\\.?", "")
  x <- stringr::str_replace_all(x, "[[:punct:]]", " ")
  stringr::str_squish(x)
}

# Whole-string acronym expansions, applied AFTER lowercasing / punctuation->space
# but BEFORE stopword removal + tokenization (so the expanded phrase passes through
# the same stopword/token-set pipeline as a spelled-out name and lands identically).
# Keys are matched against the WHOLE squished string; expansions need not be
# pre-cleaned (their generic tokens — "institute", "of" — are dropped downstream).
INST_ACRONYMS <- c(
  mit = "massachusetts institute technology", caltech = "california institute technology",
  ucla = "california los angeles", ucsf = "california san francisco",
  ucsd = "california san diego", ucsb = "california santa barbara",
  ucdavis = "california davis", ucirvine = "california irvine",
  berkeley = "california berkeley",
  nyu = "new york", usc = "southern california",
  unc = "north carolina chapel hill",   # flagship; corpus keys the full campus form
  cmu = "carnegie mellon", upenn = "pennsylvania", penn = "pennsylvania",
  ucl = "london", rpi = "rensselaer polytechnic", uiuc = "illinois urbana champaign",
  gatech = "georgia technology", osu = "ohio state", psu = "pennsylvania state",
  asu = "arizona state", msu = "michigan state", umich = "michigan",
  umn = "minnesota", utexas = "texas austin", tamu = "texas a m",
  jhu = "johns hopkins", wustl = "washington st louis", eth = "eth zurich",
  lsu = "louisiana state agricultural mechanical"
)

# Generic organizational tokens carrying no disambiguating signal. NOTE: "state"
# is deliberately NOT here (it distinguishes "Ohio State" from "University of
# Ohio"); single letters are kept ("Texas A M"). Multilingual variants cover the
# heavily-international corpus.
INST_STOPWORDS <- c(
  "university", "univ", "universidad", "universidade", "universite",
  "universitat", "universitaet", "universita", "college", "institute",
  "institut", "instituto", "school", "department", "dept", "graduate",
  "grad", "faculty", "program", "programme", "the", "of", "at", "for",
  "and", "in", "campus"
)

# Academic sub-unit phrases. LinkedIn over-specifies the degree-granting unit
# ("<University> School of Medicine") where ProQuest lists only the parent
# institution, so the surviving discipline token ("medicine") breaks the exact
# token-set blocking key ("medicine temple" != "temple"). We strip the MATCHED
# PHRASE ONLY (not to end-of-string), so a trailing geographic disambiguator
# survives ("...School of Medicine in St. Louis" -> "...washington ... st louis",
# keeping WUSTL distinct from U. of Washington). Standalone independent schools
# whose name has no parent prefix ("Oregon Health & Science University", "Medical
# University of South Carolina", "Medical College of Wisconsin") don't match
# these trailing phrases and are preserved. A bare "X College of Medicine" /
# "X Medical School" whose stem collides with a separate university (e.g. Baylor
# College of Medicine) WOULD collapse into its namesake — those are shielded by
# INST_PROTECTED below.
INST_SUBUNIT_RE <- paste0(
  "\\b(",
  "(graduate )?school of medicine( and dentistry)?( and health sciences)?",
  "|college of medicine( and life sciences)?|medical school",
  "|(graduate )?school of (dental medicine|dentistry|nursing|pharmacy|",
    "public health|veterinary medicine|osteopathic medicine|optometry|",
    "podiatric medicine|nutrition|biomedical sciences|graduate studies|",
    "health professions|allied health professions|allied health|",
    "medicine and dentistry)",
  "|graduate school of biomedical sciences",
  ")\\b"
)

# Independent degree-granting schools whose name stem collides with a SEPARATE
# university. Matched against the cleaned string BEFORE INST_SUBUNIT_RE and mapped
# to a sentinel single token, so the sub-unit strip can't collapse them into the
# namesake (e.g. Baylor College of Medicine -> "baylorcom", kept distinct from
# Baylor University -> "baylor"). Applied symmetrically on both the LinkedIn and
# ProQuest sides, so a protected school still matches its OWN dissertations. The
# sentinel must be a single bare token (no spaces/stopwords) so it survives the
# downstream pipeline unchanged. Omissions are safe: an unlisted school simply
# degrades to the plain sub-unit strip.
INST_PROTECTED <- c(
  "baylor college of medicine"   = "baylorcom",
  "morehouse school of medicine" = "morehousesom"
)

# Final-key aliases, applied AFTER tokenization + sort on BOTH sides. Each key
# and value must be a sorted, deduped token string (the blocking key format).
# Curated from the empirical orphan scan (LinkedIn keys with zero corpus
# counterpart, or corpus long-forms LinkedIn never writes): campus/branding
# qualifiers defeat exact token-set equality ("bloomington indiana" can never
# equal "indiana"). Deliberately curated, NOT subset-collapsed — "british
# columbia" ⊃ "columbia" (UBC vs Columbia) and "indiana pennsylvania" (IUP)
# show why automatic containment is unsafe.
INST_TOKENSET_ALIASES <- c(
  "bloomington indiana"                                   = "indiana",
  "cities minnesota twin"                                 = "minnesota",
  "penn state"                                            = "pennsylvania state",
  "city columbia new york"                                = "columbia",
  "center city new york"                                  = "city new york",
  "ann arbor michigan"                                    = "michigan",
  "chapel hill unc"                                       = "carolina chapel hill north",
  # Rutgers: corpus lists the long statutory forms; LinkedIn writes "Rutgers".
  "jersey new rutgers state"                              = "rutgers",
  "brunswick dentistry jersey medicine new rutgers state" = "rutgers",
  "applied jersey new professional psychology rutgers state" = "rutgers",
  "jersey new newark rutgers state"                       = "rutgers",
  # Rutgers campus brands as LinkedIn writes them ("Rutgers University - Newark").
  "newark rutgers"                                        = "rutgers",
  "brunswick new rutgers"                                 = "rutgers",
  # CUNY / NYU-Grossman / Virginia Tech: LinkedIn brand vs corpus long form.
  "cuny"                                                  = "city new york",
  "grossman nyu"                                          = "new york",
  "tech virginia"                                         = "polytechnic state virginia",
  # CSPP/Alliant: corpus splits by campus; LinkedIn writes the generic name.
  "alameda berkeley california professional psychology"   = "california professional psychology",
  "angeles california los professional psychology"        = "california professional psychology",
  "california diego professional psychology san"          = "california professional psychology",
  "california fresno professional psychology"             = "california professional psychology"
)

# Canonicalize an institution name to an order-independent token set. The blocking
# key is EXACT equality on this string, so two spellings of the same school must
# reduce to the same sorted, deduped token set. Steps: drop parentheticals (the
# ProQuest "(United Kingdom)" country tags and nested-name noise), shield
# collision-prone schools (INST_PROTECTED), strip academic
# sub-unit phrases (INST_SUBUNIT_RE), punctuation -> space, whole-string acronym
# expansion, UC-system prefix expansion, stopword
# removal, then sort + unique the surviving tokens. Sort makes word order
# irrelevant ("California Berkeley" == "Berkeley California"); unique collapses the
# duplicated tokens in ProQuest's nested strings ("University of London, University
# College London" -> "london").
norm_institution <- function(x) {
  x <- stringr::str_to_lower(dplyr::coalesce(x, ""))
  x <- stringi::stri_trans_general(x, "Latin-ASCII")          # fold accents: montréal -> montreal
  x <- stringr::str_replace_all(x, "\\(.*?\\)", " ")          # drop parentheticals
  x <- stringr::str_replace_all(x, "&", " and ")              # normalize & before phrase match
  x <- stringr::str_squish(x)
  # SUNY campuses go by the campus name ("Stony Brook University"); strip the
  # system prefix so "State University of New York at Stony Brook" keys the
  # same as the campus brand LinkedIn writes.
  x <- stringr::str_replace(x, "^(the )?state university of new york at ", "")
  x <- stringr::str_replace(x, "^suny ", "")
  x <- dplyr::if_else(x %in% names(INST_PROTECTED),
                      unname(INST_PROTECTED[x]), x)            # shield collision-prone schools
  x <- stringr::str_replace_all(x, INST_SUBUNIT_RE, " ")      # drop academic sub-unit phrase
  x <- stringr::str_squish(x)
  # Drop eponymous donor prefix so "David Geffen School of Medicine at UCLA"
  # (after sub-unit strip: "david geffen at ucla") -> "ucla" -> acronym map
  # fires. CONDITIONAL: the prefix must not itself be an institution name —
  # the old unconditional strip reduced every "University of X at Y" to bare
  # "Y", colliding U. Illinois at Chicago with U. Chicago and letting UK
  # Birmingham people block with UAB dissertations.
  donor <- stringr::str_match(x, "^([a-z][a-z ]*?) at [a-z]")[, 2]
  donor_ok <- !is.na(donor) &
    !stringr::str_detect(donor,
      "\\b(universit|college|institute|institut|school|academy|state)")
  x <- dplyr::if_else(donor_ok,
                      stringr::str_replace(x, "^[a-z][a-z ]*? at (?=[a-z])", ""),
                      x)
  x <- stringr::str_replace_all(x, "[[:punct:]]", " ")
  x <- stringr::str_squish(x)
  # Strip an academic-department suffix ("...-Biology Department", "Department of
  # Chemistry"): LinkedIn over-specifies the granting department where ProQuest
  # lists only the parent campus, and the surviving discipline token defeats the
  # exact token-set key ("biology piedras..." != "piedras...").
  x <- stringr::str_replace_all(x,
    "\\b(department of [a-z]+|[a-z]+ department)\\b", " ")
  x <- stringr::str_squish(x)
  x <- dplyr::if_else(x %in% names(INST_ACRONYMS),
                      unname(INST_ACRONYMS[x]), x)            # solid-acronym forms
  x <- stringr::str_replace(x, "^u ?c (?=[a-z])", "california ")  # "uc davis" -> "california davis"
  x <- stringr::str_replace_all(x, "\\bhawai i\\b", "hawaii")     # okina fold: hawai'i -> hawaii
  key <- purrr::map_chr(stringr::str_split(x, " "), function(t) {
    t <- t[t != "" & !(t %in% INST_STOPWORDS)]
    paste(sort(unique(t)), collapse = " ")
  })
  dplyr::if_else(key %in% names(INST_TOKENSET_ALIASES),
                 unname(INST_TOKENSET_ALIASES[key]), key)     # campus-qualifier aliases
}

# ---- structured name parsing (first + last "name key") ----
#
# Whole-string name matching let a trailing middle name push true matches over
# the Jaro-Winkler threshold ("Maria Lopez" vs ProQuest "Lopez, Maria Elena";
# "Susan Hart" vs "Hart, Susan Ann Marie"). We match on a first-token +
# last-token KEY instead, so middle names never change the comparison. The full
# normalized name (name_norm) is retained as a tie-breaker and for diagnostics.
# Fuzzy stage matching is asymmetric: first-token FUZZY (JW <= NAME_MAX),
# last-token EXACT — surnames are never fuzzy-matched.

NAME_SUFFIXES_RE <- "\\b(jr|sr|ii|iii|iv|v)\\b\\.?"

# Professional-credential tokens stripped from LinkedIn display names (e.g.
# "Jane Doe, PT, MSPT, PhD, GCS, CEEAA"). Most arrive after a comma and are
# dropped wholesale; this set catches the comma-less stragglers.
CREDENTIAL_TOKENS <- c(
  "phd","md","ms","msc","msn","mph","mba","mfa","ba","bs","bsc","rn","pt","ot",
  "dpt","do","dvm","jd","edd","psyd","mspt","gcs","ceeaa","faan","facs","frcp",
  "pmp","esq","cfa","cpa","dds","dmd","od","pharmd","np","lcsw","msw","mha",
  "scd","dsc","bsn","aprn","mscs","msph"
)

.norm_tokstr <- function(s) {
  s <- stringr::str_to_lower(dplyr::coalesce(s, ""))
  s <- stringi::stri_trans_general(s, "Latin-ASCII")  # fold accents: müller -> muller
  s <- stringr::str_replace_all(s, NAME_SUFFIXES_RE, " ")
  s <- stringr::str_replace_all(s, "[[:punct:]]", " ")
  stringr::str_squish(s)
}

.drop_credentials <- function(s) {
  purrr::map_chr(stringr::str_split(s, " "), function(t) {
    t <- t[t != "" & !(t %in% CREDENTIAL_TOKENS)]
    paste(t, collapse = " ")
  })
}

.first_tok <- function(s) dplyr::coalesce(stringr::str_extract(s, "^\\S+"), "")
.last_tok  <- function(s) dplyr::coalesce(stringr::str_extract(s, "\\S+$"), "")

# LinkedIn person name → first / last token. Prefer the native first_name/
# last_name columns; fall back to parsing `name` when those are blank. Surnames
# reduce to their LAST token so multi-word/hyphenated surnames key the same on
# both sides.
#
# Every field goes through .li_clean, which drops the post-comma credential block
# BEFORE normalizing — BrightData often dumps the whole display string (incl.
# ", Ph.D.") into first_name AND last_name, and the punctuation pass would split
# "Ph.D." into "ph"/"d" (escaping CREDENTIAL_TOKENS) and leave a bare "d" as the
# trailing token (e.g. "Jane Q. Doe, Ph.D." -> last_key "d"). Cutting at the
# first comma removes the credential block wholesale; a real surname never
# contains a comma, so this is safe for both the native columns and `name`.
.li_clean <- function(x) {
  .drop_credentials(.norm_tokstr(
    stringr::str_split_fixed(dplyr::coalesce(x, ""), ",", 2)[, 1]))
}
li_first <- function(first_name, name) {
  f <- .li_clean(first_name)
  .first_tok(dplyr::if_else(f == "", .li_clean(name), f))
}
li_last <- function(last_name, name) {
  l <- .li_clean(last_name)
  .last_tok(dplyr::if_else(l == "", .li_clean(name), l))
}

# Recover a surname from the LinkedIn URL slug when the display last name is an
# INITIAL ("Jane E."). LinkedIn often abbreviates the surname in `name`/`last_name`
# but its public URL still carries it (`/in/janeexample-phd` -> "example"). Without this, an
# initialized surname reduces to a single letter and can never clear the matcher's
# exact-surname gate, so a real dissertation is missed. Returns "" when the slug
# yields no confident surname (e.g. a display first name that is itself an
# abbreviation, so it can't anchor the split) — we decline rather than guess.
.surname_from_url <- function(url, first_key) {
  slug <- stringr::str_match(stringr::str_to_lower(dplyr::coalesce(url, "")),
                             "/in/([^/?#]+)")[, 2]
  purrr::map2_chr(slug, first_key, function(s, fk) {
    if (is.na(s) || s == "") return("")
    parts <- stringr::str_split(s, "-")[[1]]
    # Drop credential blocks and hash tokens (any digit, or single char).
    parts <- parts[parts != "" & !(parts %in% CREDENTIAL_TOKENS) &
                     !stringr::str_detect(parts, "[0-9]") & nchar(parts) > 1]
    if (length(parts) == 0) return("")
    cand <- if (length(parts) == 1) {
      one <- parts[[1]]
      # Concatenated "firstlast" slug: strip the known first name prefix.
      if (fk != "" && startsWith(one, fk) && nchar(one) > nchar(fk)) {
        substring(one, nchar(fk) + 1)
      } else NA_character_
    } else {
      # Hyphenated slug: the trailing name token is the surname (unless it is
      # itself the first name).
      last_part <- parts[[length(parts)]]
      if (last_part != fk) last_part else NA_character_
    }
    if (is.na(cand) || nchar(cand) < 2 || !stringr::str_detect(cand, "^[a-z]+$")) {
      return("")
    }
    .last_tok(cand)
  })
}
li_name_key <- function(first_name, last_name, name) {
  stringr::str_squish(paste(li_first(first_name, name),
                            li_last(last_name, name)))
}

# ProQuest first-author "Last, First Middle" (raw, comma intact) → first / last.
pq_first <- function(author_lastfirst) {
  parts    <- stringr::str_split_fixed(dplyr::coalesce(author_lastfirst, ""), ",", 2)
  surname  <- .norm_tokstr(parts[, 1])
  given    <- .norm_tokstr(parts[, 2])
  no_comma <- parts[, 2] == ""   # rare; treat the whole as "first last"
  dplyr::if_else(no_comma, .first_tok(surname), .first_tok(given))
}
pq_last <- function(author_lastfirst) {
  .last_tok(.norm_tokstr(
    stringr::str_split_fixed(dplyr::coalesce(author_lastfirst, ""), ",", 2)[, 1]))
}
pq_name_key <- function(author_lastfirst) {
  stringr::str_squish(paste(pq_first(author_lastfirst), pq_last(author_lastfirst)))
}

# ---- middle-name disconfirmation ----
#
# A middle name is used ONLY as negative evidence: when BOTH sides assert a middle
# name and their initials are disjoint, the match is rejected ("Christine
# Marie Doe" vs "Christina Anne Doe"; "Mia (Jacqueline) Roe" vs "Mi Eun Roe").
# Absence on either side is neutral — this preserves the existing tolerance for
# a middle name present on only one side ("Maria Lopez" vs "Lopez, Maria
# Elena"). .norm_tokstr already turns "(Jacqueline)" into a bare middle token.

# LinkedIn display name -> middle token(s) (everything between first and last).
.name_middle_key <- function(name) {
  s <- .li_clean(name)
  purrr::map_chr(stringr::str_split(s, " "), function(t) {
    t <- t[t != ""]
    if (length(t) >= 3) paste(t[2:(length(t) - 1)], collapse = " ") else ""
  })
}

# ProQuest "Last, First Middle" -> middle token(s) of the given-name part.
pq_middle_key <- function(author_lastfirst) {
  parts <- stringr::str_split_fixed(dplyr::coalesce(author_lastfirst, ""), ",", 2)
  given <- .norm_tokstr(parts[, 2])
  no_comma <- parts[, 2] == ""
  purrr::map2_chr(given, no_comma, function(g, nc) {
    if (nc) return("")
    t <- stringr::str_split(g, " ")[[1]]
    t <- t[t != ""]
    if (length(t) >= 2) paste(t[-1], collapse = " ") else ""
  })
}

# Middle token string -> concatenated first initials (e.g. "aagard robison" -> "ar").
.mid_inits <- function(middle_key) {
  purrr::map_chr(stringr::str_split(dplyr::coalesce(middle_key, ""), " "),
    function(t) {
      t <- t[t != ""]
      if (length(t) == 0) "" else paste(substr(t, 1, 1), collapse = "")
    })
}

# Scalar conflict test on two initial strings: both present AND no shared initial.
.middle_conflict_scalar <- function(a, b) {
  a != "" && b != "" &&
    !any(strsplit(a, "")[[1]] %in% strsplit(b, "")[[1]])
}

# ---- field / subject corroboration ----
#
# The institution-only fuzzy path (no year) accepts a fuzzy first name; require the
# LinkedIn stated PhD field to be consistent with the dissertation's subject terms
# so a cross-discipline namesake ("molecular biology" person vs a Neurosciences
# dissertation) can't match. Consistency = shared discipline FAMILY or a shared
# content token. Reuses the neuro/feeder subject vocabulary plus generic families.

# A function (not a top-level list) so it can reference NEURO_SUBJECT_LABEL_PATTERNS,
# which is defined further down this file — resolved at call time, not source time.
.field_family_patterns <- function() {
  list(
    neuro        = c(NEURO_SUBJECT_LABEL_PATTERNS, "neural", "neuro"),
    cog_psych    = "cognitive psycholog",
    psychology   = "psycholog",
    molec_bio    = "molecular biolog",
    cell_bio     = c("cell biolog", "cellular biolog"),
    biomed_eng   = c("biomedical engineer", "engineering, biomedical", "bioengineer"),
    physiology   = "physiolog",
    pharmacology = "pharmacolog",
    biophysics   = "biophysic",
    genetics     = "genetic",
    biochem      = "biochem",
    immunology   = "immunolog",
    biology      = "biolog"
  )
}

FIELD_STOPWORDS <- c(
  "science", "sciences", "studies", "study", "program", "programme", "general",
  "doctor", "doctorate", "doctoral", "philosophy", "graduate", "department",
  "dept", "major", "concentration", "specialization", "with", "from"
)

# Content tokens of a field/subject string: >= 4 chars, non-generic, deduped.
.field_norm_tokens <- function(x) {
  s <- stringr::str_to_lower(dplyr::coalesce(x, ""))
  s <- stringi::stri_trans_general(s, "Latin-ASCII")
  s <- stringr::str_replace_all(s, "[[:punct:]]", " ")
  s <- stringr::str_squish(s)
  t <- stringr::str_split(s, " ")[[1]]
  unique(t[t != "" & !(t %in% FIELD_STOPWORDS) & nchar(t) >= 4])
}

.field_families_one <- function(text) {
  s <- stringr::str_to_lower(dplyr::coalesce(text, ""))
  pats <- .field_family_patterns()
  fams <- character(0)
  for (fam in names(pats)) {
    if (any(stringr::str_detect(s, pats[[fam]]))) fams <- c(fams, fam)
  }
  fams
}

# Vectorized over pairs. TRUE when the LinkedIn field and the dissertation's
# subjects share a discipline family or a content token. Empty LinkedIn field ->
# FALSE (can't corroborate -> routed to review).
fields_consistent <- function(li_text, pq_subject_all) {
  purrr::map2_lgl(dplyr::coalesce(li_text, ""),
                  dplyr::coalesce(pq_subject_all, ""),
    function(l, p) {
      if (stringr::str_squish(l) == "") return(FALSE)
      if (length(intersect(.field_families_one(l),
                           .field_families_one(p))) > 0) return(TRUE)
      length(intersect(.field_norm_tokens(l), .field_norm_tokens(p))) > 0
    })
}

# ---- matching ----

NAME_MAX <- 0.12   # Jaro-Winkler name threshold (shared across paths)
INST_MAX <- 0.15   # reserved; institution is a blocking key, not fuzzed

# Shared identity-matching component gate, used by both the ProQuest fuzzy
# matcher (below) and the identity check in the profile-acquisition step:
# surname must match EXACTLY (last_ok TRUE); first name may differ ONLY if its
# Jaro-Winkler distance clears `name_max` AND the first initials agree. That
# combination passes true nickname variants (Josh/Joshua) while blocking
# cross-initial nicknames (Bob/Robert -- different initial) and same-surname
# strangers who share only a first initial (Austin/Audrey -- JW 0.44, well
# over threshold). Deliberately does NOT special-case common-prefix nicknames
# like Jeff/Jeffrey (JW 0.14, just over 0.12) -- same threshold everywhere, no
# per-caller drift.
#
# Callers supply `first_dist`/`last_ok`/`init_ok` already shaped for their own
# use (scalar/vector for a one-seed comparison, matrix for stringdistmatrix
# blocking) -- R's elementwise comparison ops work identically either way, so
# one function serves both shapes.
identity_match_ok <- function(first_dist, last_ok, init_ok, name_max = NAME_MAX) {
  last_ok & (first_dist <= name_max) & init_ok
}

# Year-plausibility bound for the institution-only paths (exact_inst /
# fuzzy_inst — people with no derivable phd_year). Those paths match against
# ALL years at the institution, so a historical dissertation can pair with a
# living namesake at the same school (observed: an 1883 degree_year backfilled
# onto a LinkedIn profile). Every matched person is on LinkedIn today, so their
# degree year must be within a living career span. Dissertations with an NA
# degree_year are kept — a missing year is not evidence of implausibility.
# The year-blocked paths (exact_year / fuzzy_year) need no bound: they already
# require |degree_year - phd_year| <= 1 against the person's own year.
PQ_YEAR_MIN <- 1950

# Upper bound too: no living LinkedIn person's dissertation post-dates the
# current year, and a future-dated corpus row (2027/2028 appear in the export)
# matching a present-day person is spurious. NA years are kept.
.plausible_diss_years <- function(diss) {
  cy <- as.integer(format(Sys.Date(), "%Y"))
  dplyr::filter(diss, is.na(degree_year) |
                  (degree_year >= PQ_YEAR_MIN & degree_year <= cy))
}

# Education-year window for the institution-only paths. Those people have no
# derivable phd_year, so a namesake's dissertation from a different decade can
# match on name+institution alone (observed: exact_inst matches whose
# degree_year predates the person's earliest listed education by 30-45 years).
# Where the person lists ANY education year we require the dissertation year to
# fall within [earliest_edu - EDU_WINDOW_BACK, latest_edu + EDU_WINDOW_FWD];
# forward is generous (late / career-change PhDs) but bounded. People with no
# education year at all can't be anchored and pass (still floored by
# PQ_YEAR_MIN). Only ~3% of exact_inst matches carry an education year, so this
# tightens precision where we have signal without denting recall elsewhere.
EDU_WINDOW_BACK <- 3L
EDU_WINDOW_FWD  <- 12L

.within_edu_window <- function(degree_year, edu_min, edu_max) {
  is.na(edu_min) | is.na(edu_max) | is.na(degree_year) |
    (degree_year >= edu_min - EDU_WINDOW_BACK &
       degree_year <= edu_max + EDU_WINDOW_FWD)
}

# Build the person table from ALL collected people. Each person gets the PhD
# education institution where one exists, falling back to their first education
# institution (so the ~8% without a clean PhD row can still match). features
# supplies phd_year where derivable; the rest match institution-only.
#
# `ror_crosswalk` is the optional ROR supplement (R/20_ror.R). It never replaces
# inst_norm — inst_block is coalesce(ror_id, inst_norm), so a NULL or empty
# crosswalk reproduces today's blocking exactly.
prepare_pq_persons <- function(education_long, people, features,
                               ror_crosswalk = NULL) {
  inst_phd <- education_long |>
    dplyr::filter(degree_norm == "phd") |>
    dplyr::group_by(id) |>
    dplyr::summarise(inst_phd = dplyr::first(institution), .groups = "drop")
  inst_any <- education_long |>
    dplyr::group_by(id) |>
    dplyr::summarise(inst_any = dplyr::first(institution), .groups = "drop")

  # Stated PhD field text (field_raw + degree_raw of the PhD row, falling back to
  # the first education row) — the corroboration signal for the field gate on the
  # institution-only fuzzy path. LinkedIn often carries the discipline in the
  # degree string rather than the field, so both are concatenated.
  field_phd <- education_long |>
    dplyr::filter(degree_norm == "phd") |>
    dplyr::group_by(id) |>
    dplyr::summarise(field_phd = dplyr::first(stringr::str_squish(paste(
      dplyr::coalesce(field_raw, ""), dplyr::coalesce(degree_raw, "")))),
      .groups = "drop")
  field_any <- education_long |>
    dplyr::group_by(id) |>
    dplyr::summarise(field_any = dplyr::first(stringr::str_squish(paste(
      dplyr::coalesce(field_raw, ""), dplyr::coalesce(degree_raw, "")))),
      .groups = "drop")

  # Education-year span (all start/end years across a person's education rows),
  # the anchor for .within_edu_window on the institution-only match paths.
  edu_years <- education_long |>
    dplyr::group_by(id) |>
    dplyr::summarise(
      edu_min_year = suppressWarnings(min(c(start_year, end_year), na.rm = TRUE)),
      edu_max_year = suppressWarnings(max(c(start_year, end_year), na.rm = TRUE)),
      .groups = "drop") |>
    dplyr::mutate(
      edu_min_year = dplyr::if_else(is.finite(edu_min_year),
                                    as.integer(edu_min_year), NA_integer_),
      edu_max_year = dplyr::if_else(is.finite(edu_max_year),
                                    as.integer(edu_max_year), NA_integer_))

  people |>
    dplyr::select(id, name, first_name, last_name, url) |>
    dplyr::distinct() |>
    dplyr::left_join(inst_phd, by = "id") |>
    dplyr::left_join(inst_any, by = "id") |>
    dplyr::left_join(field_phd, by = "id") |>
    dplyr::left_join(field_any, by = "id") |>
    dplyr::left_join(features |> dplyr::select(id, phd_year), by = "id") |>
    dplyr::left_join(edu_years, by = "id") |>
    dplyr::mutate(
      phd_institution = dplyr::coalesce(inst_phd, inst_any),
      phd_field_text = dplyr::if_else(
        !is.na(field_phd) & field_phd != "", field_phd,
        dplyr::coalesce(field_any, "")),
      name_norm = norm_name(name),
      first_key = li_first(first_name, name),
      last_key  = li_last(last_name, name),
      middle_key = .name_middle_key(name),
      # Initialized surname ("Alan L." -> "l"): recover the real surname from the
      # LinkedIn URL slug so it can clear the exact-surname match gate.
      .url_last = .surname_from_url(url, first_key),
      last_key  = dplyr::if_else(nchar(last_key) <= 1 & .url_last != "",
                                 .url_last, last_key),
      name_key  = stringr::str_squish(paste(first_key, last_key)),
      inst_norm = norm_institution(phd_institution),
      inst_block = ror_block_key(inst_norm, ror_crosswalk),
      # PhD-institution country, from ROR (the only country source we have).
      # LABEL ONLY — nothing is filtered here. phd_is_us is three-state:
      # NA means ROR could not place the institution, which is not "not US".
      phd_country = ror_country(inst_norm, ror_crosswalk),
      phd_is_us   = ror_is_us(inst_norm, ror_crosswalk)
    ) |>
    dplyr::select(-.url_last) |>
    dplyr::select(id, name, url, phd_institution, phd_field_text, phd_year,
                  edu_min_year, edu_max_year,
                  name_norm, first_key, last_key, middle_key, name_key,
                  inst_norm, inst_block, phd_country, phd_is_us)
}

prepare_pq_diss <- function(dissertations, ror_crosswalk = NULL) {
  dissertations |>
    dplyr::transmute(
      pq_id, author_name, pq_url, title, institution, degree_year, subject_code,
      subject, subject_all,
      name_norm = norm_name(author_name),
      first_key = pq_first(author_lastfirst),
      last_key  = pq_last(author_lastfirst),
      middle_key = pq_middle_key(author_lastfirst),
      name_key  = stringr::str_squish(paste(first_key, last_key)),
      inst_norm = norm_institution(institution),
      inst_block = ror_block_key(inst_norm, ror_crosswalk)
    )
}

# Stage 1: exact (hash-join) fast path. name_dist = 0; cheap; cached as its own
# target. Year people require |degree_year - phd_year| <= 1; no-year people match
# institution-only against ALL dissertations (enables phd_year backfill). Empty
# normalized name/institution are excluded so blanks don't collide.
#
# Blocking is on inst_block, not inst_norm: coalesce(ror_id, inst_norm), so it
# degrades to inst_norm wherever the ROR crosswalk has nothing to say.
pq_match_exact <- function(persons, diss) {
  p_keyed <- persons |> dplyr::filter(name_key != "", inst_block != "")
  d_keyed <- diss    |> dplyr::filter(name_key != "", inst_block != "")

  yr <- p_keyed |> dplyr::filter(!is.na(phd_year)) |>
    dplyr::select(id, phd_year, name_key, inst_block, p_name_norm = name_norm) |>
    dplyr::inner_join(d_keyed |> dplyr::select(pq_id, degree_year, name_key,
                                               inst_block, d_name_norm = name_norm),
                      by = c("name_key", "inst_block"),
                      relationship = "many-to-many") |>
    dplyr::filter(!is.na(degree_year), abs(degree_year - phd_year) <= 1) |>
    dplyr::transmute(pq_id, matched_id = id, match_type = "exact_year",
                     name_dist = 0, inst_dist = 0,
                     year_dist = abs(degree_year - phd_year),
                     tie = stringdist::stringdist(p_name_norm, d_name_norm,
                                                  method = "jw"))

  noyr <- p_keyed |> dplyr::filter(is.na(phd_year)) |>
    dplyr::select(id, name_key, inst_block, p_name_norm = name_norm,
                  edu_min_year, edu_max_year) |>
    dplyr::inner_join(.plausible_diss_years(d_keyed) |>
                        dplyr::select(pq_id, name_key, inst_block, degree_year,
                                      d_name_norm = name_norm),
                      by = c("name_key", "inst_block"),
                      relationship = "many-to-many") |>
    dplyr::filter(.within_edu_window(degree_year, edu_min_year, edu_max_year)) |>
    dplyr::transmute(pq_id, matched_id = id, match_type = "exact_inst",
                     name_dist = 0, inst_dist = 0, year_dist = NA_integer_,
                     tie = stringdist::stringdist(p_name_norm, d_name_norm,
                                                  method = "jw"))

  # name_key matches exactly here; the full-name JW distance (which includes any
  # middle name) breaks ties when several people share a first+last key.
  dplyr::bind_rows(yr, noyr) |>
    dplyr::group_by(pq_id) |>
    dplyr::slice_min(tie + 0.05 * dplyr::coalesce(year_dist, 0L),
                     n = 1, with_ties = FALSE) |>
    dplyr::ungroup() |>
    dplyr::select(-tie)
}

# Stage 2: fuzzy (Jaro-Winkler) on the residual — dissertations not already
# resolved by the exact stage. No neuro-code restriction (it cut work only ~8%
# while losing recall and blocking backfill): match all dissertations.
pq_match_fuzzy <- function(persons, diss, exact) {
  residual <- diss |> dplyr::filter(!(pq_id %in% exact$pq_id))

  year_persons  <- persons |> dplyr::filter(!is.na(phd_year))
  noyear_persons <- persons |> dplyr::filter(is.na(phd_year))

  matched_a <- .match_by_year(year_persons, residual, NAME_MAX, INST_MAX)
  matched_b <- .match_by_institution(noyear_persons, residual, NAME_MAX, INST_MAX)

  dplyr::bind_rows(matched_a, matched_b) |>
    dplyr::group_by(pq_id) |>
    dplyr::slice_min(name_dist + inst_dist + 0.05 * dplyr::coalesce(year_dist, 0L),
                     n = 1, with_ties = FALSE) |>
    dplyr::ungroup()
}

# Stage 3: combine exact + fuzzy (exact name_dist = 0 always wins), then attach
# to every dissertation, coalescing the rest to "unmatched".
assemble_pq_matched <- function(dissertations, exact, fuzzy) {
  matched <- dplyr::bind_rows(exact, fuzzy) |>
    dplyr::group_by(pq_id) |>
    dplyr::slice_min(name_dist + inst_dist + 0.05 * dplyr::coalesce(year_dist, 0L),
                     n = 1, with_ties = FALSE) |>
    dplyr::ungroup()

  dissertations |>
    dplyr::left_join(matched, by = "pq_id") |>
    dplyr::mutate(
      match_type = dplyr::coalesce(match_type, "unmatched"),
      matched_id = dplyr::coalesce(matched_id, NA_character_)
    )
}

# Thin wrapper composing the staged matchers (for non-targets callers; the
# pipeline wires the stages directly so the exact path is cached separately).
match_proquest <- function(dissertations, features, education_long, people,
                           ror_crosswalk = NULL) {
  persons <- prepare_pq_persons(education_long, people, features, ror_crosswalk)
  diss    <- prepare_pq_diss(dissertations, ror_crosswalk)
  exact   <- pq_match_exact(persons, diss)
  fuzzy   <- pq_match_fuzzy(persons, diss, exact)
  assemble_pq_matched(dissertations, exact, fuzzy)
}

.match_by_year <- function(persons, diss, name_max, inst_max, return_all = FALSE) {
  if (nrow(persons) == 0) return(if (return_all) .empty_cand_out() else .empty_match())

  persons_exp <- persons |>
    tidyr::crossing(year_offset = c(-1L, 0L, 1L)) |>
    dplyr::mutate(join_year = phd_year + year_offset)

  blocks <- dplyr::inner_join(
    persons_exp |> dplyr::distinct(join_year, inst_block) |>
      dplyr::filter(inst_block != "", !is.na(inst_block)),
    diss |> dplyr::transmute(join_year = degree_year, inst_block) |>
      dplyr::distinct() |>
      dplyr::filter(inst_block != "", !is.na(inst_block)),
    by = c("join_year", "inst_block"),
    relationship = "many-to-many"
  )

  if (nrow(blocks) == 0) return(if (return_all) .empty_cand_out() else .empty_match())

  results <- purrr::map2_dfr(blocks$join_year, blocks$inst_block, function(yr, inst) {
    p <- persons_exp |> dplyr::filter(join_year == yr, inst_block == inst,
                                      first_key != "", last_key != "")
    d <- diss        |> dplyr::filter(degree_year == yr, inst_block == inst,
                                      first_key != "", last_key != "")
    if (nrow(p) == 0 || nrow(d) == 0) return(tibble::tibble())

    # Component gate: first-token FUZZY (JW <= name_max), last-token EXACT
    # (dl == 0). A surname must match exactly — never fuzzy — so surname drift
    # (Chen/Chan, Moore/Moor) can't produce a false match; the first name is
    # still allowed to vary. name_dist is the worse of the two (pmax == df here).
    # The first INITIAL must also agree: distinct transliterated given names
    # sharing a common surname can clear the JW gate ("Yong" vs "Myoung",
    # 0.111) but true nickname variants keep their initial (Josh/Joshua,
    # Nathan/Nathaniel); cross-initial nicknames (Bob/Robert) fail JW anyway.
    df <- stringdist::stringdistmatrix(p$first_key, d$first_key, method = "jw")
    dl <- stringdist::stringdistmatrix(p$last_key,  d$last_key,  method = "jw")
    init_ok <- outer(substr(p$first_key, 1, 1), substr(d$first_key, 1, 1), "==")
    hits <- which(identity_match_ok(df, dl == 0, init_ok, name_max), arr.ind = TRUE)
    if (nrow(hits) == 0) return(tibble::tibble())

    # Middle-name disconfirmation (year path: no field gate — year corroborates).
    pil <- .mid_inits(p$middle_key); dil <- .mid_inits(d$middle_key)
    middle_ok <- !mapply(.middle_conflict_scalar, pil[hits[, 1]], dil[hits[, 2]])

    tibble::tibble(
      pq_id      = d$pq_id[hits[, 2]],
      matched_id = p$id[hits[, 1]],
      name_dist  = pmax(df[hits], dl[hits]),
      inst_dist  = 0,
      year_dist  = abs(p$year_offset[hits[, 1]]),
      tie        = stringdist::stringdist(p$name_norm[hits[, 1]],
                                          d$name_norm[hits[, 2]], method = "jw"),
      field_ok   = TRUE,
      middle_ok  = middle_ok
    )
  })

  if (nrow(results) == 0) return(if (return_all) .empty_cand_out() else .empty_match())

  results <- results |>
    dplyr::mutate(
      match_type = "fuzzy_year",
      reject_reason = dplyr::case_when(
        !middle_ok ~ "middle_name_conflict",
        !field_ok  ~ "field_not_corroborated",
        TRUE       ~ NA_character_))

  if (return_all) {
    return(results |>
      dplyr::select(pq_id, matched_id, match_type, name_dist, inst_dist,
                    year_dist, field_ok, middle_ok, reject_reason))
  }

  results |>
    dplyr::filter(is.na(reject_reason)) |>
    dplyr::group_by(pq_id) |>
    dplyr::slice_min(name_dist + 0.05 * year_dist + 1e-3 * dplyr::coalesce(tie, 0),
                     n = 1, with_ties = FALSE) |>
    dplyr::ungroup() |>
    dplyr::select(pq_id, matched_id, match_type, name_dist, inst_dist, year_dist)
}

.match_by_institution <- function(persons, diss, name_max, inst_max,
                                   return_all = FALSE) {
  if (nrow(persons) == 0) return(if (return_all) .empty_cand_out() else .empty_match())

  # Institution-only blocking has no year constraint — apply the plausibility
  # bound so historical dissertations can't match living namesakes.
  diss <- .plausible_diss_years(diss)

  shared_insts <- intersect(
    unique(persons$inst_block[persons$inst_block != "" & !is.na(persons$inst_block)]),
    unique(diss$inst_block[diss$inst_block != "" & !is.na(diss$inst_block)])
  )

  if (length(shared_insts) == 0) return(if (return_all) .empty_cand_out() else .empty_match())

  results <- purrr::map_dfr(shared_insts, function(inst) {
    p <- persons |> dplyr::filter(inst_block == inst, first_key != "", last_key != "")
    d <- diss    |> dplyr::filter(inst_block == inst, first_key != "", last_key != "")
    if (nrow(p) == 0 || nrow(d) == 0) return(tibble::tibble())

    # Component gate: first-token FUZZY (JW <= name_max), last-token EXACT
    # (dl == 0). Surnames never fuzzy-match; only the first name may vary.
    # First initial must agree — see .match_by_year (transliteration guard;
    # extra important here where no year corroborates the match).
    df <- stringdist::stringdistmatrix(p$first_key, d$first_key, method = "jw")
    dl <- stringdist::stringdistmatrix(p$last_key,  d$last_key,  method = "jw")
    init_ok <- outer(substr(p$first_key, 1, 1), substr(d$first_key, 1, 1), "==")
    hits <- which(identity_match_ok(df, dl == 0, init_ok, name_max), arr.ind = TRUE)
    if (nrow(hits) == 0) return(tibble::tibble())

    # Education-year window first (no phd_year on this path): gate the dissertation
    # year against the person's education span to bar different-decade namesakes.
    cand <- tibble::tibble(
      .pi        = hits[, 1],
      .di        = hits[, 2],
      pq_id      = d$pq_id[hits[, 2]],
      matched_id = p$id[hits[, 1]],
      name_dist  = pmax(df[hits], dl[hits]),
      tie        = stringdist::stringdist(p$name_norm[hits[, 1]],
                                          d$name_norm[hits[, 2]], method = "jw"),
      .dgy       = d$degree_year[hits[, 2]],
      .emn       = p$edu_min_year[hits[, 1]],
      .emx       = p$edu_max_year[hits[, 1]]
    ) |>
      dplyr::filter(.within_edu_window(.dgy, .emn, .emx))
    if (nrow(cand) == 0) return(tibble::tibble())

    # New gates on the no-year path: middle-name disconfirmation AND field/subject
    # corroboration (the LinkedIn PhD field must be consistent with the diss
    # subjects — no year to lean on here).
    pil <- .mid_inits(p$middle_key); dil <- .mid_inits(d$middle_key)
    cand |>
      dplyr::mutate(
        middle_ok = !mapply(.middle_conflict_scalar, pil[.pi], dil[.di]),
        field_ok  = fields_consistent(p$phd_field_text[.pi], d$subject_all[.di])
      ) |>
      dplyr::select(-.pi, -.di, -.dgy, -.emn, -.emx)
  })

  if (nrow(results) == 0) return(if (return_all) .empty_cand_out() else .empty_match())

  results <- results |>
    dplyr::mutate(
      match_type = "fuzzy_inst",
      inst_dist  = 0,
      year_dist  = NA_integer_,
      reject_reason = dplyr::case_when(
        !middle_ok ~ "middle_name_conflict",
        !field_ok  ~ "field_not_corroborated",
        TRUE       ~ NA_character_))

  if (return_all) {
    return(results |>
      dplyr::select(pq_id, matched_id, match_type, name_dist, inst_dist,
                    year_dist, field_ok, middle_ok, reject_reason))
  }

  results |>
    dplyr::filter(is.na(reject_reason)) |>
    dplyr::group_by(pq_id) |>
    dplyr::slice_min(name_dist + 1e-3 * dplyr::coalesce(tie, 0),
                     n = 1, with_ties = FALSE) |>
    dplyr::ungroup() |>
    dplyr::select(pq_id, matched_id, match_type, name_dist, inst_dist, year_dist)
}

.empty_match <- function() {
  tibble::tibble(
    pq_id = character(), matched_id = character(),
    match_type = character(), name_dist = double(),
    inst_dist = double(), year_dist = integer()
  )
}

# Candidate shape returned by the matchers under return_all = TRUE (accepted +
# rejected, with the gate booleans and reason).
.empty_cand_out <- function() {
  tibble::tibble(
    pq_id = character(), matched_id = character(),
    match_type = character(), name_dist = double(),
    inst_dist = double(), year_dist = integer(),
    field_ok = logical(), middle_ok = logical(), reject_reason = character()
  )
}

# ---- QC helpers ----

proquest_match_summary <- function(pq_matched) {
  pq_matched |>
    dplyr::count(match_type) |>
    dplyr::mutate(pct = n / sum(n) * 100)
}

build_tier_a_unmatched <- function(inclusion, pq_matched, people, features) {
  matched_ids <- pq_matched |>
    dplyr::filter(match_type != "unmatched", !is.na(matched_id)) |>
    dplyr::pull(matched_id) |> unique()

  inclusion |>
    dplyr::filter(tier == "A_include", !(id %in% matched_ids)) |>
    dplyr::left_join(
      people |> dplyr::select(id, name, url) |> dplyr::distinct(),
      by = "id") |>
    dplyr::left_join(
      features |> dplyr::select(id, phd_institution, phd_year) |> dplyr::distinct(),
      by = "id") |>
    dplyr::select(id, name, url, phd_institution, phd_year,
                  edu_neuro, pq_neuro, reason) |>
    dplyr::arrange(name)
}

write_tier_a_unmatched <- function(review, path) {
  readr::write_csv(review, path)
  path
}

# ---- unmatched manual-audit sample (does the pipeline miss matchable people?) ----
#
# A reproducible random sample of in-scope (Tier A/B) PhDs that ProQuest did NOT
# match, each annotated with its NEAREST dissertation candidate within the same
# block and that candidate's name distance. The diagnostic:
#   block_size == 0          ⇒ institution absent / normalization mismatch
#   small nearest_name_dist  ⇒ true match just over the 0.12 threshold
#   large nearest_name_dist  ⇒ dissertation genuinely not in the corpus
# Feed this to a human ProQuest search to judge whether NAME_MAX / normalization
# need tuning.
build_unmatched_review <- function(persons, diss, pq_matched, inclusion,
                                   education_long = NULL, n = 100, seed = 42) {
  matched_ids <- pq_matched |>
    dplyr::filter(match_type != "unmatched", !is.na(matched_id)) |>
    dplyr::pull(matched_id) |> unique()

  ab_ids <- inclusion |>
    dplyr::filter(tier %in% c("A_include", "B_adjudicate")) |>
    dplyr::pull(id) |> unique()

  cand <- persons |>
    dplyr::filter(id %in% ab_ids, !(id %in% matched_ids))
  if (nrow(cand) == 0) cand <- persons[0, ]

  set.seed(seed)
  samp <- if (nrow(cand) > 0) dplyr::slice_sample(cand, n = min(n, nrow(cand))) else cand

  field_tbl <- NULL
  if (!is.null(education_long)) {
    field_tbl <- education_long |>
      dplyr::filter(degree_norm == "phd") |>
      dplyr::group_by(id) |>
      dplyr::summarise(field = dplyr::first(field_raw), .groups = "drop")
  }

  near <- purrr::pmap_dfr(
    list(samp$id, samp$first_key, samp$last_key, samp$inst_block, samp$phd_year),
    function(pid, fk, lk, inst, yr) {
      block <- if (is.na(inst) || inst == "") diss[0, ]
               else if (!is.na(yr)) dplyr::filter(diss, inst_block == inst,
                                                  !is.na(degree_year),
                                                  abs(degree_year - yr) <= 1)
               else dplyr::filter(diss, inst_block == inst)
      if (nrow(block) == 0 || fk == "" || lk == "") {
        tibble::tibble(id = pid, nearest_pq_id = NA_character_,
                       nearest_author = NA_character_, nearest_title = NA_character_,
                       nearest_inst = NA_character_, nearest_year = NA_integer_,
                       nearest_pq_url = NA_character_,
                       nearest_name_dist = NA_real_, block_size = nrow(block))
      } else {
        # nearest by component-max distance (diagnostic only; the matcher now
        # also requires the last token to be EXACT, so a small nearest_name_dist
        # here may still be a non-match if the surname differs at all)
        d <- pmax(stringdist::stringdist(fk, block$first_key, method = "jw"),
                  stringdist::stringdist(lk, block$last_key,  method = "jw"))
        j <- which.min(d)
        tibble::tibble(id = pid, nearest_pq_id = block$pq_id[j],
                       nearest_author = block$author_name[j],
                       nearest_title = block$title[j],
                       nearest_inst = block$institution[j],
                       nearest_year = as.integer(block$degree_year[j]),
                       nearest_pq_url = block$pq_url[j],
                       nearest_name_dist = d[j], block_size = nrow(block))
      }
    })

  out <- samp |>
    dplyr::select(id, name, url, phd_institution, phd_year,
                  name_norm, name_key, inst_norm, inst_block) |>
    dplyr::left_join(near, by = "id") |>
    dplyr::mutate(
      unmatched_reason = dplyr::case_when(
        is.na(inst_block) | inst_block == "" ~ "institution_missing_on_person",
        block_size == 0                      ~ "institution_no_match_in_pq",
        is.na(nearest_name_dist)             ~ "no_name_to_compare",
        nearest_name_dist > NAME_MAX         ~ "name_above_threshold",
        TRUE                                 ~ "within_threshold_unexpected"
      )
    )
  if (!is.null(field_tbl)) out <- dplyr::left_join(out, field_tbl, by = "id")
  out
}

write_unmatched_review <- function(review, path) {
  readr::write_csv(review, path)
  path
}

# ---- accepted-fuzzy-match audit (hand-review EVERY fuzzy accept) ----
#
# The unmatched review shows misses; this shows the accepted fuzzy matches so a
# human can catch false positives (the user's explicit ask). Exact matches
# (name_dist == 0) are intentionally omitted. Sorted worst-first by name_dist.
# Carries the LinkedIn profile URL and the ProQuest docview URL so each pair can
# be opened directly, plus the dissertation's neuro classification.
build_fuzzy_match_review <- function(pq_matched, persons, pq_neuro_classified) {
  cls <- pq_neuro_classified |>
    dplyr::select(pq_id, pq_neuro, neuro_basis)

  pq_matched |>
    dplyr::filter(match_type %in% c("fuzzy_year", "fuzzy_inst"),
                  !is.na(matched_id)) |>
    dplyr::left_join(cls, by = "pq_id") |>
    dplyr::left_join(
      persons |> dplyr::select(id, li_name = name, li_url = url,
                               li_institution = phd_institution,
                               li_phd_year = phd_year),
      by = c("matched_id" = "id")) |>
    dplyr::transmute(
      matched_id, li_name, li_url, li_institution, li_phd_year,
      pq_id, pq_author = author_name, pq_url, pq_title = title,
      pq_institution = institution, pq_year = degree_year,
      pq_subject = subject, pq_subject_all = subject_all,
      pq_neuro, neuro_basis, match_type, name_dist, year_dist
    ) |>
    dplyr::arrange(dplyr::desc(name_dist))
}

write_fuzzy_match_review <- function(review, path) {
  readr::write_csv(review, path)
  path
}

# ---- rejected-fuzzy-match audit (what the new gates dropped) ----
#
# Runs both fuzzy matchers in return_all mode, keeps only the candidates a gate
# rejected (field_not_corroborated / middle_name_conflict), so recall loss from the
# gates is auditable. A dissertation still matched to a DIFFERENT person by the
# accepted stage may also appear here for its rejected candidate — this is an
# audit artifact, not the match set.
pq_fuzzy_rejected <- function(persons, diss, exact) {
  residual <- diss |> dplyr::filter(!(pq_id %in% exact$pq_id))

  year_persons   <- persons |> dplyr::filter(!is.na(phd_year))
  noyear_persons <- persons |> dplyr::filter(is.na(phd_year))

  ra <- .match_by_year(year_persons, residual, NAME_MAX, INST_MAX, return_all = TRUE)
  rb <- .match_by_institution(noyear_persons, residual, NAME_MAX, INST_MAX,
                              return_all = TRUE)

  dplyr::bind_rows(ra, rb) |>
    dplyr::filter(!is.na(reject_reason)) |>
    dplyr::group_by(pq_id) |>
    dplyr::slice_min(name_dist + 0.05 * dplyr::coalesce(year_dist, 0L),
                     n = 1, with_ties = FALSE) |>
    dplyr::ungroup()
}

# Human-readable rejected-match review, mirroring build_fuzzy_match_review (same
# LinkedIn + ProQuest URL columns and neuro classification) plus reject_reason and
# the LinkedIn field that failed corroboration.
build_fuzzy_rejected_review <- function(pq_fuzzy_rejected, persons,
                                        pq_neuro_classified) {
  cls <- pq_neuro_classified |>
    dplyr::select(pq_id, pq_author = author_name, pq_url, pq_title = title,
                  pq_institution = institution, pq_year = degree_year,
                  pq_subject = subject, pq_subject_all = subject_all,
                  pq_neuro, neuro_basis)

  pq_fuzzy_rejected |>
    dplyr::left_join(cls, by = "pq_id") |>
    dplyr::left_join(
      persons |> dplyr::select(id, li_name = name, li_url = url,
                               li_institution = phd_institution,
                               li_phd_year = phd_year,
                               li_field = phd_field_text),
      by = c("matched_id" = "id")) |>
    dplyr::transmute(
      matched_id, li_name, li_url, li_institution, li_phd_year, li_field,
      pq_id, pq_author, pq_url, pq_title, pq_institution, pq_year,
      pq_subject, pq_subject_all, pq_neuro, neuro_basis,
      match_type, name_dist, year_dist, reject_reason
    ) |>
    dplyr::arrange(reject_reason, dplyr::desc(name_dist))
}

write_fuzzy_rejected_review <- function(review, path) {
  readr::write_csv(review, path)
  path
}

# ---- subject-taxonomy classification (the precision signal) ----
#
# Prefer the dissertation's own ProQuest/UMI subject code over abstract text.
# NOTE: the four-digit codes below are the ProQuest (UMI) subject taxonomy;
# confirm the exact membership against the live classification list when creds
# arrive. The architecture — subject-code first, text only as fallback — is the
# point and does not change.

# Codes that count as neuroscience outright.
NEURO_SUBJECT_CODES <- c(
  "0317",  # Neurosciences
  "0989",  # Physiological psychology
  "0349"   # Psychobiology
)

# Feeder codes — neuro pipelines but mostly non-neuro, so they require the
# abstract/keywords to corroborate before counting.
NEURO_FEEDER_CODES <- c(
  "0633",  # Cognitive psychology (demoted from core; needs text corroboration)
  "0541",  # Engineering, Biomedical
  "0202",  # Bioengineering
  "0307",  # Molecular biology
  "0719",  # Physiology
  "0419"   # Pharmacology
)

NEURO_TEXT_PATTERNS <- c(
  "neuro", "neural", "brain", "cortex", "cortical", "synap", "axon",
  "dendrit", "hippocamp", "amygdala", "dopamin", "neuron", "glia",
  "electrophysiolog", "optogenet", "connectom", "neurodegener"
)

# The TDM export ships subject *labels*, not numeric codes, so classification
# keys on subject labels. Core-neuro detection runs over ALL subject terms (see
# pq_core_neuro_any), while the legacy label_class keys on the PRIMARY label.
# Labels that count as neuroscience outright.
NEURO_SUBJECT_LABEL_PATTERNS <- c(
  "neuroscience", "neurology", "neurobiolog", "physiological psychology",
  "psychobiology", "cognitive neuro", "behavioral neuro"
)
# Feeder labels — real neuro pipelines but mostly non-neuro on their own, so they
# need abstract/keyword corroboration. Checked AFTER the neuro set, so
# "physiological psychology" resolves to neuro before "physiolog" feeds here.
# "cognitive psychology" (0633) demoted here from core — too broad without
# text evidence of actual neuroscience content.
NEURO_FEEDER_LABEL_PATTERNS <- c(
  "cognitive psychology",
  "biomedical engineer", "engineering, biomedical", "bioengineer",
  "molecular biolog", "physiolog", "anatomy & physiolog", "pharmacolog",
  "biophysic"
)
# Adjacent labels — broad bio/psych/eng fields newly collected via the
# keyword-gated arm of the expanded ProQuest search (Block 2). A genuinely neuro
# dissertation is often filed under one of these with no neuro subject term, so
# they count ONLY with neuro text corroboration (like feeders) but carry their own
# neuro_basis tag for audit. Checked AFTER neuro + feeder, so e.g. "physiological
# psychology" still resolves to neuro. CAVEAT: the TDM export itself ships no
# abstract, so corroboration is title + keywords + abstract where available
# (see read_armv_abstracts() — Arm V's ~8.1k GOIDs only, hand-extracted from
# TDM Studio, 2026-07-31). For the ~331k dissertations outside that scope, an
# adjacent diss whose only neuro signal was in the (unshipped) abstract still
# lands "no_corroboration" — the conservative call, now narrower in scope.
NEURO_ADJACENT_LABEL_PATTERNS <- c(
  "cellular biolog", "genetics", "biochemistr", "developmental biolog",
  "clinical psychology", "developmental psychology", "experimental psychology",
  "behavioral psychology", "cognitive therapy", "cognition & reasoning",
  "medical imaging", "electrical engineer", "immunolog", "pathology",
  "endocrinolog", "computer science", "mental health", "aging"
)

# Core-neuro detection over ALL subject terms (the "; "-collapsed subject_all).
# Auto-include if ANY subject term is core neuro. Patterns are substrings, so
# detecting on the collapsed string covers every listed subject.
pq_core_neuro_any <- function(subject_all) {
  s <- stringr::str_to_lower(dplyr::coalesce(subject_all, ""))
  purrr::map_lgl(s, ~ any(stringr::str_detect(.x, NEURO_SUBJECT_LABEL_PATTERNS)))
}

# Cognitive psychology tag — still tracked for audit but no longer grants
# automatic inclusion (demoted to feeder-level, requires text corroboration).
pq_cogpsych_any <- function(subject_all) {
  s <- stringr::str_to_lower(dplyr::coalesce(subject_all, ""))
  purrr::map_lgl(s, ~ any(stringr::str_detect(.x, "cognitive psychology")))
}

# Classify the primary subject label:
# "neuro" | "feeder" | "adjacent" | "other" | "none".
.pq_label_class <- function(subject) {
  s <- stringr::str_to_lower(dplyr::coalesce(subject, ""))
  is_neuro    <- purrr::map_lgl(
    s, ~ any(stringr::str_detect(.x, NEURO_SUBJECT_LABEL_PATTERNS)))
  is_feeder   <- purrr::map_lgl(
    s, ~ any(stringr::str_detect(.x, NEURO_FEEDER_LABEL_PATTERNS)))
  is_adjacent <- purrr::map_lgl(
    s, ~ any(stringr::str_detect(.x, NEURO_ADJACENT_LABEL_PATTERNS)))
  dplyr::case_when(
    is_neuro       ~ "neuro",
    is_feeder      ~ "feeder",
    is_adjacent    ~ "adjacent",
    s == ""        ~ "none",
    TRUE           ~ "other"
  )
}

# Vectorized: TRUE if a dissertation is neuroscience. Primary subject label is
# authoritative; feeder labels need text corroboration; missing label falls back
# to abstract/keyword text. `subject` is the primary subject label.
pq_is_neuro <- function(subject, abstract = NULL, keywords = NULL,
                        title = NULL, subject_all = NULL) {
  cls <- .pq_label_class(subject)
  # Core/cog-psych checks run over ALL subjects when subject_all is supplied;
  # fall back to the primary subject otherwise (preserves legacy callers).
  sa <- if (is.null(subject_all)) subject else subject_all
  core_any <- pq_core_neuro_any(sa)
  cog_any  <- pq_cogpsych_any(sa)
  text <- stringr::str_to_lower(paste(dplyr::coalesce(title, ""),
                                      dplyr::coalesce(abstract, ""),
                                      dplyr::coalesce(keywords, "")))
  text_neuro <- purrr::map_lgl(
    text, ~ any(stringr::str_detect(.x, NEURO_TEXT_PATTERNS)))

  dplyr::case_when(
    core_any                       ~ TRUE,   # core neuro in any subject term
    cls == "feeder"   & text_neuro ~ TRUE,   # feeder (incl. cog psych) + text
    cls == "feeder"                ~ FALSE,
    cls == "adjacent" & text_neuro ~ TRUE,   # keyword-gated adjacent field
    cls == "adjacent"              ~ FALSE,
    cls == "other"                 ~ FALSE,   # primary subject is non-neuro
    TRUE                           ~ text_neuro   # no subject label
  )
}

# ---- neuro classification with audit trail ----

pq_classify_neuro <- function(dissertations) {
  label_class <- .pq_label_class(dissertations$subject)
  core_any <- pq_core_neuro_any(dissertations$subject_all)
  cog_any  <- pq_cogpsych_any(dissertations$subject_all)
  text <- stringr::str_to_lower(paste(
    dplyr::coalesce(dissertations$title, ""),
    dplyr::coalesce(dissertations$abstract, ""),
    dplyr::coalesce(dissertations$keywords, "")))
  text_neuro <- purrr::map_lgl(
    text, ~ any(stringr::str_detect(.x, NEURO_TEXT_PATTERNS)))
  text_hits <- purrr::map_chr(text, function(t) {
    matched <- NEURO_TEXT_PATTERNS[stringr::str_detect(t, NEURO_TEXT_PATTERNS)]
    paste(matched, collapse = "; ")
  })

  dissertations |>
    dplyr::mutate(
      label_class    = label_class,
      core_neuro_any  = core_any,
      cogpsych_any    = cog_any,
      is_neuro_label    = label_class == "neuro",
      is_feeder_label   = label_class == "feeder",
      is_adjacent_label = label_class == "adjacent",
      text_neuro     = text_neuro,
      text_hits      = text_hits,
      pq_neuro = dplyr::case_when(
        core_any                         ~ TRUE,
        is_feeder_label & text_neuro     ~ TRUE,   # incl. cog psych
        is_feeder_label                  ~ FALSE,
        is_adjacent_label & text_neuro   ~ TRUE,
        is_adjacent_label                ~ FALSE,
        label_class == "other"           ~ FALSE,
        TRUE                             ~ text_neuro
      ),
      neuro_basis = dplyr::case_when(
        core_any & is_neuro_label        ~ "primary_subject_neuro",
        core_any                         ~ "core_subject_any",
        is_feeder_label & text_neuro     ~ "feeder_label_text_corroborated",
        is_feeder_label                  ~ "feeder_label_no_corroboration",
        is_adjacent_label & text_neuro   ~ "adjacent_label_text_corroborated",
        is_adjacent_label                ~ "adjacent_label_no_corroboration",
        label_class == "other"           ~ "non_neuro_subject",
        text_neuro                       ~ "text_only",
        TRUE                             ~ "no_signal"
      )
    )
}

# Every feeder- AND adjacent-label dissertation with its neuro call, for human
# audit (neuro_basis distinguishes feeder vs adjacent and corroborated vs not).
write_pq_feeder_review <- function(pq_neuro_classified, path) {
  pq_neuro_classified |>
    dplyr::filter(is_feeder_label | is_adjacent_label) |>
    dplyr::select(pq_id, pq_url, author_name, title, institution, degree_year,
                  subject_code, subject, subject_all, label_class, pq_neuro,
                  neuro_basis, text_hits, keywords) |>
    readr::write_csv(path)
  path
}

# ---- match-rate measurement (does ProQuest deserve to be a gate?) ----
#
# Of the people whose *education* already flags neuro (high-precision true
# positives), what fraction did we link to a ProQuest dissertation? This is the
# number that decides whether ProQuest can act as a near-gate (~90%) or only as
# an adjudicator/positive signal (~60%). Meaningful only once real ProQuest data
# replaces the stub.
proquest_match_rate <- function(pq_matched, education_long) {
  neuro_ids <- education_long |>
    dplyr::filter(degree_norm == "phd",
                  is_neuro(field_raw, field_norm, institution)) |>
    dplyr::pull(id) |> unique()

  matched_ids <- pq_matched |>
    dplyr::filter(match_type != "unmatched", !is.na(matched_id)) |>
    dplyr::pull(matched_id) |> unique()

  n_cohort  <- length(neuro_ids)
  n_matched <- length(intersect(neuro_ids, matched_ids))

  tibble::tibble(
    cohort     = "education_neuro_phd",
    n_cohort   = n_cohort,
    n_matched  = n_matched,
    match_rate = dplyr::if_else(n_cohort > 0, n_matched / n_cohort, NA_real_)
  )
}
