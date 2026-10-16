# R/08_classify.R — classify dissertations into content area + methodology
#
# REPLACED 2026-07-31. This file used to carry a 13-category `neuro_taxonomy()`
# and a nearest-centroid `classify_subfield()` over a TF-IDF bag-of-words
# `embed_text()` stub. Measured on the Arm-V dissertations that classifier
# flagged 77.7% of its own answers `needs_review` and agreed with the
# deterministic ProQuest-term label (`SUBFIELD_TERM_MAP`, below) only 22.2% of
# the time — barely above chance, and it could not carry a published figure.
# See METHODS.md, "Dissertation subfield".
#
# Two things changed at once:
#   1. The taxonomy shrank from 13 fine-grained subfields to two orthogonal
#      4-category schemes — content area and primary methodology — because the
#      13-way distinction was too fine for any classifier (embedding or LLM) to
#      carry reliably.
#   2. The classifier itself is now an LLM call (Claude, via the Messages API)
#      instead of a from-scratch embedding, following the same
#      billable-network-call pattern used for the data-acquisition steps:
#      abstracted behind a `backend` argument, a `stub` backend that returns
#      NA and keeps the pipeline offline-safe, and a manual `refresh_*_cache()`
#      driver — NOT a `tar_target` — that hits the live API once and writes a
#      cache CSV the pipeline then just reads. Nothing in this file calls the
#      live API on its own; that only happens if `refresh_llm_classify_cache()`
#      is run by hand.
#
# The DETERMINISTIC ProQuest-subject-term map (`SUBFIELD_TERM_MAP`,
# `subfield_from_subject_terms()`) is UNCHANGED and still feeds
# `fig_first_sector_by_subfield.png` — this rewrite only touches the model
# side, which used to be `subfield_model` and is now `content_area` +
# `methodology`.

# ---- content area taxonomy (4 categories) ----

content_area_taxonomy <- function() {
  tibble::tribble(
    ~area_id, ~content_area,          ~description,
    1L, "Cellular/Molecular",  "Ion channels, synapses, signal transduction, gene expression, neurodevelopment at the cell level, molecular and structural biology of neurons and glia.",
    2L, "Systems/Circuits",    "Neural circuits, electrophysiology, in vivo recording, sensory and motor systems, population coding, circuit-level dynamics, systems-level anatomy and physiology.",
    3L, "Cognitive/Behavioral", "Cognition, perception, learning, memory, decision making, emotion, social/affective processing, animal or human behavior at the whole-organism level.",
    4L, "Disease/Clinical",    "Neurological or psychiatric disease and disorder, diagnosis, biomarkers, treatment, patient populations, neurodegeneration, translational and clinical work."
  )
}

# ---- primary methodology taxonomy (4 categories) ----

methodology_taxonomy <- function() {
  tibble::tribble(
    ~method_id, ~methodology,                  ~description,
    1L, "Wet-lab/experimental",       "Bench work: molecular biology, histology, animal surgery/electrophysiology, in vitro or in vivo experiments generating primary wet-lab data.",
    2L, "Computational/quantitative", "Modeling, simulation, theory, machine learning, bioinformatics, or quantitative/statistical analysis as the primary method, with no primary wet-lab or human-subjects data collection.",
    3L, "Human-subjects/neuroimaging", "Human research participants: neuroimaging (fMRI/EEG/MEG/PET), psychophysics, clinical trials, cognitive/behavioral testing in human subjects.",
    4L, "Mixed/other",                "Substantial use of two or more of the above with no single dominant method, or a methodology none of the above describes."
  )
}

# ---- LLM classification backend ----
#
# Same abstraction shape as embed_text() used to have: a `backend` argument,
# a "stub" that does no network I/O (keeps tar_make() green offline), and a
# real backend behind it. "stub" here returns NA rather than fabricating an
# assignment via BoW similarity — there is no offline substitute for the LLM
# call, unlike the old embedding approach.

classify_llm <- function(texts,
                         backend          = "stub",
                         model            = "claude-haiku-4-5",
                         content_areas    = content_area_taxonomy(),
                         methodologies    = methodology_taxonomy(),
                         api_key          = Sys.getenv("ANTHROPIC_API_KEY")) {
  switch(backend,
    stub = .llm_classify_stub(texts),
    api  = .llm_classify_api(texts, model, content_areas, methodologies, api_key),
    stop("Unknown LLM classify backend: ", backend)
  )
}

.llm_classify_stub <- function(texts) {
  tibble::tibble(
    content_area          = NA_character_,
    methodology           = NA_character_,
    classify_confidence   = NA_character_,
    classify_backend      = "stub"
  )[rep(1, length(texts)), ]
}

# One dissertation per Messages API call — the Messages API has no bulk
# classify endpoint, and per-record cost/latency is exactly why this is a
# manual driver script (see refresh_llm_classify_cache()) and never a
# tar_target. NOT invoked anywhere by default; requires backend = "api"
# and a real ANTHROPIC_API_KEY.
.llm_classify_api <- function(texts, model, content_areas, methodologies, api_key) {
  if (!nzchar(api_key)) {
    stop("classify_llm(backend = 'api') needs ANTHROPIC_API_KEY set.")
  }
  purrr::map_dfr(texts, function(txt) {
    resp <- tryCatch(
      .anthropic_classify_one(txt, model, content_areas, methodologies, api_key),
      error = function(e) NULL
    )
    if (is.null(resp)) {
      return(tibble::tibble(
        content_area = NA_character_, methodology = NA_character_,
        classify_confidence = NA_character_, classify_backend = "api"
      ))
    }
    resp
  })
}

.llm_classify_system_prompt <- function(content_areas, methodologies) {
  paste0(
    "You classify neuroscience PhD dissertations into exactly one content area ",
    "and exactly one primary methodology, from fixed lists. Respond with ONLY a ",
    "JSON object, no markdown code fence, no other text: {\"content_area\": \"...\", ",
    "\"methodology\": \"...\", \"confidence\": \"high\"|\"medium\"|\"low\"}. ",
    "Use the category names verbatim.\n\n",
    "Content areas:\n",
    paste(sprintf("- %s: %s", content_areas$content_area, content_areas$description),
          collapse = "\n"),
    "\n\nPrimary methodology:\n",
    paste(sprintf("- %s: %s", methodologies$methodology, methodologies$description),
          collapse = "\n")
  )
}

.anthropic_classify_one <- function(text, model, content_areas, methodologies, api_key) {
  # system as a cache_control-annotated block, not a bare string: the prompt
  # is identical on every call (only `text` varies), so this is a straight
  # ~90% discount on the system-prompt half of the input cost after the first
  # call.
  body <- list(
    model      = model,
    max_tokens = 200,
    system     = list(list(
      type          = "text",
      text          = .llm_classify_system_prompt(content_areas, methodologies),
      cache_control = list(type = "ephemeral")
    )),
    messages   = list(list(role = "user", content = text))
  )
  resp <- httr2::request("https://api.anthropic.com/v1/messages") |>
    httr2::req_headers(
      "x-api-key"         = api_key,
      "anthropic-version" = "2023-06-01",
      "content-type"      = "application/json"
    ) |>
    httr2::req_body_json(body) |>
    httr2::req_perform()

  raw <- httr2::resp_body_json(resp)
  txt <- raw$content[[1]]$text
  # Haiku routinely wraps the JSON in a ```json ... ``` markdown fence even
  # when told to respond with ONLY the object. Strip fences, then fall back
  # to pulling the first {...} span, before giving up.
  txt_clean <- stringr::str_trim(stringr::str_remove_all(txt, "```(json)?"))
  parsed <- tryCatch(jsonlite::fromJSON(txt_clean), error = function(e) NULL)
  if (is.null(parsed)) {
    span <- stringr::str_extract(txt, "\\{[\\s\\S]*\\}")
    if (!is.na(span)) parsed <- tryCatch(jsonlite::fromJSON(span), error = function(e) NULL)
  }
  if (is.null(parsed)) {
    return(tibble::tibble(
      content_area = NA_character_, methodology = NA_character_,
      classify_confidence = NA_character_, classify_backend = "api"
    ))
  }

  tibble::tibble(
    content_area        = dplyr::if_else(parsed$content_area %in% content_areas$content_area,
                                         parsed$content_area, NA_character_),
    methodology         = dplyr::if_else(parsed$methodology %in% methodologies$methodology,
                                         parsed$methodology, NA_character_),
    classify_confidence = parsed$confidence %||% NA_character_,
    classify_backend    = "api"
  )
}

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x

# ---- pipeline wrappers ----
#
# Cache-path pattern identical to the old classify_openalex_dissertations():
# a cache CSV, if present, is preferred over any live call, so the default
# path through this function is offline and free. refresh_llm_classify_cache()
# below is the only thing that ever writes that cache.

classify_dissertations_llm <- function(dissertations,
                                       backend    = "stub",
                                       cache_path = NULL,
                                       ...) {
  base <- dissertations |> dplyr::select(pq_id)

  if (backend != "api" && !is.null(cache_path) && file.exists(cache_path)) {
    cached <- readr::read_csv(cache_path, show_col_types = FALSE,
                              col_types = readr::cols(pq_id = readr::col_character()))
    return(dplyr::left_join(base, cached, by = "pq_id"))
  }

  cls <- classify_llm(dissertations$text, backend = backend, ...)
  dplyr::bind_cols(base, cls)
}

# Arm-V pq_ids (the ProQuest dissertations that actually reach a figure), via
# the inclusion_table.csv seam — same rule build_subfield_labels() applies.
# `pq_neuro_only` (the LLM-classify store's input) is corpus-wide — 64,405
# dissertations classed neuro across all of ProQuest — but only the 8,153
# matched to an Arm-V person ever join to `subfield_labels`/the figure. Scope
# the billable classification pass to this set: same downstream coverage at
# ~1/8th the cost, since the rest would be spend with no consumer.
armv_pq_ids <- function(inclusion_csv = "output/tables/inclusion_table.csv") {
  readr::read_csv(inclusion_csv, show_col_types = FALSE,
                  col_types = readr::cols(.default = readr::col_character())) |>
    dplyr::filter(tier == "A_include", !is.na(pq_url)) |>
    dplyr::transmute(pq_id = stringr::str_extract(pq_url, "[0-9]+$")) |>
    dplyr::pull(pq_id)
}

# Manual helper: hit the live Anthropic API and persist for the pipeline to
# pick up. Billable — run by hand, never from a tar_target.
#
# `dissertations` is filtered to Arm V by default via armv_pq_ids() — pass
# armv_only = FALSE to classify the full corpus-wide `pq_neuro_only` instead
# (64,405 vs 8,153; ~8x the cost for coverage nothing downstream reads yet).
#
# RESUMABLE. A run that dies partway (a 2026-07-31 run stopped mid-batch on
# "credit balance too low", HTTP 400) must not re-pay for rows it already
# classified. If `path` already has a row with a non-NA content_area for a
# pq_id, that row is kept as-is and NOT re-sent to the API — only pq_ids
# missing from the cache, or present with content_area NA (a prior failure),
# are classified this run. Pass resume = FALSE to force reclassifying
# everything (e.g. after a prompt or model change makes the old cache stale).
refresh_llm_classify_cache <- function(dissertations, path,
                                       model     = "claude-haiku-4-5",
                                       api_key   = Sys.getenv("ANTHROPIC_API_KEY"),
                                       armv_only = TRUE,
                                       resume    = TRUE) {
  if (armv_only) {
    dissertations <- dissertations |> dplyr::filter(pq_id %in% armv_pq_ids())
  }

  done <- NULL
  if (resume && file.exists(path)) {
    cached <- readr::read_csv(path, show_col_types = FALSE,
                              col_types = readr::cols(pq_id = readr::col_character()))
    done <- cached |> dplyr::filter(!is.na(content_area))
    dissertations <- dissertations |> dplyr::filter(!(pq_id %in% done$pq_id))
  }

  if (nrow(dissertations) > 0) {
    cls <- classify_llm(dissertations$text, backend = "api", model = model, api_key = api_key)
    new <- dplyr::bind_cols(dissertations |> dplyr::select(pq_id), cls)
  } else {
    new <- NULL
  }

  out <- dplyr::bind_rows(done, new) |> dplyr::distinct(pq_id, .keep_all = TRUE)
  readr::write_csv(out, path)

  # Loud, not silent: this is exactly what let two prior runs burn budget
  # unnoticed (a markdown-fence parse failure, then a mid-run billing cutoff)
  # — both degraded to NA with no visible signal.
  n_attempted <- if (is.null(new)) 0L else nrow(new)
  n_failed    <- if (is.null(new)) 0L else sum(is.na(new$content_area))
  message(sprintf(
    "refresh_llm_classify_cache: %d attempted this run, %d failed (NA) -- %d/%d total rows classified in %s",
    n_attempted, n_failed, sum(!is.na(out$content_area)), nrow(out), path
  ))
  path
}

classify_llm_summary <- function(classified) {
  n_total <- nrow(classified)
  list(
    content_area = classified |>
      dplyr::count(content_area, sort = TRUE) |>
      dplyr::mutate(pct = n / n_total * 100),
    methodology = classified |>
      dplyr::count(methodology, sort = TRUE) |>
      dplyr::mutate(pct = n / n_total * 100),
    cross = classified |>
      dplyr::count(content_area, methodology, sort = TRUE) |>
      dplyr::mutate(pct = n / n_total * 100),
    n_total     = n_total,
    n_labeled   = sum(!is.na(classified$content_area) & !is.na(classified$methodology))
  )
}

write_classify_llm_summary <- function(summary, path) {
  readr::write_csv(summary$cross, path)
  path
}

# ---- subfield from ProQuest subject terms ----------------------------------
#
# A DETERMINISTIC map from ProQuest's controlled vocabulary onto 13 legacy
# subfield names. UNCHANGED by the 2026-07-31 rewrite above — this is what
# `fig_first_sector_by_subfield.png` plots, and it does not depend on
# content_area_taxonomy()/methodology_taxonomy() or the LLM classifier in any
# way. The 13 names here are a fixed vocabulary of their own now, not "the same
# names neuro_taxonomy() uses" (that function no longer exists).
#
# ORDER MATTERS: first match wins, most specific first. The map is applied to
# each of a person's subject terms in turn and the FIRST term that maps at all
# decides, so a generic term never outranks an informative one.
#
# "Neurosciences" and friends are deliberately absent: 83.9% of Arm V carries
# "Neurosciences", so it is the single least informative token in the corpus.
# Someone whose only terms are generic gets "General / unspecified", which is a
# real answer here and is plotted as its own bar — not dropped, not guessed.
SUBFIELD_TERM_MAP <- tibble::tribble(
  ~pattern,                                                      ~subfield,
  "medical imaging|radiolog|imaging science",                    "Neuroimaging",
  "pharmacolog|toxicolog",                                       "Neuropharmacology",
  "biomedical engineering|bioengineering|electrical engineering|robotic", "Neural engineering",
  "computer science|artificial intelligence|bioinformatic|statistic|applied mathematic", "Computational neuroscience",
  "cognitive psychology|cognitive science|linguistic",           "Cognitive neuroscience",
  "behavioral psychology|behavioral science|psychobiology|experimental psychology", "Behavioral neuroscience",
  "social psychology|personality psychology",                    "Social & affective neuroscience",
  "developmental (biology|psychology)|embryolog",                "Developmental neuroscience",
  "aging|gerontolog|alzheimer|parkinson",                        "Neurodegenerative disease",
  "clinical psychology|psychotherapy|mental health|psychiatry|neurology|medicine|nursing|public health|epidemiolog", "Clinical neuroscience",
  "molecular biology|cellular biology|biochemistry|genetics|immunolog|microbiolog|biophysic|chemistry", "Cellular & molecular neuroscience",
  "physiolog|anatomy|zoolog|animal",                             "Systems neuroscience",
  "audiolog|optic|ophthalm|speech|kinesiolog|physical therapy",  "Sensory & motor neuroscience"
)

SUBFIELD_UNSPECIFIED <- "General / unspecified"

# Split a ";"-separated ProQuest subject string and return ONE subfield label.
# Vectorised over the raw strings.
subfield_from_subject_terms <- function(subject_terms) {
  purrr::map_chr(subject_terms, function(s) {
    if (is.na(s) || !nzchar(s)) return(NA_character_)
    terms <- stringr::str_squish(stringr::str_split(s, ";")[[1]])
    terms <- stringr::str_to_lower(terms[nzchar(terms)])
    for (tm in terms) {
      hit <- which(stringr::str_detect(tm, SUBFIELD_TERM_MAP$pattern))
      if (length(hit)) return(SUBFIELD_TERM_MAP$subfield[hit[1]])
    }
    SUBFIELD_UNSPECIFIED
  })
}

# Deterministic subfield term-map label, plus the LLM content_area/methodology
# labels, for every Arm-V person, keyed on the LinkedIn `id`.
#
# READS FROM DISK, NOT FROM A STORE. This is what keeps the subfield figure a
# dev_figs.R job rather than a pipeline rebuild: `pq_subject_terms` already
# rides the inclusion_table.csv seam, and the dissertation text needed for the
# LLM label is in the read-only TDM export. Nothing here needs a tar_make().
#
# `extended_csv = NULL` or a missing `llm_cache_path` skips the LLM labels —
# the figure that uses `subfield_terms` still draws either way.
build_subfield_labels <- function(
    inclusion_csv   = "output/tables/inclusion_table.csv",
    extended_csv    = "data/extended.csv/extended.csv",
    llm_cache_path  = "output/tables/pq_llm_classify_cache.csv") {

  inc <- readr::read_csv(inclusion_csv, show_col_types = FALSE,
                         col_types = readr::cols(.default = readr::col_character()))
  armv <- inc |>
    dplyr::filter(tier == "A_include", !is.na(pq_url)) |>
    dplyr::transmute(
      id, pq_url, pq_subject_terms,
      # The seam carries the docview URL, not the GOID; the GOID is its last path
      # segment and is what joins to the TDM export. pq_id (the LLM cache key)
      # is the same value as character — see R/07_proquest.R::pq_url().
      pq_id = stringr::str_extract(pq_url, "[0-9]+$"),
      subfield_terms = subfield_from_subject_terms(pq_subject_terms)
    )

  if (is.null(extended_csv) || !file.exists(extended_csv) ||
      is.null(llm_cache_path) || !file.exists(llm_cache_path)) {
    armv$content_area = NA_character_
    armv$methodology   = NA_character_
    return(armv)
  }

  cached <- readr::read_csv(llm_cache_path, show_col_types = FALSE,
                            col_types = readr::cols(pq_id = readr::col_character())) |>
    dplyr::select(pq_id, content_area, methodology)

  dplyr::left_join(armv, cached, by = "pq_id")
}
