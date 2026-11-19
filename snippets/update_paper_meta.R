## Keep _data/papers.bib and _data/paper-open.yml up to date between the
## weekly publication updates: replace listed preprints that have since been
## published, and suggest open research links for papers that have none yet.
## Everything this changes goes into the weekly pull request for review.
##
## The step before this one has already added the week's papers to
## papers.bib and closed their issue, so this step must never fail the run:
## it works on copies, writes the real files only once every check has
## passed, and on any error leaves both files as they were.

source("_automation/get_new_papers.R")
source("_automation/paper_meta.R")

bib_path <- "_data/papers.bib"
meta_path <- "_data/paper-open.yml"

preprint_journals <- c("medRxiv", "bioRxiv", "arXiv", "OSF Preprints")

update_paper_meta <- function(bib_file, meta_file) {
  n_before <- length(bibtex::read.bib(bib_file))
  bib <- bib2df::bib2df(bib_file) |>
    dplyr::select(-dplyr::matches("^(N|ID|N_PP)(\\.\\d+)?$")) |>
    dplyr::mutate(DOI = tolower(DOI))
  meta <- read_paper_meta(meta_file)

  ## A listed preprint that has been published is replaced by the published
  ## paper, which then links to the preprint
  preprints <- bib$DOI[bib$JOURNAL %in% preprint_journals]
  published <- purrr::set_names(
    purrr::map_chr(preprints, published_version), preprints
  )
  published <- published[!is.na(published)]

  new_entries <- character(0)
  for (preprint in names(published)) {
    paper <- published[[preprint]]
    message("Preprint ", preprint, " was published as ", paper)
    if (!(paper %in% bib$DOI)) {
      refs <- cr_works_retry(dois = paper)
      if (nrow(refs) == 0) next
      new_entries <- c(new_entries, vapply(
        create_bibentries(refs),
        function(b) paste(format(b, style = "Bibtex"), collapse = "\n"),
        ""
      ))
    }
    bib <- bib[bib$DOI != preprint, ]
    fields <- utils::modifyList(meta[[preprint]] %||% list(), meta[[paper]] %||% list())
    fields$preprint <- fields$preprint %||% paste0("https://doi.org/", preprint)
    meta[[paper]] <- fields
    meta[[preprint]] <- NULL
  }

  bib2df::df2bib(bib, file = bib_file)
  if (length(new_entries) > 0) {
    ## read in full before writing: write() and df2bib() open the file, and
    ## so empty it, before they evaluate what they are given
    current <- readLines(bib_file)
    write(c(current, new_entries), bib_file)
    ## the same normalisation update_publications.R applies
    normalised <- bib2df::bib2df(bib_file) |>
      dplyr::select(-dplyr::matches("^(N|ID|N_PP)(\\.\\d+)?$"))
    bib2df::df2bib(normalised, file = bib_file)
  }

  dois <- tolower(vapply(bibtex::read.bib(bib_file), function(e) e$doi, ""))
  ## each swap replaces one entry with one, or drops a duplicate, so a bigger
  ## loss means the file was damaged
  if (length(dois) < n_before - length(published)) {
    stop("papers.bib has ", length(dois), " entries, expected at least ",
         n_before - length(published))
  }

  ## Suggestions for papers that have no entry yet, and again for those still
  ## pending because their full text was not available. A paper whose lookup
  ## fails is tried again next week. Links already recorded, whether found
  ## earlier or edited by hand, are kept over new suggestions.
  pending <- names(meta)[vapply(meta, function(m) isTRUE(m$pending), TRUE)]
  for (doi in union(setdiff(dois, names(meta)), intersect(pending, dois))) {
    message("Suggesting links for ", doi)
    found <- suggest_paper_meta(doi)
    if (is.null(found)) next
    recorded <- meta[[doi]] %||% list()
    recorded$pending <- NULL
    meta[[doi]] <- utils::modifyList(found, recorded)
  }
  ## a paper that has left the list takes its links with it
  meta <- meta[names(meta) %in% dois]

  write_paper_meta(meta, meta_file)
}

bib_copy <- tempfile(fileext = ".bib")
meta_copy <- tempfile(fileext = ".yml")
file.copy(bib_path, bib_copy)
file.copy(meta_path, meta_copy)

done <- tryCatch(
  {
    update_paper_meta(bib_copy, meta_copy)
    TRUE
  },
  error = function(e) {
    ## a GitHub Actions warning, shown on the run without failing it
    ## purrr wraps the error, so report the one underneath as well
    reason <- conditionMessage(e)
    if (!is.null(e$parent)) reason <- paste(reason, conditionMessage(e$parent))
    message("::warning::Paper link update skipped: ", gsub("\n", " ", reason))
    FALSE
  }
)
if (done) {
  file.copy(bib_copy, bib_path, overwrite = TRUE)
  file.copy(meta_copy, meta_path, overwrite = TRUE)
}
