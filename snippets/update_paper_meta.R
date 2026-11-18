## Keep _data/papers.bib and _data/paper-open.yml up to date between the
## weekly publication updates: replace listed preprints that have since been
## published, and suggest open research links for papers that have none yet.
## Everything this changes goes into the weekly pull request for review.

source("_automation/get_new_papers.R")
source("_automation/paper_meta.R")

bib_path <- "_data/papers.bib"
meta_path <- "_data/paper-open.yml"

preprint_journals <- c("medRxiv", "bioRxiv", "arXiv", "OSF Preprints")

n_before <- length(bibtex::read.bib(bib_path))
bib <- bib2df::bib2df(bib_path) |>
  dplyr::select(-dplyr::matches("^(N|ID|N_PP)(\\.\\d+)?$")) |>
  dplyr::mutate(DOI = tolower(DOI))
meta <- read_paper_meta(meta_path)

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

bib2df::df2bib(bib, file = bib_path)
if (length(new_entries) > 0) {
  ## read in full before writing: write() and df2bib() open the file, and so
  ## empty it, before they evaluate what they are given
  current <- readLines(bib_path)
  write(c(current, new_entries), bib_path)
  ## the same normalisation update_publications.R applies
  normalised <- bib2df::bib2df(bib_path) |>
    dplyr::select(-dplyr::matches("^(N|ID|N_PP)(\\.\\d+)?$"))
  bib2df::df2bib(normalised, file = bib_path)
}

## Suggestions for papers that have no entry yet. A paper with nothing to
## suggest is still recorded, so it is not looked up again every week.
dois <- tolower(vapply(bibtex::read.bib(bib_path), function(e) e$doi, ""))
## each swap replaces one entry with one, or drops a duplicate, so a bigger
## loss means the file was damaged; stop before the links are pruned to match
if (length(dois) < n_before - length(published)) {
  stop("papers.bib has ", length(dois), " entries, expected at least ",
       n_before - length(published), "; leaving the files for inspection")
}
for (doi in setdiff(dois, names(meta))) {
  message("Suggesting links for ", doi)
  meta[[doi]] <- suggest_paper_meta(doi)
}
## a paper that has left the list takes its links with it
meta <- meta[names(meta) %in% dois]

write_paper_meta(meta, meta_path)
