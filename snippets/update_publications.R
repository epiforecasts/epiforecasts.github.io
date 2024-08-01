last_comment_pubs <- gh::gh(
  "/repos/{gh_repo}/issues/{issue_number}/comments",
  gh_repo = gh_repository,
  issue_number = 3,
  .accept = "application/vnd.github.v3.full+json"
) |>
  purrr::keep(~ .x$user$login == "github-actions[bot]") |>
  tail(1) |>
  purrr::map_chr("body_html") |>
  xml2::read_html()

last_comment_pubs |>
  xml2::xml_find_first("//details") |>
  xml2::xml_text() |>
  write("bibentries_previous_month.bib")

bibentries_previous_month <- bibtex::read.bib("bibentries_previous_month.bib")

selected <- last_comment_pubs |>
  xml2::xml_find_first("//ul") |>
  xml2::xml_find_all("//input") |>
  xml2::xml_has_attr("checked")

selected_bibentries <- bibentries_previous_month[selected]

bib_new <- vapply(selected_bibentries, format, style = "Bibtex", character(1))

bib_old <- readLines("_data/papers.bib")

bib <- paste(c(bib_old, bib_new), collapse = "\n")

write(bib, "_data/papers.bib")

## filter latest version
df <- bib2df::bib2df("_data/papers.bib") |>
  dplyr::mutate(
    version = dplyr::if_else(
      grepl("wellcomeopenres", DOI),
      sub("^.*\\.([1-9][0-9]*)$", "\\1", DOI),
      "1"
    ),
    base_doi = dplyr::if_else(
      grepl("wellcomeopenres", DOI),
      sub("\\.([1-9][0-9]*)$", "", DOI),
      DOI
    )
  ) |>
  dplyr::group_by(base_doi) |>
  dplyr::filter(version == max(version)) |>
  dplyr::ungroup() |>
  dplyr::select(-base_doi, -version)

## filter preprints
df <- df |>
  dplyr::group_by(TITLE) |>
  dplyr::mutate(n = dplyr::n()) |>
  dplyr::mutate(id = seq_len(dplyr::n())) |>
  dplyr::mutate(n_pp = sum(JOURNAL == "medRxiv")) |>
  dplyr::filter(
    !((n_pp < n) & (JOURNAL == "medRxiv"))
  ) |>
  dplyr::filter(
    !(n > 1 & id < max(id))
  ) |>
  dplyr::ungroup()
