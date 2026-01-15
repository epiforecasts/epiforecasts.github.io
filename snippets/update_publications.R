# Find all open publications issues marked as "processed" (ready to be processed)
open_issues <- gh::gh(
  "/repos/{gh_repo}/issues",
  gh_repo = gh_repository,
  state = "open",
  labels = "publications,processed",
  sort = "created",
  direction = "asc",
  per_page = 100
)

if (length(open_issues) == 0) {
  message("No open publications issues marked as 'processed' found")
  quit(save = "no")
}

message(sprintf("Found %d issue(s) ready to process", length(open_issues)))

# Process each issue and collect selected papers
all_selected_bibs <- list()

for (issue in open_issues) {
  issue_number <- issue$number
  message(sprintf("Processing issue #%d", issue_number))

  # Get HTML body
  issue_html <- issue$body_html
  if (is.null(issue_html)) {
    issue <- gh::gh(
      "/repos/{gh_repo}/issues/{issue_number}",
      gh_repo = gh_repository,
      issue_number = issue_number,
      .accept = "application/vnd.github.v3.full+json"
    )
    issue_html <- issue$body_html
  }

  parsed_html <- xml2::read_html(issue_html)

  # Extract BibTeX from the code block inside <details>
  bib_text <- parsed_html |>
    xml2::xml_find_first("//details//pre") |>
    xml2::xml_text()

  # Fallback to <details> directly
  if (is.na(bib_text) || nchar(trimws(bib_text)) == 0) {
    bib_text <- parsed_html |>
      xml2::xml_find_first("//details") |>
      xml2::xml_text()
  }

  bib_text <- trimws(bib_text)

  if (is.na(bib_text) || nchar(bib_text) == 0) {
    message(sprintf("  No BibTeX content found in issue #%d, closing", issue_number))
    gh::gh(
      "PATCH /repos/{gh_repo}/issues/{issue_number}",
      gh_repo = gh_repository,
      issue_number = issue_number,
      state = "closed",
      labels = list("publications", "processed")
    )
    next
  }

  # Write and read bib entries
  write(bib_text, "bibentries_temp.bib")
  bibentries <- bibtex::read.bib("bibentries_temp.bib")

  # Find selected (checked) papers
  selected <- parsed_html |>
    xml2::xml_find_first("//ul") |>
    xml2::xml_find_all(".//input") |>
    xml2::xml_has_attr("checked")

  if (!any(selected)) {
    message(sprintf("  No papers selected in issue #%d, closing", issue_number))
  } else {
    selected_bibentries <- bibentries[selected]
    message(sprintf("  Found %d selected paper(s)", length(selected_bibentries)))
    all_selected_bibs <- c(all_selected_bibs, selected_bibentries)
  }

  # Close the issue and add "processed" label
  gh::gh(
    "PATCH /repos/{gh_repo}/issues/{issue_number}",
    gh_repo = gh_repository,
    issue_number = issue_number,
    state = "closed",
    labels = list("publications", "processed")
  )
}

# Clean up temp file
unlink("bibentries_temp.bib")

if (length(all_selected_bibs) == 0) {
  message("No papers selected across all issues")
  quit(save = "no")
}

message(sprintf("Adding %d paper(s) to papers.bib", length(all_selected_bibs)))

# Add all selected papers to bib file
bib_new <- vapply(all_selected_bibs, format, style = "Bibtex", character(1))
bib_old <- readLines("_data/papers.bib")
bib <- paste(c(bib_old, bib_new), collapse = "\n")
write(bib, "_data/papers.bib")

## filter latest version
df <- bib2df::bib2df("_data/papers.bib") |>
  ## remove accumulated junk columns from previous runs
  dplyr::select(-dplyr::matches("^(N|ID|N_PP)(\\.\\d+)?$")) |>
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
  dplyr::ungroup() |>
  dplyr::select(-n, -id, -n_pp)

bib2df::df2bib(df, file = "_data/papers.bib")
