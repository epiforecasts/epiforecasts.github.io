get_new_papers_orcid <- function(orcid, from_date) {
  refs <- rcrossref::cr_works(filter = list(
    orcid = orcid,
    from_deposit_date = from_date
  )) |>
    purrr::pluck("data")

  if (nrow(refs) > 0) {
    if (!("container.title" %in% colnames(refs))) {
      refs <- refs |>
        dplyr::mutate(container.title = NA_character_)
    }
    refs <- refs |>
      dplyr::mutate(
        container.title = dplyr::if_else(
          publisher == "Cold Spring Harbor Laboratory",
          "medRxiv",
          container.title
        ),
        type = dplyr::if_else(
          publisher == "Cold Spring Harbor Laboratory",
          "journal-article",
          type
        )
      ) |>
      dplyr::filter(type == "journal-article") |>
      ## filter out old papers that were re-deposited
      dplyr::mutate(
        pub_year = as.numeric(format(
          lubridate::parse_date_time(issued, c("Y-m-d", "Y-m", "Y")), "%Y"
        ))
      ) |>
      dplyr::filter(pub_year >= as.numeric(format(Sys.Date(), "%Y")) - 1) |>
      dplyr::select(-pub_year)
    existing_papers <- dois_from_bib("_data/papers.bib")
    refs <- refs |>
      dplyr::filter(!(doi %in% existing_papers))
  }
  return(refs)
}

get_new_papers_team <- function(from_date) {
  
  recent_papers <- fs::dir_ls("_data/team", regexp = "\\w+\\-\\w+\\.yml") |>
    purrr::map(yaml::read_yaml) |>
    purrr::keep(\(x) {
      any(purrr::map_lgl(x$position, \(y) {
        is.null(y$end) || y$end > Sys.Date()
      }))
    })  |>
    purrr::map("orcid") |>
    purrr::keep(~ !is.null(.x)) |>
    purrr::map_dfr(get_new_papers_orcid, from_date = from_date) |>
    dplyr::distinct()
  
  return(recent_papers)
  
}

create_bibentries <- function(papers) {
  papers |>
    dplyr::mutate(
      author = purrr::map(author, ~ dplyr::filter(.x, !is.na(given) &!is.na(family))),
      author = purrr::map(author, ~ paste(.x$given, .x$family)),
      author = purrr::map(author, as.person),
      year = format(lubridate::parse_date_time(issued, c("Y-m-d", "Y-m", "Y")), "%Y")
    ) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      bib = list(bibentry(
        "Article", 
        title = title, 
        journal = container.title, 
        author = author, 
        year = year,
        doi = doi,
        key = ids::adjective_animal()
      ))
    ) |>
    dplyr::pull(bib)
  
}

dois_from_bib <- function(bib) {
  bib |>
    bibtex::read.bib() |>
    purrr::map_chr(~ .x$doi)
}
