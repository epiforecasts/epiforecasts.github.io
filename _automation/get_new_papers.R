`%>%` <- magrittr::`%>%`

get_new_papers_orcid <- function(orcid, from_date) {
  
  rcrossref::cr_works(filter = list(orcid = orcid,
                                    from_deposit_date = from_date,
                                    type = "journal-article")) %>%
    purrr::pluck("data")
  
}

get_new_papers_team <- function(from_date) {
  
  recent_papers <- fs::dir_ls("_data/team", regexp = "\\w+\\-\\w+\\.yml") %>%
    purrr::map(yaml::read_yaml) %>%
    purrr::keep(function(x) x[["current-member"]]) %>%
    purrr::map("orcid") %>%
    purrr::keep(~ !is.null(.x)) %>%
    purrr::map_dfr(get_new_papers_orcid, from_date = from_date) %>%
    dplyr::distinct()
  
  return(recent_papers)
  
}

create_bibentries <- function(papers) {
  
  papers %>%
    dplyr::mutate(
      author = purrr::map(author, ~ paste(.x$given, .x$family)),
      author = purrr::map(author, as.person),
      year = format(lubridate::parse_date_time(issued, c("Y-m-d", "Y-m")), "%Y")
    ) %>%
    dplyr::rowwise() %>%
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
    ) %>%
    dplyr::pull(bib)
  
}
