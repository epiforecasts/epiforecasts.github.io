`%>%` <- magrittr::`%>%`

get_new_papers_orcid <- function(orcid, from_date) {
  
  rcrossref::cr_works(filter = list(orcid = '0000-0002-7750-5280',
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
      author = purrr::map(author, ~ ifelse(is.na(.x$name), paste(.x$given, .x$family), .x$name)),
      author = purrr::map(author, as.person),
      year = format(lubridate::ymd(published.online), "%Y")
    ) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      bib = list(bibentry(
        "Article", 
        title = title, 
        journal = container.title, 
        author = author, 
        year = year,
        doi = doi
      ))
    ) %>%
    dplyr::pull(bib)
  
}
