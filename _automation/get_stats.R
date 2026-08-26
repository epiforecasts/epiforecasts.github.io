## Collect weekly metrics for the group's packages and papers.
##
## Two sources of truth, both already maintained elsewhere: the r-universe
## registry plus _data/software-extras.yml decide which packages count, and
## _data/papers.bib decides which papers count. Nothing here needs its own
## hand-kept list.

openalex_mailto <- function() {
  email <- Sys.getenv("CROSSREF_EMAIL")
  if (identical(email, "")) NULL else email
}

## Citation counts are tracked for papers only. Packages rarely declare a
## citable reference: of sixteen, three do, and one of those is a CRAN package
## DOI that is almost never cited, so a package citation column would be blank
## or misleading nearly everywhere.
##
## OpenAlex is generous but rate-limits anonymous callers, so identify us where
## an address is available.
openalex_citations <- function(doi) {
  url <- paste0("https://api.openalex.org/works/doi:", utils::URLencode(doi))
  mailto <- openalex_mailto()
  if (!is.null(mailto)) url <- paste0(url, "?mailto=", mailto)
  res <- tryCatch(jsonlite::fromJSON(url), error = function(err) NULL)
  if (is.null(res) || is.null(res$cited_by_count)) {
    return(list(
      citations = NA_integer_,
      title = NA_character_,
      year = NA_integer_
    ))
  }
  list(
    citations = as.integer(res$cited_by_count),
    title = res$display_name %||% NA_character_,
    year = as.integer(res$publication_year %||% NA)
  )
}

## ---- packages --------------------------------------------------------------

## Every repo the software page shows, from the same two sources it uses.
package_repos <- function() {
  universe <- jsonlite::read_json(
    "https://github.com/epiforecasts/universe/raw/main/packages.json"
  )
  flagged <- purrr::keep(universe, ~ isTRUE(.x$display_website))
  from_universe <- purrr::map_chr(flagged, function(e) {
    sub("^https://github.com/", "", e$url)
  })

  extras <- yaml::read_yaml("_data/software-extras.yml")
  from_extras <- purrr::map_chr(extras, "repo")

  unique(c(from_universe, from_extras))
}

github_stats <- function(repo) {
  info <- tryCatch(
    gh::gh("/repos/{repo}", repo = repo),
    error = function(err) NULL
  )
  if (is.null(info)) {
    return(list(stars = NA_integer_, forks = NA_integer_, issues = NA_integer_))
  }
  list(
    stars = as.integer(info$stargazers_count %||% NA),
    forks = as.integer(info$forks_count %||% NA),
    issues = as.integer(info$open_issues_count %||% NA)
  )
}

## r-universe reports a download count per package, sourced from cranlogs. For
## a package that is not on CRAN that count is always zero, which would read as
## "nobody downloaded it" rather than "this figure does not apply", so keep the
## CRAN flag alongside it and blank the count when it does not apply.
universe_downloads <- function() {
  pkgs <- jsonlite::fromJSON(
    "https://epiforecasts.r-universe.dev/api/packages",
    simplifyVector = FALSE
  )
  stats <- purrr::map(pkgs, function(p) {
    on_cran <- isTRUE(p$`_cranurl`)
    list(
      package = p$Package,
      on_cran = on_cran,
      downloads = if (on_cran) p$`_downloads`$count %||% NA else NA
    )
  })
  ## registry names and repo names differ in case (RBi vs rbi)
  purrr::set_names(stats, tolower(purrr::map_chr(stats, "package")))
}

## Packages listed from outside the epiforecasts registry have no entry there,
## so ask cranlogs directly. A package absent from CRAN returns a zero, which
## is why the answer is only trusted when crandb knows the package.
cranlogs_downloads <- function(package) {
  known <- tryCatch(
    !is.null(
      jsonlite::fromJSON(paste0("https://crandb.r-pkg.org/", package))$Package
    ),
    error = function(err) FALSE
  )
  if (!isTRUE(known)) return(list(on_cran = FALSE, downloads = NA_integer_))

  res <- tryCatch(
    jsonlite::fromJSON(
      paste0("https://cranlogs.r-pkg.org/downloads/total/last-month/", package)
    ),
    error = function(err) NULL
  )
  count <- if (is.null(res) || length(res$downloads) == 0) {
    NA_integer_
  } else {
    as.integer(res$downloads[[1]])
  }
  list(on_cran = TRUE, downloads = count)
}

collect_package_stats <- function(on_date = Sys.Date()) {
  repos <- package_repos()
  downloads <- universe_downloads()

  rows <- purrr::map(repos, function(repo) {
    package <- basename(repo)
    gh_stats <- github_stats(repo)
    dl <- downloads[[tolower(package)]] %||% cranlogs_downloads(package)

    data.frame(
      date = as.character(on_date),
      package = package,
      repo = repo,
      stars = gh_stats$stars,
      forks = gh_stats$forks,
      open_issues = gh_stats$issues,
      on_cran = dl$on_cran,
      downloads_last_month = dl$downloads,
      stringsAsFactors = FALSE
    )
  })

  dplyr::bind_rows(rows)
}

## ---- papers ----------------------------------------------------------------

dois_from_papers <- function(bib = "_data/papers.bib") {
  lines <- readLines(bib, warn = FALSE)
  pattern <- "DOI = \\{[^}]+\\}"
  hits <- regmatches(lines, regexpr(pattern, lines, ignore.case = TRUE))
  unique(gsub("DOI = \\{|\\}", "", hits, ignore.case = TRUE))
}

collect_paper_citations <- function(on_date = Sys.Date()) {
  dois <- dois_from_papers()
  rows <- purrr::map(dois, function(doi) {
    info <- openalex_citations(doi)
    data.frame(
      date = as.character(on_date),
      doi = doi,
      title = info$title,
      year = info$year,
      citations = info$citations,
      stringsAsFactors = FALSE
    )
  })
  dplyr::bind_rows(rows)
}

## ---- writing ---------------------------------------------------------------

## Append this week's rows, keeping one row per subject per date so a rerun on
## the same day corrects rather than duplicates.
append_stats <- function(new_rows, path, key) {
  if (nrow(new_rows) == 0) return(invisible(NULL))
  ## Every fetch here turns a failure into NA rather than stopping, so a run
  ## that was rate-limited writes a row of NAs and exits cleanly. Re-running
  ## the same day must be able to repair that, without a second failed run
  ## then wiping what the good one wrote. So neither run wins outright: for
  ## each (date, key) take the newest value that is actually present, falling
  ## back to what was already recorded.
  combined <- if (file.exists(path)) {
    old <- utils::read.csv(path, stringsAsFactors = FALSE)
    dplyr::bind_rows(new_rows, old)
  } else {
    new_rows
  }
  newest_present <- function(x) {
    present <- which(!is.na(x))
    if (length(present) == 0) x[1] else x[present[1]]
  }
  combined <- combined |>
    dplyr::group_by(dplyr::across(dplyr::all_of(c("date", key)))) |>
    dplyr::summarise(
      dplyr::across(dplyr::everything(), newest_present),
      .groups = "drop"
    ) |>
    dplyr::arrange(date, .data[[key]])
  utils::write.csv(combined, path, row.names = FALSE)
  invisible(combined)
}
