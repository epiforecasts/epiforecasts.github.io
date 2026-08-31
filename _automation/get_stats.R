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

## Citation counts are tracked for papers only. Few packages declare a citable
## reference, and a CRAN package DOI is almost never cited, so the column would
## be blank or misleading for most of them.
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
## The name a package declares for itself, which is what CRAN knows it by.
## Falls back to the repository name, which is usually the same.
package_name_from_description <- function(repo) {
  raw <- tryCatch(
    gh::gh("/repos/{repo}/contents/DESCRIPTION", repo = repo,
           .accept = "application/vnd.github.raw"),
    error = function(err) NULL
  )
  if (is.null(raw)) return(basename(repo))
  text <- if (is.raw(raw)) rawToChar(as.raw(raw)) else as.character(raw)
  hit <- regmatches(text, regexpr("(?m)^Package:[ \t]*\\S+", text, perl = TRUE))
  if (length(hit) == 0) return(basename(repo))
  trimws(sub("^Package:", "", hit[[1]]))
}

## Returns repository slugs named by the package name the registry uses. The
## two differ in case for RBi/rbi, and crandb is case-sensitive, so the
## registry name is the one to ask CRAN about.
package_repos <- function() {
  universe <- tryCatch(
    jsonlite::read_json(
      "https://github.com/epiforecasts/universe/raw/main/packages.json"
    ),
    error = function(err) NULL
  )
  if (is.null(universe)) {
    message("  registry unreachable, skipping package metrics")
    return(character(0))
  }
  flagged <- purrr::keep(universe, ~ isTRUE(.x$display_website))
  from_universe <- purrr::set_names(
    purrr::map_chr(flagged, ~ sub("^https://github.com/", "", .x$url)),
    purrr::map_chr(flagged, "package")
  )

  ## Extras have no registry entry, and it is exactly these that fall through
  ## to crandb, which is case-sensitive. The repo name is not reliably the
  ## package name (sbfnk/RBi declares Package: rbi), so read DESCRIPTION.
  extras <- yaml::read_yaml("_data/software-extras.yml")
  extra_repos <- purrr::map_chr(extras, "repo")
  from_extras <- purrr::set_names(
    extra_repos,
    purrr::map_chr(extra_repos, package_name_from_description)
  )

  both <- c(from_universe, from_extras)
  both[!duplicated(both)]
}

## `open_issues_count` counts open issues *plus* open pull requests, so
## subtract the pull requests rather than record the sum under a column called
## issues. The search endpoint separates them directly but will not do here:
## it does not follow repository renames, and three registry URLs point at
## repositories that have since moved. Both endpoints used below do follow.
open_pull_requests <- function(repo) {
  prs <- tryCatch(
    gh::gh("/repos/{repo}/pulls", repo = repo, state = "open",
           per_page = 100, .limit = Inf),
    error = function(err) NULL
  )
  if (is.null(prs)) NA_integer_ else length(prs)
}

github_stats <- function(repo) {
  info <- tryCatch(
    gh::gh("/repos/{repo}", repo = repo),
    error = function(err) NULL
  )
  if (is.null(info)) {
    ## Not the registry slug: three of those are stale and redirect, so
    ## recording one as the join key would write a name the site cannot
    ## match, and being present it would beat the canonical name a later
    ## run recovers. Unknown has to stay unknown so a rerun can repair it.
    return(list(
      full_name = NA_character_, stars = NA_integer_,
      forks = NA_integer_, issues = NA_integer_
    ))
  }
  issues_and_prs <- as.integer(info$open_issues_count %||% NA)
  prs <- open_pull_requests(repo)
  list(
    ## the canonical name, which is what the website sees; three registry
    ## URLs are stale slugs that redirect
    full_name = info$full_name %||% repo,
    stars = as.integer(info$stargazers_count %||% NA),
    forks = as.integer(info$forks_count %||% NA),
    issues = issues_and_prs - prs
  )
}

## The only download source. r-universe also reports a count, but it is the
## figure from whenever it last indexed the package rather than a current
## one, so the same number can repeat for weeks or differ from cranlogs by a
## quarter. Asking cranlogs directly gives one source and a current window.
cranlogs_downloads <- function(package) {
  ## Ask by status code rather than by whether the body parses: crandb throws
  ## on a 404 as well as on a network failure, and those mean different
  ## things. A 404 is "not on CRAN"; anything else unexpected is "we do not
  ## know", which must not be recorded as though we do.
  crandb <- tryCatch(
    curl::curl_fetch_memory(paste0("https://crandb.r-pkg.org/", package)),
    error = function(err) NULL
  )
  if (is.null(crandb) || !crandb$status_code %in% c(200L, 404L)) {
    return(list(on_cran = NA, downloads = NA_integer_))
  }
  if (identical(crandb$status_code, 404L)) {
    return(list(on_cran = FALSE, downloads = NA_integer_))
  }

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
  if (length(repos) == 0) return(data.frame())

  rows <- purrr::imap(repos, function(repo, package) {
    gh_stats <- github_stats(repo)
    dl <- cranlogs_downloads(package)

    data.frame(
      date = as.character(on_date),
      package = package,
      repo = gh_stats$full_name,
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
  ## A failed fetch records NA and exits cleanly, so a rate-limited run
  ## writes a row of NAs that a later run must be able to repair, without a
  ## second failed run wiping what a good one wrote. For each (date, key),
  ## take the newest value that is actually present.
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
