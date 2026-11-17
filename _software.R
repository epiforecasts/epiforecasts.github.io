# Shared code for the software pages (software.qmd for R, julia.qmd for
# Julia): fetching package details from GitHub and rendering them.

library(magrittr)

# Hide software not updated in this many months (set to NULL to disable)
stale_months <- 24

# Team members keyed by GitHub username, to pick out group contributors
load_team_by_github <- function() {
  fs::dir_ls("_data/team", regexp = "\\w+\\-\\w+\\.yml") |>
    purrr::map(yaml::read_yaml) %>%
    purrr::keep(~ !is.null(.x$github) && .x$github != "") %>%
    purrr::set_names(purrr::map_chr(., "github"))
}

# Weekly CRAN download figures, collected by
# .github/workflows/update-stats.yaml. Absent until that job has run at least
# once, so treated as optional.
load_downloads <- function() {
  # The weekly job records to the stats-data branch. Read from there, falling
  # back to a local file so the page still renders before the branch exists
  # or if the fetch fails.
  stats_url <- paste0(
    "https://raw.githubusercontent.com/epiforecasts/epiforecasts.github.io/",
    "stats-data/_data/package-stats.csv"
  )
  stats <- tryCatch(
    read.csv(stats_url, stringsAsFactors = FALSE),
    error = function(err) NULL
  )
  if (is.null(stats) && file.exists("_data/package-stats.csv")) {
    stats <- read.csv("_data/package-stats.csv", stringsAsFactors = FALSE)
  }
  if (is.null(stats) || nrow(stats) == 0) {
    return(list())
  }

  # Each repository's most recent figure: a week where cranlogs is
  # unreachable writes NAs for everything, and the previous week's figures
  # are still usable. Bounded so a long outage shows nothing rather than
  # something months out of date.
  stats <- stats[!is.na(stats$downloads_last_month), ]
  stats <- stats[as.Date(stats$date) > Sys.Date() - 35, ]
  stats <- stats[order(stats$date, decreasing = TRUE), ]
  stats <- stats[!duplicated(tolower(stats$repo)), ]
  # keyed by repository, not package name: the two can differ and the
  # repository is what both sides can agree on
  split(stats$downloads_last_month, tolower(stats$repo))
}

fetch_package_info <- function(repo, team_by_github) {
  repo_info <- tryCatch(
    gh::gh("/repos/{repo}", repo = repo),
    error = function(err) list(
      name = basename(repo),
      description = "No description available",
      html_url = paste0("https://github.com/", repo),
      stargazers_count = 0,
      language = NA,
      pushed_at = NA
    )
  )

  contributors <- tryCatch(
    gh::gh("/repos/{repo}/contributors", repo = repo, per_page = 100),
    error = function(err) list()
  )

  team_contributors <- contributors %>%
    purrr::keep(~ .x$login %in% names(team_by_github)) %>%
    purrr::map(function(c) {
      tm <- team_by_github[[c$login]]
      list(
        name = tm$name,
        github = c$login,
        avatar_url = c$avatar_url
      )
    })

  # R packages keep their logo in man/figures, Julia packages in the
  # Documenter assets folder. There the SVG is the sticker, while a PNG
  # alongside it can be a different mark.
  find_file <- function(paths) {
    for (f in paths) {
      hit <- tryCatch(
        gh::gh("/repos/{repo}/contents/{f}", repo = repo, f = f),
        error = function(err) NULL
      )
      if (!is.null(hit$download_url)) {
        return(list(path = f, url = hit$download_url))
      }
    }
    NULL
  }
  logo <- find_file(c(
    "man/figures/logo.png", "man/figures/logo.svg",
    "docs/src/assets/logo.svg", "docs/src/assets/logo.png"
  ))
  logo_url <- logo$url %||% NA_character_
  # A dark-mode variant sits next to the logo, as pkgdown's logo-dark does
  logo_dark_url <- NA_character_
  if (!is.null(logo)) {
    dark <- find_file(file.path(
      dirname(logo$path), c("logo-dark.svg", "logo-dark.png")
    ))
    logo_dark_url <- dark$url %||% NA_character_
  }

  list(
    Package = repo_info$name,
    logo_url = logo_url,
    logo_dark_url = logo_dark_url,
    description = if (nzchar(repo_info$description %||% "")) {
      repo_info$description
    } else {
      "No description available"
    },
    github_url = repo_info$html_url,
    homepage = repo_info$homepage %||% "",
    stars = repo_info$stargazers_count %||% 0,
    language = repo_info$language,
    updated = if (!is.null(repo_info$pushed_at)) substr(repo_info$pushed_at, 1, 10) else NA,
    team_contributors = team_contributors
  )
}

# Packages listed in a YAML file rather than a registry
fetch_listed_packages <- function(path, team_by_github) {
  purrr::map(yaml::read_yaml(path), function(e) {
    info <- fetch_package_info(e$repo, team_by_github)
    # marks a package belonging to a separate community project
    info$part_of <- e$part_of
    if (!is.null(e$description)) info$description <- e$description
    info$hex <- !isFALSE(e$hex)
    if (!is.null(e$logo)) {
      info$logo_url <- paste0(
        "https://raw.githubusercontent.com/", e$repo, "/HEAD/", e$logo
      )
      # a dark variant found at the default path would belong to another logo
      info$logo_dark_url <- NA_character_
    }
    # not in our r-universe, so there is no page there to link to
    info$from_extras <- TRUE
    info
  })
}

# Drop stale packages and sort by stars
tidy_packages <- function(packages) {
  packages %>%
    purrr::keep(~ !is.null(.x)) %>%
    purrr::keep(~ {
      if (is.null(stale_months) || is.na(.x$updated)) return(TRUE)
      as.Date(.x$updated) > Sys.Date() - (stale_months * 30)
    }) %>%
    { .[order(purrr::map_dbl(., "stars"), decreasing = TRUE)] }
}

# A logo as one <img>, or as a light and a dark one when the package has a
# dark-mode variant; styles.css shows the one matching the colour scheme
logo_img <- function(e, alt, attrs = "") {
  light <- sprintf('<img src="%s" alt="%s"%s>', e$logo_url, alt, attrs)
  if (is.null(e$logo_dark_url) || is.na(e$logo_dark_url)) {
    return(light)
  }
  paste0(
    sprintf('<img class="logo-for-light" src="%s" alt="%s"%s>', e$logo_url, alt, attrs),
    sprintf('<img class="logo-for-dark" src="%s" alt="%s"%s>', e$logo_dark_url, alt, attrs)
  )
}

# Honeycomb index: every package with a hex logo, in offset rows so the hexes
# tessellate. Rows alternate in length by one, which is what produces the
# half-width stagger without any positioning maths.
hex_wall <- function(packages) {
  with_logo <- purrr::keep(
    packages, ~ !is.na(.x$logo_url) && !isFALSE(.x$hex)
  )
  if (length(with_logo) == 0) {
    return(invisible())
  }

  # Adjacent rows must differ by exactly one hex, which is what offsets them
  # by half a width and makes the shapes interlock. Enumerate the row patterns
  # that satisfy that and sum exactly, preferring a symmetric one so the
  # widest row sits in the middle.
  patterns <- function(total, rows) {
    out <- list()
    for (first in 7:3) {
      steps <- as.matrix(expand.grid(rep(list(c(-1, 1)), rows - 1)))
      for (k in seq_len(nrow(steps))) {
        widths <- cumsum(c(first, steps[k, ]))
        if (sum(widths) == total && min(widths) >= 3) {
          out[[length(out) + 1]] <- as.integer(widths)
        }
      }
    }
    out
  }

  n <- length(with_logo)
  options <- c(patterns(n, 3), patterns(n, 4), patterns(n, 2))
  palindromic <- purrr::keep(options, ~ identical(.x, rev(.x)))

  sizes <- if (length(palindromic) > 0) {
    palindromic[[1]]
  } else if (length(options) > 0) {
    options[[1]]
  } else {
    # nothing interlocks exactly, so fill rows greedily and accept the shape
    widths <- integer(0)
    left <- n
    wide <- FALSE
    while (left > 0) {
      take <- min(if (wide) 6 else 5, left)
      widths <- c(widths, take)
      left <- left - take
      wide <- !wide
    }
    widths
  }

  idx <- 0
  rows <- character(0)
  for (n in sizes) {
    cells <- character(0)
    for (i in seq_len(n)) {
      idx <- idx + 1
      e <- with_logo[[idx]]
      cells <- c(cells, sprintf(
        '<a href="%s" title="%s">%s</a>',
        e$github_url, e$Package, logo_img(e, paste(e$Package, "hex logo"))
      ))
    }
    rows <- c(rows, paste0(
      '<div class="hex-row">', paste(cells, collapse = ""), '</div>'
    ))
  }

  cat(paste0(
    '<div class="hex-wall">',
    paste(rows, collapse = ""), '</div>\n\n'
  ))
}

# Package cards
render_packages <- function(packages, downloads_by_package = list()) {
  packages %>%
    purrr::map_chr(function(e) {
      contributors_section <- ""
      if (length(e$team_contributors) > 0) {
        contributor_items <- purrr::map_chr(e$team_contributors, function(c) {
          sprintf(
            '<a href="https://github.com/%s" title="%s"><img src="%s" alt="%s" width="32" height="32"></a>',
            c$github, c$name, c$avatar_url, c$name
          )
        })
        contributors_section <- paste(contributor_items, collapse = " ")
      }

      logo_html <- ""
      if (!is.na(e$logo_url)) {
        logo_html <- sprintf(
          '<div class="pkg-logo">%s</div>',
          logo_img(e, paste(e$Package, "logo"), ' loading="lazy"')
        )
      }

      part_of_html <- ""
      if (!is.null(e$part_of)) {
        part_of_html <- sprintf(
          '<div class="pkg-part-of">Part of <a href="%s">%s</a></div>',
          e$part_of$url, e$part_of$name
        )
      }

      slug <- sub("^https://github.com/", "", e$github_url)
      downloads <- downloads_by_package[[tolower(slug)]]
      # Naming the source matters: a package with no CRAN figure is
      # distributed elsewhere, not unused.
      downloads_text <- if (!is.null(downloads)) {
        paste(
          formatC(downloads, big.mark = ",", format = "d"),
          "CRAN downloads/month"
        )
      } else if (isTRUE(e$from_extras)) {
        # Only link where the registry is known. A registry package is
        # served by the epiforecasts universe whatever repo it lives in, so
        # epimixr at sbfnk/epimixr still resolves there. A listed package
        # could be in any universe or none.
        ""
      } else {
        # single-quoted attribute: substituted into R source
        sprintf(
          "<a href='https://epiforecasts.r-universe.dev/%s'>On r-universe</a>",
          e$Package
        )
      }

      # a GitHub homepage that is the repository itself adds nothing
      docs_text <- ""
      if (nzchar(e$homepage) && !startsWith(e$homepage, e$github_url)) {
        docs_text <- sprintf("<a href='%s'>Documentation</a>", e$homepage)
      }

      knitr::knit_expand(
        "_software-item.Rmd",
        Package = e$Package,
        downloads_text = downloads_text,
        logo_html = logo_html,
        part_of_html = part_of_html,
        description = e$description,
        github_url = e$github_url,
        language = e$language %||% "",
        updated = e$updated %||% "",
        contributors_section = contributors_section,
        docs_text = docs_text
      )
    }) %>%
    { knitr::knit_child(text = unlist(.), quiet = TRUE) } %>%
    cat(sep = "\n")
}
