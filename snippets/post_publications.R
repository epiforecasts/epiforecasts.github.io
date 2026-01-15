options(width = 10000) # do not hard-wrap formatted citations

source("_automation/get_new_papers.R")
month_1st <- format(lubridate::floor_date(lubridate::today() - 1, "month"))
new_papers <- get_new_papers_team(from_date = month_1st)

# Get DOIs from open OR processed publications issues to avoid duplicates
# "processed" label marks issues that have been reviewed (even if no papers added)
open_issues <- gh::gh(
 "/repos/{gh_repo}/issues",
 gh_repo = gh_repository,
 state = "all",
 labels = "publications",
 per_page = 100
) |>
 purrr::keep(~ .x$state == "open" || "processed" %in% purrr::map_chr(.x$labels, "name"))

pending_dois <- character(0)
if (length(open_issues) > 0) {
 # Extract DOIs from issue bodies using regex
 pending_dois <- open_issues |>
   purrr::map_chr("body", .default = "") |>
   purrr::map(~ regmatches(.x, gregexpr("doi\\s*=\\s*\\{([^}]+)\\}", .x, ignore.case = TRUE))) |>
   purrr::map(~ gsub("doi\\s*=\\s*\\{|\\}", "", .x, ignore.case = TRUE)) |>
   unlist() |>
   unique()
}

# Filter out papers already in open issues
if (nrow(new_papers) > 0 && length(pending_dois) > 0) {
 new_papers <- new_papers |>
   dplyr::filter(!(doi %in% pending_dois))
}

if (nrow(new_papers) > 0) {
  bibentries_new <- create_bibentries(new_papers)
  saveRDS(bibentries_new, paste0("bibentries_previous_month.rds"))

  bibentries_new_checklist <- paste(
    "- [ ]", vapply(bibentries_new, format, character(1))
  )
  bibentries_body <- paste(
    c(
      "Papers published last month. Check the boxes for papers to add, then the next monthly run will create a PR.",
      "",
      bibentries_new_checklist,
      "<details>",
      "",
      "```",
      vapply(bibentries_new, format, character(1), "bibtex"),
      "```",
      "</details>"
    ),
    collapse = "\n"
  )

  issue_title <- paste(
    "Publications update -",
    format(lubridate::today(), "%B %Y")
  )

  gh::gh(
    "POST /repos/{gh_repo}/issues",
    gh_repo = gh_repository,
    title = issue_title,
    body = bibentries_body,
    labels = list("publications")
  )
}
