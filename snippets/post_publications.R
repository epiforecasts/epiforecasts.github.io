options(width = 10000) # do not hard-wrap formatted citations

source("_automation/get_new_papers.R")
month_1st <- format(lubridate::floor_date(lubridate::today() - 1, "month"))
new_papers <- get_new_papers_team(from_date = month_1st)

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
