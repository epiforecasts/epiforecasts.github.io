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
  bibentries_comment <- paste(
    c(
      "Papers published last month:",
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

  gh::gh(
    "POST /repos/{gh_repo}/issues/{issue_number}/comments",
    gh_repo = "${{ github.repository }}",
    issue_number = 3,
    body = bibentries_comment
  )
}
