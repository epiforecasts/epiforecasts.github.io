team <- fs::dir_ls("_data/team", regexp = "\\w+\\-\\w+\\.yml") |>
  purrr::map(yaml::read_yaml)

# Put Seb first (group leader)
team <- c(
  team[which(purrr::map(team, "name") == "Sebastian Funk")],
  team[-which(purrr::map(team, "name") == "Sebastian Funk")]
)

## keep current team members
current_team <- team |>
  purrr::keep(\(x) {
    any(purrr::map_lgl(x$position, \(y) {
      is.null(y$end) || y$end > Sys.Date()
    }))
  })
