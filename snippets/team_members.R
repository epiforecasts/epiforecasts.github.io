team <- fs::dir_ls("_data/team", regexp = "\\w+\\-\\w+\\.yml") |>
  purrr::map(yaml::read_yaml)

# Put Seb first (group leader)
team <- c(
  team[which(purrr::map(team, "name") == "Sebastian Funk")],
  team[-which(purrr::map(team, "name") == "Sebastian Funk")]
)
current_team <- team |>
  purrr::keep(function(x) x[["current-member"]])
