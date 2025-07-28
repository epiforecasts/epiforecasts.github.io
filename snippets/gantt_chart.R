library("plotrix")
library("readr")
library("tidyr")
library("dplyr")
library("lubridate")

roles <- c("Staff", "PhD", "Visitor")

team <- fs::dir_ls("_data/team", regexp = "\\w+\\-\\w+\\.yml") |>
  purrr::map(yaml::read_yaml)

# Put Seb first (group leader)
team <- c(
  team[which(purrr::map(team, "name") == "Sebastian Funk")],
  team[-which(purrr::map(team, "name") == "Sebastian Funk")]
)

## Convert to data frame
df <- team |>
  purrr::map_df(\(x) {
    ldf <- purrr::map_df(x$position, as.data.frame) |>
      bind_rows() |>
      mutate(
        name = x$name
      )
  }) |>
  mutate(
    start = as.Date(start),
    end = as.Date(end)
  ) |>
  replace_na(list(end = today()))

vgridpos <- seq(floor_date(min(df$start), "year"), today(), by = "year")
vgridlab <- year(vgridpos)

colfunc <- colorRampPalette(
  c("#762a83", "#af8dc3","#e7d4e8","#d9f0d3","#7fbf7b","#1b7837")
)

timeframe <- c(min(df$start), today())

gantt_info <- df |>
  arrange(start) |>
  rename(starts = start, ends = end, labels = name) |>
  mutate(priorities = as.integer(factor(type, levels = roles))) |>
  select(-type)

#Plot and save your Gantt chart into PDF form
gantt.chart(
  gantt_info,
  taskcolors = colfunc(6),
  xlim = timeframe,
  priority.legend = FALSE,
  vgridpos = vgridpos,
  vgridlab = vgridlab,
  hgrid = TRUE,
  half.height = 0.45,
  cylindrical = FALSE,
  border.col = "black",
  label.cex = 0.8,
  priority.label = "Role",
  priority.extremes = c("Staff", "Visitor"),
  time.axis = 1
)
legend(
  "bottomleft",
  roles,
  fill = colfunc(6),
  inset = .1
)
