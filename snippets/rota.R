library("lubridate")
library("dplyr")

source(here::here("snippets", "team_members.R"))

shift <- function(x) {
  c(x[2:length(x)], x[1])
}

names <- unname(unlist((purrr::map(current_team, ~.x$name))))

summer_break <- data.frame(
  Date = ymd("2024-08-08") + 0:3 * weeks(1),
  Speaker = "",
  Topic = "[OFF] Summer break"
)

## fixed dates
fixed <- data.frame(
  Date = ymd("2024-08-01"),
  Speaker = "Manuel Stapper",
  Topic = ""
)
extra_meetings <- c()

fixed <- rbind(fixed, summer_break)
names <- setdiff(names, fixed$Speaker)
names <- setdiff(names, c("Sebastian Funk", "Liza Hadley"))

pool <- data.frame(
  Speaker = c(names, rep("", length(extra_meetings))),
  Topic = c(rep("", length(names)), extra_meetings)
) |>
  mutate(id = 1:n())
df <- fixed
next_date <- floor_date(today(), "week", week_start = 4)
while (nrow(pool) > 0) {
  next_date <- next_date + weeks(1)
  ## if date exists already, skip
  if (next_date %in% df$Date) next()
  ## if it's a first of the month, do the journal club
  if (day(next_date) <= 7) {
    ## journal club
    df <- rbind(df, data.frame(
      Date = next_date,
      Speaker = "",
      Topic = "Journal Club"
    ))
  } else {
    ## speaker
    if (nrow(pool) > 1) {
      random_id <- sample(pool$id, 1)
    } else {
      random_id <- pool$id
    }
    random_speaker <- pool |>
      filter(id == random_id)
    df <- rbind(df, data.frame(
      Date = next_date,
      Speaker = random_speaker$Speaker,
      Topic = random_speaker$Topic
    ))
    pool <- pool |>
      filter(id != random_id)
  }
}
df <- df |>
  arrange(Date) |>
  mutate(
    Chair = ifelse(
      Speaker == "",
      Speaker,
      shift(setdiff(Speaker, ""))
    ),
    Notetaking = ifelse(
      Chair == "",
      Chair,
      shift(setdiff(Chair, ""))
    ))
