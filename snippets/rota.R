library("lubridate")
library("dplyr")
library("readr")

source(here::here("snippets", "team_members.R"))

shift <- function(x) {
  c(x[2:length(x)], x[1])
}

names <- unname(unlist((purrr::map(current_team, ~.x$name))))

break_time <- data.frame(
  Date = ymd("2025-04-10") + 0:1 * weeks(1),
  Speaker = "",
  Topic = "[OFF] Easter break"
)

## fixed dates
fixed_speakers <- data.frame(
  Date = ymd(character(0)),
  Speaker = character(0),
  Topic = character(0)
)
extra_meetings <- c("Seb updates", "Bring a figure", "Bring an idea")

fixed <- rbind(fixed_speakers, break_time)
names <- setdiff(names, c("Sebastian Funk", fixed$Speaker))

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
    )
)

write_csv(df, "rota.csv")
