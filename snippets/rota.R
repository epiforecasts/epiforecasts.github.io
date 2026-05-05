library("lubridate")
library("dplyr")
library("readr")
library("purrr")

source(here::here("snippets", "team_members.R"))

names <- unname(unlist(map(current_team, ~ .x$name)))

## ---- rotation parameters (edit these each rotation) ------------------------

## first and last Thursday of the rotation. End is set to give roughly 6 months;
## the final Thursday on or before end_date becomes the end-of-rotation update.
start_date <- floor_date(today(), "week", week_start = 4) + weeks(1)
end_date <- start_date + months(6) - weeks(1)

## "Group updates": Seb (or another lead) presents grants / group direction.
updates_presenter <- "Sebastian Funk"
updates_date <- ymd(NA)  # e.g. ymd("2026-06-11"); NA = first eligible Thursday

## skip journal club on these dates (e.g. too short notice at rotation start)
no_jc_dates <- ymd(character(0))

## people temporarily out of the rotation (e.g. on leave)
pool_exclusions <- character(0)

## people who shouldn't chair before a given date (e.g. new chairs, or the
## group leader during travel)
not_chair_before <- list(
  # "Some Person" = ymd("2026-09-01")
)

## people who presented recently (end of previous rotation) — set last_presented
## to that date so they don't get drawn again until the minimum gap has passed
recently_presented <- list(
  # "Some Person" = ymd("2026-04-30")
)

## minimum gap (in weeks) between presentation slots for the same person
min_presenter_gap_weeks <- 5

## ---- machinery -------------------------------------------------------------

august_thursdays <- function(year) {
  thursdays <- seq(ymd(paste0(year, "-08-01")), ymd(paste0(year, "-08-31")), by = "day")
  thursdays[wday(thursdays, week_start = 1) == 4]
}

break_time <- data.frame(
  Date = unlist(lapply(
    unique(year(c(start_date, end_date))),
    august_thursdays
  )) |> as.Date(origin = "1970-01-01"),
  Topic = "[OFF] August break"
)
fixed_topics <- data.frame(
  Date = ymd(character(0)),
  Topic = character(0)
)
fixed <- rbind(fixed_topics, break_time)

pool_names <- setdiff(names, pool_exclusions)

## round-robin rotation; excluded candidates stay in their slot until eligible
make_rotation <- function(pool) {
  order <- sample(pool)
  function(exclude = character(0)) {
    for (i in seq_along(order)) {
      candidate <- order[i]
      if (!(candidate %in% exclude)) {
        order <<- c(order[-i], candidate)
        return(candidate)
      }
    }
    stop("No eligible rotation candidate")
  }
}

next_presenter <- make_rotation(pool_names)
## single chair rotation covers both group meetings and journal clubs, so the
## same person can't chair two weeks in a row by accident
next_chair <- make_rotation(pool_names)

nth_thursday_of_month <- function(date) {
  ((day(date) - 1) %/% 7) + 1
}

end_of_rotation <- floor_date(end_date, "week", week_start = 4)

## first regular Thursday of the rotation that isn't journal club / coffee walk
## — used as the default Group updates date if updates_date is NA
if (is.na(updates_date)) {
  candidate <- start_date
  while (TRUE) {
    nth <- nth_thursday_of_month(candidate)
    is_jc <- nth == 1 && !(candidate %in% no_jc_dates)
    is_walk <- nth == 3
    if (!(candidate %in% fixed$Date) && !is_jc && !is_walk &&
        candidate != end_of_rotation) {
      updates_date <- candidate
      break
    }
    candidate <- candidate + weeks(1)
  }
}

last_presented <- setNames(as.Date(rep(NA, length(pool_names))), pool_names)
for (p in names(recently_presented)) {
  if (p %in% pool_names) last_presented[[p]] <- recently_presented[[p]]
}

df <- data.frame(
  Date = ymd(character(0)),
  Topic = character(0),
  Chair = character(0),
  Presenter1 = character(0),
  Presenter2 = character(0),
  Presenter3 = character(0)
)

next_date <- start_date
while (next_date <= end_date) {
  chair_excluded <- names(not_chair_before)[
    map_lgl(not_chair_before, ~ next_date < .x)
  ]
  presenter_excluded <- pool_names[
    map_lgl(pool_names, function(p) {
      lp <- last_presented[[p]]
      !is.na(lp) &&
        as.numeric(next_date - lp, units = "weeks") < min_presenter_gap_weeks
    })
  ]

  if (next_date %in% fixed$Date) {
    row <- fixed |> filter(Date == next_date)
    df <- bind_rows(df, data.frame(
      Date = next_date, Topic = row$Topic,
      Chair = "", Presenter1 = "", Presenter2 = "", Presenter3 = ""
    ))
  } else if (next_date == end_of_rotation) {
    df <- bind_rows(df, data.frame(
      Date = next_date, Topic = "End-of-rotation / 6-monthly update",
      Chair = "", Presenter1 = "", Presenter2 = "", Presenter3 = ""
    ))
  } else if (next_date == updates_date) {
    df <- bind_rows(df, data.frame(
      Date = next_date, Topic = "Group updates",
      Chair = "", Presenter1 = updates_presenter,
      Presenter2 = "", Presenter3 = ""
    ))
  } else {
    nth <- nth_thursday_of_month(next_date)
    if (nth == 1 && !(next_date %in% no_jc_dates)) {
      lead <- next_chair(exclude = chair_excluded)
      df <- bind_rows(df, data.frame(
        Date = next_date, Topic = "Journal Club",
        Chair = lead, Presenter1 = "", Presenter2 = "", Presenter3 = ""
      ))
    } else if (nth == 3) {
      df <- bind_rows(df, data.frame(
        Date = next_date, Topic = "Coffee walk",
        Chair = "", Presenter1 = "", Presenter2 = "", Presenter3 = ""
      ))
    } else {
      ## try to fill up to 3 presenter slots; some early meetings may have
      ## fewer if the pool is constrained by recent-presentation gaps
      picks <- character(0)
      for (i in 1:3) {
        candidate <- tryCatch(
          next_presenter(exclude = c(presenter_excluded, picks)),
          error = function(e) NA_character_
        )
        if (is.na(candidate)) break
        picks <- c(picks, candidate)
      }
      chair <- next_chair(exclude = c(chair_excluded, picks))
      last_presented[picks] <- next_date
      slot <- function(i) if (i <= length(picks)) picks[i] else ""
      df <- bind_rows(df, data.frame(
        Date = next_date, Topic = "Group meeting",
        Chair = chair, Presenter1 = slot(1),
        Presenter2 = slot(2), Presenter3 = slot(3)
      ))
    }
  }
  next_date <- next_date + weeks(1)
}

df <- df |> arrange(Date)

write_csv(df, "rota.csv")

## paste-ready CSV matching the group-meeting spreadsheet schema
## (Date, Speaker, Topic, Chair). For group meetings Speaker is a comma-
## separated list of the three presenters so a Slack automation can split
## and tag each. For journal clubs the lead goes in Speaker so they get pinged.
join_presenters <- function(p1, p2, p3) {
  parts <- c(p1, p2, p3)
  paste(parts[parts != ""], collapse = ", ")
}

sheet_df <- df |>
  rowwise() |>
  mutate(
    Speaker = case_when(
      Topic == "Group meeting" ~ join_presenters(Presenter1, Presenter2, Presenter3),
      Topic == "Group updates" ~ Presenter1,
      Topic == "Journal Club" ~ Chair,
      Topic == "End-of-rotation / 6-monthly update" ~ "Everyone",
      TRUE ~ ""
    ),
    Topic = case_when(
      Topic == "Group meeting" ~ paste0(
        "Project updates (",
        join_presenters(Presenter1, Presenter2, Presenter3),
        ")"
      ),
      TRUE ~ Topic
    ),
    Chair = if_else(Topic == "Journal Club", "", Chair)
  ) |>
  ungroup() |>
  select(Date, Speaker, Topic, Chair)

write_csv(sheet_df, "rota_for_sheet.csv")
