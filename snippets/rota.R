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

## people in the rotation who aren't on the website team page yet
pool_additions <- character(0)

## people who shouldn't chair before a given date (e.g. new chairs, or the
## group leader during travel)
not_chair_before <- list(
  # "Some Person" = ymd("2026-09-01")
)

## people who shouldn't present before a given date (e.g. just started)
not_present_before <- list(
  # "Some Person" = ymd("2026-09-01")
)

## people who presented recently (end of previous rotation) — set last_presented
## to that date so they don't get drawn again until the minimum gap has passed
recently_presented <- list(
  # "Some Person" = ymd("2026-04-30")
)

## most recent journal club each person led; people who have never led one
## (or led one longest ago) are first in line for the next
last_journal_club <- list(
  # "Some Person" = ymd("2026-03-05")
)

## meetings alternate weekly between 10:30 and 15:30; this is a 15:30 Thursday
afternoon_anchor <- ymd("2026-10-01")

## people who can only make the 15:30 meetings
afternoon_only <- character(0)

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

pool_names <- setdiff(union(names, pool_additions), pool_exclusions)

## round-robin rotation; excluded candidates stay in their slot until eligible
make_rotation <- function(pool, last_done = list()) {
  last <- map_dbl(pool, ~ if (is.null(last_done[[.x]])) -Inf else as.numeric(last_done[[.x]]))
  order <- pool[order(last, runif(length(pool)))]
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
## journal club leads have their own rotation, ordered by who led one longest
## ago, so it doesn't drift with the chair draws
next_chair <- make_rotation(pool_names)
next_jc_lead <- make_rotation(pool_names, last_journal_club)
last_jc <- list(date = ymd(NA), lead = NA_character_)

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
  presenter_excluded <- c(
    pool_names[
      map_lgl(pool_names, function(p) {
        lp <- last_presented[[p]]
        !is.na(lp) &&
          as.numeric(next_date - lp, units = "weeks") < min_presenter_gap_weeks
      })
    ],
    names(not_present_before)[map_lgl(not_present_before, ~ next_date < .x)]
  )
  is_afternoon <- as.numeric(next_date - afternoon_anchor) %% 14 == 0
  if (!is_afternoon) {
    chair_excluded <- c(chair_excluded, afternoon_only)
    presenter_excluded <- c(presenter_excluded, afternoon_only)
  }

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
      ## leading journal club counts as a presentation for the gap rule
      lead <- next_jc_lead(exclude = presenter_excluded)
      last_jc <- list(date = next_date, lead = lead)
      last_presented[lead] <- next_date
      df <- bind_rows(df, data.frame(
        Date = next_date, Topic = "Journal Club",
        Chair = "", Presenter1 = lead, Presenter2 = "", Presenter3 = ""
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
      ## don't have last week's journal club lead chair straight after
      jc_lead_last_week <- if (isTRUE(next_date - last_jc$date == 7)) {
        last_jc$lead
      } else {
        character(0)
      }
      chair <- next_chair(exclude = c(chair_excluded, picks, jc_lead_last_week))
      last_presented[picks] <- next_date
      slot <- function(i) if (i <= length(picks)) picks[i] else ""
      df <- bind_rows(df, data.frame(
        Date = next_date, Topic = "Project updates",
        Chair = chair, Presenter1 = slot(1),
        Presenter2 = slot(2), Presenter3 = slot(3)
      ))
    }
  }
  next_date <- next_date + weeks(1)
}

df <- df |> arrange(Date)

write_csv(df, "rota.csv")

## paste-ready CSV matching the group-meeting spreadsheet schema with three
## speaker columns (Date, Speaker, Speaker2, Speaker3, Topic, Chair). The
## Slack bot tags each non-empty speaker.
sheet_df <- df |>
  mutate(
    Speaker = case_when(
      Topic %in% c("Project updates", "Group updates", "Journal Club") ~ Presenter1,
      Topic == "End-of-rotation / 6-monthly update" ~ "Everyone",
      TRUE ~ ""
    ),
    Speaker2 = if_else(Topic == "Project updates", Presenter2, ""),
    Speaker3 = if_else(Topic == "Project updates", Presenter3, "")
  ) |>
  select(Date, Speaker, Speaker2, Speaker3, Topic, Chair)

write_csv(sheet_df, "rota_for_sheet.csv")
