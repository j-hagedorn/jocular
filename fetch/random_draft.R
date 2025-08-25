library(hoopR); library(tidyverse)

player_box_25 <- load_nba_player_box(2025)

players <-
  player_box_25 |>
  group_by(athlete_id,athlete_display_name) |>
  mutate(played = !did_not_play) |>
  summarise(
    minutes = mean(minutes, na.rm = T),
    games = sum(played)
  ) |>
  filter(minutes >= 20) |>
  filter(games >= 40) |>
  ungroup() |>
  slice_sample(n = 40)
