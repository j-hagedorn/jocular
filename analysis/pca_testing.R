library(tidymodels)
tidymodels_prefer()
library(bestNormalize)

player_season <- arrow::read_parquet("data/player_season.parquet")
# df_nba_player_dict <- arrow::read_parquet("data/df_nba_player_dict.parquet")

df <-
  player_season %>%
  select(
    yearSeason, idPlayerNBA, namePlayer, agePlayer, 
    slugPosition, slugTeamBREF, where(is.numeric)
  ) %>%
  select(
    -countTeamsPlayerSeason,
    -countTeamsPlayerSeasonPerGame,
    -countTeamsPlayerSeasonTotals,-minutes,
    -ends_with("Totals"),-starts_with("url")
  ) %>%
  rename(
    season = yearSeason, player_id = idPlayerNBA, player_name = namePlayer,
    player_age = agePlayer, player_pos = slugPosition, player_team = slugTeamBREF
  ) %>%
  select(
    season,starts_with("player_"), countGames, countGamesStarted, minutesPerGame,
    starts_with("pct"),ends_with("PerGame"),starts_with("ratio")
  ) %>%
  # Remove "made" per game, since this is accounted for in % stats
  select(-ends_with("mPerGame")) 


tst <-
  df %>%
  # Keep most recent season
  filter(season == max(season)) %>%
  filter(countGames >= 41)

rec <-
  # Use the training data from the bean_val split object
  recipe(ratioVORP ~ ., data = df %>% select(-(season:player_team))) %>%
  step_zv(all_numeric_predictors()) %>%
  step_orderNorm(all_numeric_predictors()) %>% 
  step_normalize(all_numeric_predictors())

rec_trained <- prep(rec)
