library(nbastatR);library(tidyverse)

# https://github.com/rajshah4/BasketballData
# http://asbcllc.com/nbastatR/index.html

Sys.setenv("VROOM_CONNECTION_SIZE" = 131072 * 2)

games <-
  game_logs(
    seasons = 1959:2023, league = "NBA", result_types = "player",
    season_types = "Regular Season", nest_data = F,
    assign_to_environment = TRUE, return_message = TRUE
  )

player_season <- bref_players_stats(seasons = 1978:2022, tables = c("advanced", "totals", "per_game"))
drafts <- drafts(draft_years = 2023, nest_data = F, return_message = T)

arrow::write_parquet(games,"data/games.parquet")
arrow::write_parquet(player_season,"data/player_season.parquet")
arrow::write_parquet(drafts,"data/drafts.parquet")
arrow::write_parquet(df_nba_player_dict,"../data/df_nba_player_dict.parquet")
  

player_season <- arrow::read_parquet("data/player_season.parquet")
games <- arrow::read_parquet("data/games.parquet")



