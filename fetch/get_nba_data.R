library(nbastatR);library(tidyverse)

# https://github.com/rajshah4/BasketballData
# http://asbcllc.com/nbastatR/index.html

nba_players_08_19 <-
  bref_players_stats(
    seasons = 2008:2019, 
    tables = c("advanced", "totals", "per_game"),
    include_all_nba = T, 
    only_totals = TRUE, 
    assign_to_environment = F
  )



library(plotly)

p <-
nba_players_08_19 %>%
  filter(yearSeason == 2018) %>%
  ggplot(
    aes(
      x = stlPerGame,
      y = blkPerGame,
      color = minutesPerGame
    )
  ) +
  geom_point(alpha = 0.3) +
  theme_minimal() 

nba_players_08_19 %>%
  filter(yearSeason == 2018) %>%
  plot_ly(
    x = ~stlPerGame,
    y = ~blkPerGame,
    color = ~minutesPerGame,
    hoverinfo = 'text',
    text = ~paste0(
      "Player: ", namePlayer
    )
  ) %>%
  add_markers()
  