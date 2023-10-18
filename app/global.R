library(tidyverse); library(shiny); library(bslib)
library(plotly); library(paletteer)
# games <- arrow::read_parquet("data/games.parquet")
player_season <- arrow::read_parquet("../data/player_season.parquet")

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
    trbPerGame, astPerGame, ptsPerGame, fg3mPerGame, stlPerGame, 
    blkPerGame, pctFG, pctFT, tovPerGame
  ) %>%
  filter(countGames >= 30) %>%
  group_by(player_id) %>%
  filter(season == max(season)) %>%
  ungroup() %>%
  filter(season %in% c(2022, 2023)) %>%
  mutate_at(vars(trbPerGame:pctFG), list(rank = percent_rank)) %>%
  # rank turnovers inversely
  mutate(tovPerGame_rank = 1 - percent_rank(tovPerGame)) %>%
  rowwise() %>%
  mutate(mean_rank = mean(c_across(ends_with("_rank")))) %>%
  ungroup() %>%
  arrange(mean_rank) %>%
  select(-season) 


picks_01 <- c("Nikola Jokic","Damian Lillard","Devin Booker","Evan Mobley","De'Aaron Fox","Bruce Brown","Kyle Kuzma","Tyrese Maxey","John Collins","","","") # picks_01
picks_02 <- c("Shai Gilgeous-Alexander","Jaren Jackson Jr.","Cade Cunningham","Victor Wembanyama","Jaden McDaniels","DeMar DeRozan","Scoot Henderson","Ben Simmons","Rudy Gobert","","","")  # picks_02
picks_03 <- c("Kyrie Irving","Al Horford","Donovan Mitchell","Trey Murphy III","Jerami Grant","Brook Lopez","Kelly Olynyk","D'Angelo Russell","Christian Wood","","","") # picks_03
picks_04 <- c("Luka Doncic","Anthony Davis","Domantas Sabonis","Bradley Beal","Buddy Hield","Derrick White","Michael Porter Jr.","Herbert Jones","Terry Rozier","","","") # picks_04
picks_05 <- c("Jayson Tatum","James Harden","Pascal Siakam","Fred VanVleet","Bam Adebayo","Draymond Green","Zach LaVine","Tyler Herro","Wendell Carter Jr.","","","") # picks_05
picks_06 <- c("Karl-Anthony Towns","Andrew Wiggins","Nikola Vucevic","Tobias Harris","P.J. Washington","Nic Claxton","Jusuf Nurkic","Josh Hart","Larry Nance Jr.","","","") # picks_06
picks_07 <- c("Joel Embiid","Mikal Bridges","LaMelo Ball","Cameron Johnson","Scottie Barnes","Julius Randle","Jalen Brunson","Jarrett Allen","Russell Westbrook","","","") # picks_07
picks_08 <- c("Tyrese Haliburton","Desmond Bane","OG Anunoby","Lauri Markkanen","Alperen Sengun","Devin Vassell","Darius Garland","Deandre Ayton","Klay Thompson","","","") # picks_08
picks_09 <- c("Kawhi Leonard","Miles Bridges","Lonzo Ball","Jrue Holiday","Jalen Williams","Otto Porter Jr.","Kevin Porter Jr.","Kevin Huerter","Kevon Looney","","","") # picks_09
picks_10 <- c("Stephen Curry","Giannis Antetokounmpo","Jaylen Brown","Dejounte Murray","Josh Giddey","Brandon Ingram","Trae Young","Paolo Banchero","","","","") # picks_10
picks_11 <- c("Kevin Durant","Anthony Edwards","Myles Turner","Jimmy Butler","Keegan Murray","Ja Morant","Jamal Murray","Franz Wagner","","","","") # picks_11
picks_12 <- c("LeBron James","Kristaps Porzingis","Paul George","Aaron Gordon","CJ McCollum","Kyle Anderson","Chris Paul","Jakob Poeltl","","","","") # picks_12

picks <- c(picks_01,picks_02,picks_03,picks_04,picks_05,picks_06,picks_07,picks_08,picks_09,picks_10,picks_11,picks_12)

