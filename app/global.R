library(tidyverse); library(shiny); library(bslib)
library(plotly); library(paletteer); library(DT)
# games <- arrow::read_parquet("data/games.parquet")
player_season <- arrow::read_parquet("../data/player_season.parquet")
drafts <- arrow::read_parquet("../data/drafts.parquet")

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
  mutate(rank = mean(c_across(ends_with("_rank")))) %>%
  ungroup() %>%
  mutate(rank = round(rank, digits = 4)) %>%
  arrange(rank) %>%
  select(-season) %>%
  rename(
    age = player_age, pos = player_pos, games = countGames,
    rebounds = trbPerGame, assists = astPerGame, pts = ptsPerGame,
    threes = fg3mPerGame, steals = stlPerGame, blocks = blkPerGame,
    turnovers = tovPerGame
  ) %>%
  bind_rows(
    drafts %>% select(player_id = idPlayer, player_name = namePlayer)
  ) %>%
  filter(player_id != 0)

picks_01 <- c("","","","","","","","","","","","","")
picks_02 <- c("","","","","","","","","","","","","")
picks_03 <- c("","","","","","","","","","","","","")
picks_04 <- c("","","","","","","","","","","","","")
picks_05 <- c("","","","","","","","","","","","","")
picks_06 <- c("","","","","","","","","","","","","")
picks_07 <- c("","","","","","","","","","","","","")
picks_08 <- c("","","","","","","","","","","","","")
picks_09 <- c("","","","","","","","","","","","","")
picks_10 <- c("","","","","","","","","","","","","")

# picks_01 <- c("Nikola Jokic","Damian Lillard","Devin Booker","Evan Mobley","De'Aaron Fox","Bruce Brown","Kyle Kuzma","Tyrese Maxey","John Collins","Kevin Durant","Anthony Edwards","Myles Turner") # picks_01
# picks_02 <- c("Shai Gilgeous-Alexander","Jaren Jackson Jr.","Cade Cunningham","Victor Wembanyama","Jaden McDaniels","DeMar DeRozan","Scoot Henderson","Ben Simmons","Rudy Gobert","Mitchell Robinson","Kelly Oubre Jr.","Cody Martin")  # picks_02
# picks_03 <- c("Kyrie Irving","Al Horford","Donovan Mitchell","Trey Murphy III","Jerami Grant","Brook Lopez","Kelly Olynyk","D'Angelo Russell","Christian Wood","Malcolm Brogdon","Kenrich Williams","Caleb Martin") # picks_03
# picks_04 <- c("Luka Doncic","Anthony Davis","Domantas Sabonis","Bradley Beal","Buddy Hield","Derrick White","Michael Porter Jr.","Herbert Jones","Terry Rozier","Immanuel Quickley","Spencer Dinwiddie","Jarred Vanderbilt") # picks_04
# picks_05 <- c("Jayson Tatum","James Harden","Pascal Siakam","Fred VanVleet","Bam Adebayo","Draymond Green","Zach LaVine","Tyler Herro","Wendell Carter Jr.","Jakob Poeltl","Jae Crowder","Kentavious Caldwell-Pope") # picks_05
# picks_06 <- c("Karl-Anthony Towns","Andrew Wiggins","Nikola Vucevic","Tobias Harris","P.J. Washington","Nic Claxton","Jusuf Nurkic","Josh Hart","Larry Nance Jr.","Clint Capela","Steven Adams","Mason Plumlee") # picks_06
# picks_07 <- c("Joel Embiid","Mikal Bridges","LaMelo Ball","Cameron Johnson","Scottie Barnes","Julius Randle","Jalen Brunson","Jarrett Allen","Russell Westbrook","CJ McCollum","Kyle Anderson","Chris Paul") # picks_07
# picks_08 <- c("Tyrese Haliburton","Desmond Bane","OG Anunoby","Lauri Markkanen","Alperen Sengun","Devin Vassell","Darius Garland","Deandre Ayton","Klay Thompson","Jamal Murray","Franz Wagner","De'Anthony Melton") # picks_08
# picks_09 <- c("Kawhi Leonard","Miles Bridges","Lonzo Ball","Jrue Holiday","Jalen Williams","Otto Porter Jr.","Kevin Porter Jr.","Kevin Huerter","Kevon Looney","Jimmy Butler","Keegan Murray","Ja Morant") # picks_09
# picks_10 <- c("Stephen Curry","Giannis Antetokounmpo","Jaylen Brown","Dejounte Murray","Josh Giddey","Brandon Ingram","Trae Young","Paolo Banchero","LeBron James","Kristaps Porzingis","Paul George","Aaron Gordon") # picks_10

picks <- c(picks_01,picks_02,picks_03,picks_04,picks_05,picks_06,picks_07,picks_08,picks_09,picks_10)

