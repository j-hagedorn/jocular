library(tidyverse); library(shiny); library(bslib)
library(plotly); library(paletteer); library(DT)
player_season <- arrow::read_parquet("../data/player_season.parquet")
drafts <- arrow::read_parquet("../data/drafts.parquet")
# games_df <- arrow::read_parquet("../data/games.parquet") %>% filter(yearSeason == 2023)

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

picks_01 <- c("Nikola Jokic","Jimmy Butler","Fred VanVleet","Darius Garland","Kristaps Porzingis","Zach LaVine","Alperen Sengun","Onyeka Okongwu","Khris Middleton","D'Angelo Russell","Robert Williams III","P.J. Washington","Bruce Brown")
picks_02 <- c("Joel Embiid","Kyrie Irving","Mikal Bridges","Kawhi Leonard","Jamal Murray","Deandre Ayton","Julius Randle","Michael Porter Jr.","Draymond Green","Tre Jones","Jalen Duren","Kevin Huerter","Herbert Jones")
picks_03 <- c("Luka Doncic","Devin Booker","Victor","Paul George","DeMar DeRozan","Nikola Vucevic","Chris Paul","Kyle Kuzma","Jabari Smith Jr.","Scoot","John Collins","Ausur","Spencer Dinwiddie")
picks_04 <- c("Shai Gilgeous-Alexander","Jaren Jackson Jr.","Cade Cunningham","Chet","Scottie Barnes","Brandon Ingram","Tyus Jones","Tyler Herro","Terry Rozier","Al Horford","Russell Westbrook","Tari Eason","Mitchell Robinson")
picks_05 <- c("Jayson Tatum","LaMelo Ball","Desmond Bane","James Harden","Jalen Brunson","Josh Giddey","Brook Lopez","Mark Williams","Markelle Fultz","Andrew Wiggins","Bobby Portis","Keldon Johnson","Trey Murphy III")
picks_06 <- c("Giannis Antetokounmpo","Trae Young","Dejounte Murray","Jaylen Brown","Walker Kessler","Cameron Johnson","Devin Vassell","Jerami Grant","Derrick White","Clint Capela","Jaden McDaniels","Jordan Clarkson","Josh Hart")
picks_07 <- c("Tyrese Haliburton","Domantas Sabonis","Karl-Anthony Towns","Myles Turner","Jordan Poole","Zion","Franz Wagner","Jalen Green","Jakob Poeltl","Klay Thompson","Keegan Murray","Collin Sexton","Coby White")
picks_08 <- c("Stephen Curry","Damian Lillard","Lauri Markkanen","Pascal Siakam","Nic Claxton","Tyrese Maxey","Anfernee Simons","Rudy Gobert","Daniel Gafford","Zach Collins","Ben Simmons","Bennedict Mathurin","Jeremy Sochan")
picks_09 <- c("Anthony Davis","Donovan Mitchell","Bam Adebayo","De'Aaron Fox","Jrue Holiday","OG Anunoby","Jarrett Allen","Jalen Williams","Austin Reaves","Tobias Harris","De'Anthony Melton","Gary Trent Jr.","Shaedon Sharpe")
picks_10 <- c("Anthony Edwards","Kevin Durant","LeBron James","Evan Mobley","Paolo Banchero","Bradley Beal","CJ McCollum","Ja Morant","Buddy Hield","Jonas Valanciunas","Marcus Smart","Aaron Gordon","Ivica Zubac")

picks <- c(picks_01,picks_02,picks_03,picks_04,picks_05,picks_06,picks_07,picks_08,picks_09,picks_10)

