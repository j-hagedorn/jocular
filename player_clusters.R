library(tidyverse); library(broom)


player <-
  games %>%
  select(
    idGame,idPlayer,namePlayer,yearSeason,ftm:pts,oreb:pctFG2,
    -fg3m,-fg3a,-fg2m,-fg2a,-ftm,-fta
  ) %>%
  # Remove players whose careers would be split up by the 1968 break
  filter(yearSeason >= 1968) %>%
  filter(complete.cases(.)) %>%
  group_by(idPlayer,namePlayer) %>%
  summarise_at(vars(treb:pctFG2),list(~mean(., na.rm = T))) %>%
  filter(!is.infinite(pctFG3)) %>% 
  ungroup()

prcomps <-
  tibble(pc = 1:15) %>%
  mutate(
    pca = map(pc, ~prcomp(player %>% select(treb:pctFG2) %>% scale(), .x)),
    tidied = map(pca, tidy),
    augmented = map(pca, augment, player)
  )

best_players <- 
  prcomps %>% 
  unnest(augmented) %>%
  filter(pc == 6) %>%
  filter(.fittedPC1 >= 2) %>%
  select(idPlayer:.fittedPC6)

mtrx <- 
  best_players %>%
  select(treb:pctFG2) %>%
  scale() 

kclusts <- 
  tibble(k = 1:15) %>%
  mutate(
    kclust = map(k, ~kmeans(mtrx, .x)),
    tidied = map(kclust, tidy),
    glanced = map(kclust, glance),
    augmented = map(kclust, augment, best_players)
  )

# Find the elbow
clusterings <- kclusts %>% unnest(glanced, .drop = TRUE)
ggplot(clusterings, aes(k, tot.withinss)) + geom_line()

player_types <- 
  kclusts %>% 
  unnest(augmented) %>%
  filter(k == 6) %>%
  select(idPlayer:.cluster)

library(echarts4r)

player_types %>%
  group_by(.cluster) %>%
  e_chart(x = .fittedPC1) %>%
  e_scatter(.fittedPC2, bind = namePlayer) %>%
  e_toolbox_feature(feature = "dataZoom") %>%
  e_title("NBA Player Types", "Players grouped using k-means clustering") %>%
  e_x_axis(name = "PC1") %>%
  e_y_axis(name = "PC2") %>%
  e_theme("dark") %>%  # theme
  e_tooltip(
    formatter = htmlwidgets::JS(
      "function(params){
        return('<strong>' + params.name + 
              '</strong><br /> Principal Component 1: ' + params.value[0] + 
              '<br /> Principal Component 2: ' + params.value[1])
      }"
    )
  ) 

