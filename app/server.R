server <-
  shinyServer(function(input, output) {
    
    # bs_themer()
    
    output$sidebar <- renderUI({
      
      sidebar(
        open = "closed"
      )
      
    })
    
    output$nav_teams <- renderUI({
      
      layout_column_wrap(
        width = 1/2, height = 400,
        card(
          full_screen = TRUE,
          card_header("Unpicked"),
          card_body(DT::dataTableOutput("unpicked"))
        ),
        card(
          full_screen = TRUE,
          card_header("Team Summary"),
          card_body(plotlyOutput("team_bars"))
        )
      )
      
    })
    
    # Reactive Data
    
    teams_picked <- reactive({
      
      df %>%
        filter(player_name %in% picks) %>%
        mutate(
          team = case_when(
            player_name %in% picks_01 ~ "team_01",
            player_name %in% picks_02 ~ "team_02",
            player_name %in% picks_03 ~ "team_03",
            player_name %in% picks_04 ~ "team_04",
            player_name %in% picks_05 ~ "team_05",
            player_name %in% picks_06 ~ "team_06",
            player_name %in% picks_07 ~ "team_07",
            player_name %in% picks_08 ~ "team_08",
            player_name %in% picks_09 ~ "team_09",
            player_name %in% picks_10 ~ "team_10",
            player_name %in% picks_11 ~ "team_11",
            player_name %in% picks_12 ~ "team_12"
          )
        )
      
    })
    
    team_comp <- reactive({
      
      teams_picked() %>%
        group_by(team) %>%
        summarize_at(vars(trbPerGame:tovPerGame), list(~sum(.))) %>%
        pivot_longer(trbPerGame:tovPerGame,names_to = "category") %>% 
        group_by(category) %>%
        mutate(
          rank = cume_dist(value),
          n_teams = n_distinct(team),
          n_beats = rank * n_teams
        ) 
      
    })
    
    # Interactive Visuals
    
    output$unpicked <- DT::renderDataTable({
      
      unpicked <- 
        df %>% 
        filter(!player_name %in% picks) %>% 
        arrange(desc(mean_rank)) %>%
        select(player_name,player_pos,mean_rank,trbPerGame:tovPerGame)
      
      DT::datatable(
        unpicked, 
        rownames = F,
        filter = 'top',
        extensions = 'Buttons', 
        options = list(
          pageLength = 5,
          lengthMenu = c(5, 10, 20),
          scrollX = TRUE,
          scrollY = TRUE
        )
      )
      
    })
    
    output$team_bars <- renderPlotly({
      
      p <- 
        team_comp() %>%
        mutate(team = str_remove(team,"^team_")) %>%
        ggplot(aes(x = category, y = n_beats)) +
        facet_grid(vars(team), scales = "free") +
        geom_col() +
        scale_x_discrete(guide = guide_axis(angle = 90)) +
        theme_minimal()
        
      ggplotly(p)
      
    })
    
  }
)