server <-
  shinyServer(function(input, output) {
    
    # bs_themer()
    
    output$nav_teams <- renderUI({
      
      layout_column_wrap(
        width = NULL, 
        height = 550,
        style = htmltools::css(grid_template_columns = "2fr 1fr"),
        card(
          full_screen = TRUE,
          # style = "resize:horizontal;",
          card_header("Unpicked"),
          layout_sidebar(
            sidebar = sidebar(
              open = "closed",
              sliderInput(
                inputId = "games_filt",
                label = "Games per season",
                min = min(df$games,na.rm = T),
                max = max(df$games,na.rm = T),
                value = c(40,max(df$games,na.rm = T))
              )
            ),
            DT::dataTableOutput("unpicked")
          )
        ),
        navset_card_tab(
          # height = 500,
          full_screen = TRUE,
          title = "Team Summary",
          nav_panel(
            "Table",
            DT::dataTableOutput("team_tbl")
          ),
          nav_panel(
            "Bars",
            plotlyOutput("team_bars")
          )
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
            player_name %in% picks_10 ~ "team_10"
          )
        )
      
    })
    
    team_comp <- reactive({
      
      team_pos <-
        teams_picked() %>%
        arrange(team,pos) %>%
        group_by(team) %>%
        summarize(pos = paste(pos,collapse = ","))
      
      teams_picked() %>%
        group_by(team) %>%
        summarize_at(vars(rebounds:turnovers), list(~mean(.,na.rm = T))) %>%
        pivot_longer(rebounds:turnovers,names_to = "category") %>% 
        group_by(category) %>%
        mutate(
          rank = cume_dist(value),
          n_teams = n_distinct(team),
          n_beats = rank * n_teams
        ) %>%
        left_join(team_pos, by = "team")
      
    })
    
    # Interactive Visuals
    
    output$unpicked <- DT::renderDataTable({
      
      unpicked <- 
        df %>% 
        filter(!player_name %in% picks) %>% 
        filter(games %in% input$games_filt[1]:input$games_filt[2]) %>%
        arrange(desc(rank)) %>%
        select(player_name,pos,rank,rebounds:turnovers) %>%
        select(
          player_name,pos,rank,pts,threes,pctFG,pctFT,
          assists,turnovers,steals,rebounds,blocks
        ) 

      DT::datatable(
        unpicked, 
        rownames = F,
        # plugins = "ellipsis",
        filter = 'top',
        extensions = 'Buttons', 
        options = list(
          pageLength = 7,
          lengthMenu = c(5, 7, 10),
          scrollX = TRUE,
          scrollY = TRUE,
          columnDefs = list(list(
            targets = c(2),
            render = JS("$.fn.dataTable.render.ellipsis( 5, false )")
          ))
        )
      ) # %>%
      # DT::formatStyle(
      #   'rank',
      #   background = styleColorBar(unpicked$rank, 'steelblue'),
      #   backgroundSize = '100% 90%',
      #   backgroundRepeat = 'no-repeat',
      #   backgroundPosition = 'center'
      # )
      
    })
    
    output$team_tbl <- DT::renderDataTable({
      
      x <-
        team_comp() %>%
        group_by(team,pos) %>%
        summarise(
          matches = sum(n_teams),
          wins = sum(n_beats)
        ) %>%
        ungroup() %>%
        mutate(pct_wins = round(wins/matches, digits = 3)) %>%
        select(team,pos,pct_wins) %>%
        arrange(desc(pct_wins))
      
      DT::datatable(
        x, 
        rownames = F,
        plugins = "ellipsis",
        extensions = 'Buttons', 
        options = list(
          columnDefs = list(list(
            targets = c(1),
            render = JS("$.fn.dataTable.render.ellipsis( 8, false )")
          ))
        )
      ) %>%
      DT::formatStyle(
        'pct_wins',
        background = styleColorBar(x$pct_wins, 'steelblue'),
        backgroundSize = '100% 90%',
        backgroundRepeat = 'no-repeat',
        backgroundPosition = 'center'
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
        theme_minimal() +
        theme(
          axis.text.y=element_blank(),  #remove y axis labels
          axis.ticks.y=element_blank()  #remove y axis ticks
        )
        
      ggplotly(p)
      
    })
    
  }
)