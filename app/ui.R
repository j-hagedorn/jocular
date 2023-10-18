ui <- page_sidebar(
  title = "Draft Picks",
  sidebar = uiOutput("sidebar"),
  theme = bs_theme(bootswatch = "superhero"),
  navset_pill(
    nav_panel(
      title = "Teams",
      br(),
      uiOutput("nav_teams")
    ),
    nav_panel(
      title = "Other", 
      br(),
      uiOutput("nav_other")
    )
  )
)