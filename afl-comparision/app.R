library(shiny)
library(shinydashboard)
library(fitzRoy)
library(tidyr)
library(ggplot2)
library(dplyr)
library(lubridate)
library(DT)
library(plotly)

afl_fixture <- fetch_fixture_afl(2025)

afl_fixture <- afl_fixture %>%
  mutate(
    date = as.Date(utcStartTime),
    season = year(date),
    matchup = paste(home.team.name, "vs", away.team.name)
  ) %>%
  select(
    Match_ID = id,
    Date = date,
    Season = season,
    Round = round.roundNumber,
    Matchup = matchup,
    Home_Team = home.team.name,
    Home_Goals = home.score.goals,
    Home_Behinds = home.score.behinds,
    Home_Total = home.score.totalScore,
    Away_Team = away.team.name,
    Away_Goals = away.score.goals,
    Away_Behinds = away.score.behinds,
    Away_Total = away.score.totalScore
  )

afl_lineups <- fetch_lineup_afl(2025, round = 1)

afl_lineups <- afl_lineups %>%
  mutate(
    Player = paste(player.playerName.givenName, player.playerName.surname),
    date = as.Date(utcStartTime),
    season = year(date)
  ) %>%
  select(
    Date = date,
    Season = season,
    Round = round.roundNumber,
    Player,
    Team = teamName,
    Position = position,
    Jumper_Number = player.playerJumperNumber
  )

afl_details <- fetch_player_details_afl(2025)

afl_details <- afl_details %>%
  mutate(
    Player = paste(firstName, surname)
  ) %>%
  select(
    Player,
    Player_ID = id,
    Team = team,
    Season = season,
    Jumper_Number = jumperNumber,
    Position = position,
    DOB = dateOfBirth,
    Draft_Year = draftYear,
    Height_cm = heightInCm,
    Weight_kg = weightInKg,
    Recruited_From = recruitedFrom,
    Debut_Year = debutYear,
    Draft_Position = draftPosition,
    Draft_Type = draftType
  )

afl_lineups_details <- afl_lineups %>%
  left_join(afl_details, by = "Player")

# There is a function in fitzroy that allows for collection of player photo URL can add that in and some other stuff

# Get historical stats - 2010 is the earliest
afl_stats <- fetch_player_stats_footywire(2010:2025)
colnames(afl_stats)
afl_stats <- afl_stats %>%
  select(-Venue, -Status, -Match_id) %>%
  rename(
    Goal_Assists = GA,
    Contested_Possessions = CP,
    Uncontested_Possessions = UP,
    Effective_Disposals = ED,
    Disposal_Efficiency = DE,
    Contested_Marks = CM,
    Marks_Inside_50 = MI5,
    One_Percenters = One.Percenters,
    Bounces = BO,
    Time_on_Ground_Pct = TOG,
    Kicks = K,
    Handballs = HB,
    Disposals = D,
    Marks = M,
    Goals = G,
    Behinds = B,
    Tackles = T,
    Hitouts = HO,
    Inside_50s = I50,
    Clearances = CL,
    Clangers = CG,
    Rebound_50s = R50,
    Frees_For = FF,
    Frees_Against = FA,
    AFL_Fantasy = AF,
    SuperCoach = SC,
    Centre_Clearances = CCL,
    Stoppage_Clearances = SCL,
    Score_Involvements = SI,
    Metres_Gained = MG,
    Turnovers = TO,
    Intercepts = ITC,
    Tackles_Inside_50 = T5
  )
# DOES NOT INCLUDE FINALS
afl_stats <- afl_stats %>%
  mutate(Round = as.integer(gsub("Round ", "", Round)))

# Calculate player averages for their career
player_averages <- afl_stats %>%
  group_by(Player) %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE)) %>%
  ungroup()

# Calculate player averages over the last 10 games
last_10_games <- afl_stats %>%
  group_by(Player) %>%
  arrange(desc(Season), desc(Round)) %>%
  slice_head(n = 10) %>%
  ungroup()
last_10_averages <- last_10_games %>%
  group_by(Player) %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE)) %>%
  ungroup()

# Show the last 5 games
recent_5_games <- afl_stats %>%
  group_by(Player) %>%
  arrange(desc(Season), desc(Round)) %>%
  slice_head(n = 5) %>%
  ungroup()

library(shiny)
library(shinydashboard)
library(fitzRoy)
library(tidyr)
library(ggplot2)
library(dplyr)
library(lubridate)
library(DT)
library(plotly)

# UI
ui <- dashboardPage(
  dashboardHeader(title = "AFL Stats Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "overview", icon = icon("dashboard")),
      menuItem("Fixtures", tabName = "fixtures", icon = icon("calendar")),
      menuItem("Player Stats", tabName = "player_stats", icon = icon("user")),
      menuItem("Team Comparison", tabName = "team_comparison", icon = icon("users")),
      menuItem("Data Tables", tabName = "data_tables", icon = icon("table"))
    )
  ),
  dashboardBody(
    tabItems(
      # Overview tab
      tabItem(tabName = "overview",
              fluidRow(
                box(
                  title = "Welcome to AFL Stats Dashboard",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  "This dashboard allows you to explore AFL data for the 2025 season and historical stats since 2010. Use the sidebar to navigate through different sections."
                )
              ),
              fluidRow(
                valueBoxOutput("total_matches_box", width = 4),
                valueBoxOutput("total_players_box", width = 4),
                valueBoxOutput("avg_score_box", width = 4)
              ),
              fluidRow(
                box(
                  title = "Scoring Trends (2010-2025)",
                  status = "primary",
                  solidHeader = TRUE,
                  plotlyOutput("scoring_trends", height = 300)
                ),
                box(
                  title = "Top Performers 2025",
                  status = "warning",
                  solidHeader = TRUE,
                  tableOutput("top_performers")
                )
              )
      ),
      
      # Fixtures tab
      tabItem(tabName = "fixtures",
              fluidRow(
                box(
                  title = "Fixture Filter",
                  status = "primary",
                  solidHeader = TRUE,
                  selectInput("round_select", "Select Round", choices = c("All", 1:23)),
                  selectInput("team_filter", "Filter by Team", choices = c("All Teams"))
                ),
                box(
                  title = "Fixture Calendar",
                  status = "primary",
                  solidHeader = TRUE,
                  plotOutput("fixture_calendar", height = 300)
                )
              ),
              fluidRow(
                box(
                  title = "Fixtures",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  DTOutput("fixture_table")
                )
              )
      ),
      
      # Player Stats tab
      tabItem(tabName = "player_stats",
              fluidRow(
                box(
                  title = "Player Selection",
                  status = "primary",
                  solidHeader = TRUE,
                  selectInput("player_select", "Select Player", choices = NULL),
                  selectInput("stat_type", "Statistic Type", 
                              choices = c("Disposals", "Goals", "Marks", "Tackles", "AFL_Fantasy", "SuperCoach"))
                ),
                box(
                  title = "Player Information",
                  status = "info",
                  solidHeader = TRUE,
                  uiOutput("player_info")
                )
              ),
              fluidRow(
                box(
                  title = "Player Statistics Over Time",
                  status = "primary",
                  solidHeader = TRUE,
                  plotlyOutput("player_stats_chart", height = 300)
                ),
                box(
                  title = "Player Career Averages",
                  status = "warning",
                  solidHeader = TRUE,
                  tableOutput("player_averages_table")
                )
              ),
              fluidRow(
                box(
                  title = "Recent Form (Last 5 Games)",
                  status = "success",
                  solidHeader = TRUE,
                  width = 12,
                  DTOutput("recent_form_table")
                )
              )
      ),
      
      # Team Comparison tab
      tabItem(tabName = "team_comparison",
              fluidRow(
                box(
                  title = "Team Selection",
                  status = "primary",
                  solidHeader = TRUE,
                  selectInput("team1_select", "Select Team 1", choices = NULL),
                  selectInput("team2_select", "Select Team 2", choices = NULL),
                  selectInput("comp_stat", "Comparison Statistic", 
                              choices = c("Disposals", "Goals", "Marks", "Tackles", "Inside_50s", "Rebound_50s"))
                ),
                box(
                  title = "Team Comparison",
                  status = "primary",
                  solidHeader = TRUE,
                  plotlyOutput("team_comparison_chart", height = 300)
                )
              ),
              fluidRow(
                box(
                  title = "Team 1 Top Players",
                  status = "info",
                  solidHeader = TRUE,
                  tableOutput("team1_top_players")
                ),
                box(
                  title = "Team 2 Top Players",
                  status = "warning",
                  solidHeader = TRUE,
                  tableOutput("team2_top_players")
                )
              )
      ),
      
      # Data Tables tab
      tabItem(tabName = "data_tables",
              fluidRow(
                box(
                  title = "Data Selection",
                  status = "primary",
                  solidHeader = TRUE,
                  selectInput("data_table_select", "Select Data Table", 
                              choices = c("Fixtures", "Player Stats", "Player Details", "Lineups"))
                )
              ),
              fluidRow(
                box(
                  title = "Data Table",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  DTOutput("full_data_table")
                )
              )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Load data
  afl_fixture <- reactive({
    fetch_fixture_afl(2025) %>%
      mutate(
        date = as.Date(utcStartTime),
        season = year(date),
        matchup = paste(home.team.name, "vs", away.team.name)
      ) %>%
      select(
        Match_ID = id,
        Date = date,
        Season = season,
        Round = round.roundNumber,
        Matchup = matchup,
        Home_Team = home.team.name,
        Home_Goals = home.score.goals,
        Home_Behinds = home.score.behinds,
        Home_Total = home.score.totalScore,
        Away_Team = away.team.name,
        Away_Goals = away.score.goals,
        Away_Behinds = away.score.behinds,
        Away_Total = away.score.totalScore
      )
  })
  
  afl_lineups <- reactive({
    fetch_lineup_afl(2025, round = 1) %>%
      mutate(
        Player = paste(player.playerName.givenName, player.playerName.surname),
        date = as.Date(utcStartTime),
        season = year(date)
      ) %>%
      select(
        Date = date,
        Season = season,
        Round = round.roundNumber,
        Player,
        Team = teamName,
        Position = position,
        Jumper_Number = player.playerJumperNumber
      )
  })
  
  afl_details <- reactive({
    fetch_player_details_afl(2025) %>%
      mutate(
        Player = paste(firstName, surname)
      ) %>%
      select(
        Player,
        Player_ID = id,
        Team = team,
        Season = season,
        Jumper_Number = jumperNumber,
        Position = position,
        DOB = dateOfBirth,
        Draft_Year = draftYear,
        Height_cm = heightInCm,
        Weight_kg = weightInKg,
        Recruited_From = recruitedFrom,
        Debut_Year = debutYear,
        Draft_Position = draftPosition,
        Draft_Type = draftType
      )
  })
  
  afl_stats <- reactive({
    fetch_player_stats_footywire(2010:2025) %>%
      select(-Venue, -Status, -Match_id) %>%
      rename(
        Goal_Assists = GA,
        Contested_Possessions = CP,
        Uncontested_Possessions = UP,
        Effective_Disposals = ED,
        Disposal_Efficiency = DE,
        Contested_Marks = CM,
        Marks_Inside_50 = MI5,
        One_Percenters = One.Percenters,
        Bounces = BO,
        Time_on_Ground_Pct = TOG,
        Kicks = K,
        Handballs = HB,
        Disposals = D,
        Marks = M,
        Goals = G,
        Behinds = B,
        Tackles = T,
        Hitouts = HO,
        Inside_50s = I50,
        Clearances = CL,
        Clangers = CG,
        Rebound_50s = R50,
        Frees_For = FF,
        Frees_Against = FA,
        AFL_Fantasy = AF,
        SuperCoach = SC,
        Centre_Clearances = CCL,
        Stoppage_Clearances = SCL,
        Score_Involvements = SI,
        Metres_Gained = MG,
        Turnovers = TO,
        Intercepts = ITC,
        Tackles_Inside_50 = T5
      ) %>%
      mutate(Round = as.integer(gsub("Round ", "", Round)))
  })
  
  # Calculate player averages for their career
  player_averages <- reactive({
    afl_stats() %>%
      group_by(Player) %>%
      summarise(across(where(is.numeric), mean, na.rm = TRUE)) %>%
      ungroup()
  })
  
  # Last 10 games for players
  last_10_averages <- reactive({
    afl_stats() %>%
      group_by(Player) %>%
      arrange(desc(Season), desc(Round)) %>%
      slice_head(n = 10) %>%
      summarise(across(where(is.numeric), mean, na.rm = TRUE)) %>%
      ungroup()
  })
  
  # Recent 5 games
  recent_5_games <- reactive({
    afl_stats() %>%
      group_by(Player) %>%
      arrange(desc(Season), desc(Round)) %>%
      slice_head(n = 5) %>%
      ungroup()
  })
  
  # Update dropdown choices
  observe({
    # Teams for filters
    all_teams <- unique(c(afl_fixture()$Home_Team, afl_fixture()$Away_Team))
    updateSelectInput(session, "team_filter", choices = c("All Teams", all_teams))
    updateSelectInput(session, "team1_select", choices = all_teams)
    updateSelectInput(session, "team2_select", choices = all_teams, selected = all_teams[2])
    
    # Players for selection
    all_players <- unique(afl_stats()$Player)
    updateSelectInput(session, "player_select", choices = all_players)
  })
  
  # Overview tab outputs
  output$total_matches_box <- renderValueBox({
    valueBox(
      nrow(afl_fixture()), "Total Matches", icon = icon("gamepad"), color = "blue"
    )
  })
  
  output$total_players_box <- renderValueBox({
    valueBox(
      length(unique(afl_stats()$Player)), "Players", icon = icon("users"), color = "green"
    )
  })
  
  output$avg_score_box <- renderValueBox({
    avg_score <- mean(c(afl_fixture()$Home_Total, afl_fixture()$Away_Total), na.rm = TRUE)
    valueBox(
      round(avg_score, 1), "Avg. Score", icon = icon("chart-line"), color = "purple"
    )
  })
  
  output$scoring_trends <- renderPlotly({
    scoring_by_year <- afl_stats() %>%
      group_by(Season) %>%
      summarise(
        Avg_Goals = mean(Goals, na.rm = TRUE),
        Avg_Behinds = mean(Behinds, na.rm = TRUE)
      )
    
    p <- ggplot(scoring_by_year, aes(x = Season)) +
      geom_line(aes(y = Avg_Goals, color = "Goals")) +
      geom_line(aes(y = Avg_Behinds, color = "Behinds")) +
      labs(title = "Average Scoring per Player by Season", y = "Average", color = "Stat") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  output$top_performers <- renderTable({
    current_year_stats <- afl_stats() %>%
      filter(Season == 2025) %>%
      group_by(Player) %>%
      summarise(
        Avg_Disposals = mean(Disposals, na.rm = TRUE),
        Avg_Goals = mean(Goals, na.rm = TRUE),
        Avg_Fantasy = mean(AFL_Fantasy, na.rm = TRUE)
      ) %>%
      arrange(desc(Avg_Fantasy)) %>%
      head(5) %>%
      mutate(across(where(is.numeric), round, 1))
  })
  
  # Fixtures tab outputs
  output$fixture_calendar <- renderPlot({
    filtered_fixture <- afl_fixture()
    
    if (input$round_select != "All") {
      filtered_fixture <- filtered_fixture %>%
        filter(Round == input$round_select)
    }
    
    if (input$team_filter != "All Teams") {
      filtered_fixture <- filtered_fixture %>%
        filter(Home_Team == input$team_filter | Away_Team == input$team_filter)
    }
    
    # Create a calendar view of fixtures
    ggplot(filtered_fixture, aes(x = Date)) +
      geom_point(aes(y = Matchup, color = factor(Round)), size = 3) +
      scale_color_viridis_d() +
      labs(title = "AFL Fixture Calendar", color = "Round") +
      theme_minimal() +
      theme(axis.text.y = element_text(size = 8))
  })
  
  output$fixture_table <- renderDT({
    filtered_fixture <- afl_fixture()
    
    if (input$round_select != "All") {
      filtered_fixture <- filtered_fixture %>%
        filter(Round == input$round_select)
    }
    
    if (input$team_filter != "All Teams") {
      filtered_fixture <- filtered_fixture %>%
        filter(Home_Team == input$team_filter | Away_Team == input$team_filter)
    }
    
    filtered_fixture %>%
      select(Round, Date, Matchup, Home_Team, Home_Total, Away_Total, Away_Team) %>%
      datatable(options = list(pageLength = 10, autoWidth = TRUE))
  })
  
  # Player Stats tab outputs
  output$player_info <- renderUI({
    player_data <- afl_details() %>%
      filter(Player == input$player_select) %>%
      head(1)
    
    if (nrow(player_data) == 0) {
      return(HTML("<p>No detailed information available for this player.</p>"))
    }
    
    HTML(paste0(
      "<strong>", player_data$Player, "</strong><br>",
      "Team: ", player_data$Team, "<br>",
      "Position: ", player_data$Position, "<br>",
      "Height: ", player_data$Height_cm, " cm<br>",
      "Weight: ", player_data$Weight_kg, " kg<br>",
      "Debut Year: ", player_data$Debut_Year, "<br>",
      "Draft: ", player_data$Draft_Year, " (Pick ", player_data$Draft_Position, ")"
    ))
  })
  
  output$player_stats_chart <- renderPlotly({
    player_data <- afl_stats() %>%
      filter(Player == input$player_select) %>%
      arrange(Season, Round)
    
    if (nrow(player_data) == 0) {
      return(NULL)
    }
    
    stat_col <- input$stat_type
    
    p <- ggplot(player_data, aes(x = interaction(Season, Round), y = !!sym(stat_col), group = 1)) +
      geom_line() +
      geom_point(aes(text = paste("Round:", Round, "\nSeason:", Season, "\nValue:", !!sym(stat_col)))) +
      labs(title = paste(input$player_select, "-", input$stat_type, "Over Time"), 
           x = "Season-Round", y = input$stat_type) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 8))
    
    ggplotly(p, tooltip = "text")
  })
  
  output$player_averages_table <- renderTable({
    player_avg <- player_averages() %>%
      filter(Player == input$player_select)
    
    recent_avg <- last_10_averages() %>%
      filter(Player == input$player_select)
    
    if (nrow(player_avg) == 0) {
      return(NULL)
    }
    
    key_stats <- c("Disposals", "Marks", "Goals", "Tackles", "Inside_50s", "Rebound_50s", "AFL_Fantasy")
    
    bind_rows(
      player_avg %>% select(Player, all_of(key_stats)) %>% mutate(Period = "Career"),
      recent_avg %>% select(Player, all_of(key_stats)) %>% mutate(Period = "Last 10 Games")
    ) %>%
      select(-Player) %>%
      select(Period, everything()) %>%
      mutate(across(where(is.numeric), round, 1))
  })
  
  output$recent_form_table <- renderDT({
    player_recent <- recent_5_games() %>%
      filter(Player == input$player_select) %>%
      arrange(desc(Season), desc(Round))
    
    if (nrow(player_recent) == 0) {
      return(NULL)
    }
    
    player_recent %>%
      select(Season, Round, Opposition, Disposals, Goals, Behinds, Marks, Tackles, AFL_Fantasy) %>%
      datatable(options = list(pageLength = 5, autoWidth = TRUE))
  })
  
  # Team Comparison tab outputs
  output$team_comparison_chart <- renderPlotly({
    team1_data <- afl_stats() %>%
      filter(Team == input$team1_select & Season == 2025) %>%
      group_by(Round) %>%
      summarise(Value = mean(!!sym(input$comp_stat), na.rm = TRUE)) %>%
      mutate(Team = input$team1_select)
    
    team2_data <- afl_stats() %>%
      filter(Team == input$team2_select & Season == 2025) %>%
      group_by(Round) %>%
      summarise(Value = mean(!!sym(input$comp_stat), na.rm = TRUE)) %>%
      mutate(Team = input$team2_select)
    
    combined_data <- bind_rows(team1_data, team2_data)
    
    p <- ggplot(combined_data, aes(x = Round, y = Value, color = Team, group = Team)) +
      geom_line() +
      geom_point() +
      labs(title = paste("Team Comparison -", input$comp_stat), y = input$comp_stat) +
      theme_minimal()
    
    ggplotly(p)
  })
  
  output$team1_top_players <- renderTable({
    team1_players <- afl_stats() %>%
      filter(Team == input$team1_select & Season == 2025) %>%
      group_by(Player) %>%
      summarise(
        Avg_Stat = mean(!!sym(input$comp_stat), na.rm = TRUE),
        Games = n()
      ) %>%
      arrange(desc(Avg_Stat)) %>%
      head(5) %>%
      mutate(Avg_Stat = round(Avg_Stat, 1))
    
    colnames(team1_players)[2] <- paste("Avg", input$comp_stat)
    team1_players
  })
  
  output$team2_top_players <- renderTable({
    team2_players <- afl_stats() %>%
      filter(Team == input$team2_select & Season == 2025) %>%
      group_by(Player) %>%
      summarise(
        Avg_Stat = mean(!!sym(input$comp_stat), na.rm = TRUE),
        Games = n()
      ) %>%
      arrange(desc(Avg_Stat)) %>%
      head(5) %>%
      mutate(Avg_Stat = round(Avg_Stat, 1))
    
    colnames(team2_players)[2] <- paste("Avg", input$comp_stat)
    team2_players
  })
  
  # Data Tables tab outputs
  output$full_data_table <- renderDT({
    table_data <- switch(input$data_table_select,
                         "Fixtures" = afl_fixture(),
                         "Player Stats" = afl_stats(),
                         "Player Details" = afl_details(),
                         "Lineups" = afl_lineups()
    )
    
    datatable(table_data, options = list(
      pageLength = 10,
      scrollX = TRUE,
      autoWidth = TRUE
    ))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)