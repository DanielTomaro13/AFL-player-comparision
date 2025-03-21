library(fitzRoy)
library(tidyr)
library(ggplot2)
library(dplyr)

# Get fixture
afl_fixture <- fetch_fixture_afl(2025)

afl_fixture <- afl_fixture %>%
  mutate(matchup = paste(home.team.name, "vs", away.team.name))


# Get lineups for the current round
afl_lineups <- fetch_lineup_afl(2025, round = 1)
afl_lineups <- afl_lineups %>% mutate(
  Player = paste(player.playerName.givenName,player.playerName.surname)
) %>% select(everything())

# Get extra player information
afl_details <- fetch_player_details_afl(2025)
colnames(afl)
afl_details <- afl_details %>% mutate(
  Player = paste(firstName,surname)
) %>% select(
  Player, id, team, season, jumperNumber, position, dateOfBirth, draftYear, heightInCm, weightInKg,
  recruitedFrom, debutYear, draftPosition, draftType
)

afl_lineups_details <- afl_lineups %>% left_join(
  afl_details, by = 'Player'
)

# There is a function in fitzroy that allows for collection of player photo URL can add that in and some other stuff

# Get historical stats - 2010 is the earliest
afl_stats <- fetch_player_stats_footywire(2010:2025)

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

ui <- fluidPage(
  titlePanel("AFL Player Comparison Tool"),
  sidebarLayout(
    sidebarPanel(
      selectInput("fixture", "Select Matchup", choices = unique(afl_fixture$matchup)),
      uiOutput("player1_ui"),
      uiOutput("player2_ui")
    ),
    mainPanel(
      h3("Player Information"),
      fluidRow(
        column(6, tableOutput("player1_info")),
        column(6, tableOutput("player2_info"))
      ),
      h3("Career and Recent Averages"),
      fluidRow(
        column(6, tableOutput("player1_avg")),
        column(6, tableOutput("player2_avg"))
      ),
      h3("Last 5 Games"),
      fluidRow(
        column(6, tableOutput("player1_recent")),
        column(6, tableOutput("player2_recent"))
      )
    )
  )
)

server <- function(input, output, session) {
  
  selected_teams <- reactive({
    req(input$fixture)
    selected <- afl_fixture %>% filter(matchup == input$fixture)
    c(selected$home.team.name, selected$away.team.name)
  })
  
  players_for_teams <- reactive({
    req(selected_teams())
    afl_lineups_details %>%
      filter(team %in% selected_teams())
  })
  
  output$player1_ui <- renderUI({
    req(players_for_teams())
    selectInput("player1", "Select Player 1",
                choices = unique(players_for_teams()$Player[players_for_teams()$team == selected_teams()[1]]))
  })
  
  output$player2_ui <- renderUI({
    req(players_for_teams())
    selectInput("player2", "Select Player 2",
                choices = unique(players_for_teams()$Player[players_for_teams()$team == selected_teams()[2]]))
  })
  
  get_player_info <- function(player) {
    afl_lineups_details %>% filter(Player == player)
  }
  
  get_player_avg <- function(player) {
    left_join(
      player_averages %>% filter(Player == player),
      last_10_averages %>% filter(Player == player),
      by = "Player",
      suffix = c("_Career", "_Last10")
    ) %>%
      select(Player, contains("disposals"), contains("goals"), contains("marks"), contains("tackles"))
  }
  
  get_recent_games <- function(player) {
    recent_5_games %>%
      filter(Player == player) %>%
      select(Season, Round, Team, Opposition, Disposals, Goals, Marks, Tackles)
  }
  
  output$player1_info <- renderTable({
    req(input$player1)
    get_player_info(input$player1)
  })
  
  output$player2_info <- renderTable({
    req(input$player2)
    get_player_info(input$player2)
  })
  
  output$player1_avg <- renderTable({
    req(input$player1)
    get_player_avg(input$player1)
  })
  
  output$player2_avg <- renderTable({
    req(input$player2)
    get_player_avg(input$player2)
  })
  
  output$player1_recent <- renderTable({
    req(input$player1)
    get_recent_games(input$player1)
  })
  
  output$player2_recent <- renderTable({
    req(input$player2)
    get_recent_games(input$player2)
  })
}

shinyApp(ui = ui, server = server)
