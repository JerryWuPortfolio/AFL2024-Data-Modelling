
library(tidyverse)
library(elo)
library(fitzRoy)
library(PlayerRatings)
library(caret)
library(tidymodels)
library(rpart.plot)
library(vip)
library(gtools)
library(data.table)

#Import AFL 2024 Season Data
afl2024 <- fitzRoy::fetch_player_stats_afltables(season = 2024)
afl2024_new <- fitzRoy::fetch_player_stats_fryzigg(season = 2024)

#Elo Model Dataset
afl2024_home_away <- afl2024 %>% 
  group_by(Round, Home.team) %>% 
  summarise(Away.team = Away.team,
            Margin = Home.score - Away.score, # create margin variable
            Score = ifelse(Margin > 0, 1, 0)) %>% # create win/lose variable 
  unique() %>% 
  filter(Round %in% c(1:25)) %>%  # only include regular rounds
  ungroup()

afl2024_home_away$Round <- as.numeric(afl2024_home_away$Round)

afl2024_full <- afl2024_new %>% 
  group_by(match_round, match_home_team) %>% 
  summarise(Home.team = match_home_team,
            Away.team = match_away_team,
            Round = match_round,
            Margin = match_home_team_score - match_away_team_score, # create margin variable
            Score = ifelse(Margin > 0, 1, 0)) %>% # create win/lose variable 
  unique() %>% 
  filter(Round %in% c(1:25, 'Finals Week 1', 'Semi Finals', 'Preliminary Finals', 'Grand Final')) %>%  # only include Finals
  mutate(#Round = case_when(Round == 'Finals Week 1' ~ 26)
         Round = replace(Round, Round == 'Finals Week 1', 26),
         Round = replace(Round, Round == 'Semi Finals', 27),
         Round = replace(Round, Round == 'Preliminary Finals', 28),
         Round = replace(Round, Round == 'Grand Final', 29),
         Round = as.numeric(Round)
         )%>%
  ungroup() %>%
  select(c(3:7))

#Tree Model Dataset
afl2024_home_dt <- afl2024 %>% 
  filter(Round %in% c(1:25)) %>%
  group_by(Round, Playing.for) %>%
  mutate(Home.Kicks = ifelse(Home.team == Playing.for, sum(Kicks), NA),
         Home.Marks = ifelse(Home.team == Playing.for, sum(Marks), NA),
         Home.MarksInside50 = ifelse(Home.team == Playing.for, sum(Marks.Inside.50), NA),
         Home.Disposal = ifelse(Home.team == Playing.for, sum(Disposals), NA),
         Home.Tackles = ifelse(Home.team == Playing.for, sum(Tackles), NA),
         Home.Inside50 = ifelse(Home.team == Playing.for, sum(Inside.50s), NA),
         Home.ContestedPossession = ifelse(Home.team == Playing.for, sum(Contested.Possessions), NA),
         Home.UncontestedPossession = ifelse(Home.team == Playing.for, sum(Uncontested.Possessions), NA),
         Home.FreekickFor = ifelse(Home.team == Playing.for, sum(Frees.For), NA),
         Home.FreekickAgainst = ifelse(Home.team == Playing.for, sum(Frees.Against), NA)) %>%
  filter(row_number() == 1) %>% 
  filter(!is.na(Home.Kicks)) %>% #Only need to filter one column
  ungroup() %>% 
  select(Round, Home.team, Home.score, Home.Kicks, Home.Marks, Home.MarksInside50, Home.Disposal, Home.Tackles, Home.Inside50, Home.ContestedPossession, Home.UncontestedPossession, Home.FreekickFor, Home.FreekickAgainst)

afl2024_away_dt <- afl2024 %>% 
  filter(Round %in% c(1:25)) %>%
  group_by(Round, Playing.for) %>%
  mutate(Away.Kicks = ifelse(Away.team == Playing.for, sum(Kicks), NA),
         Away.Marks = ifelse(Away.team == Playing.for, sum(Marks), NA),
         Away.MarksInside50 = ifelse(Away.team == Playing.for, sum(Marks.Inside.50), NA),
         Away.Disposal = ifelse(Away.team == Playing.for, sum(Disposals), NA),
         Away.Tackles = ifelse(Away.team == Playing.for, sum(Tackles), NA),
         Away.Inside50 = ifelse(Away.team == Playing.for, sum(Inside.50s), NA),
         Away.ContestedPossession = ifelse(Away.team == Playing.for, sum(Contested.Possessions), NA),
         Away.UncontestedPossession = ifelse(Away.team == Playing.for, sum(Uncontested.Possessions), NA),
         Away.FreekickFor = ifelse(Away.team == Playing.for, sum(Frees.For), NA),
         Away.FreekickAgainst = ifelse(Away.team == Playing.for, sum(Frees.Against), NA)) %>%
  filter(row_number() == 1) %>% 
  filter(!is.na(Away.Kicks)) %>% 
  ungroup() %>% 
  select(Round, Away.team, Away.score, Away.Kicks, Away.Marks, Away.MarksInside50, Away.Disposal, Away.Tackles, Away.Inside50, Away.ContestedPossession, Away.UncontestedPossession, Away.FreekickFor, Away.FreekickAgainst)

afl2024_home_away_dt <- afl2024_home_dt %>% 
  cbind(afl2024_away_dt) %>%
  select(c(1,2,15,3,16,4,17,5,18,6,19,7,20,8,21,9,22,10,23,11,24,12,25,13,26)) %>% #Column Index
  as_tibble() %>% 
  mutate(Outcome = factor(ifelse(Home.score > Away.score, 'Win', 'Lose')),
         Score = factor(ifelse(Home.score > Away.score, 1, 0)),
         Margin = Home.score - Away.score)


afl2024_finals_home_dt <- afl2024_new %>% 
  filter(match_round %in% c('Finals Week 1', 'Semi Finals', 'Preliminary Finals', 'Grand Final')) %>%
  group_by(match_round, player_team) %>%
  mutate(Home.Kicks = ifelse(match_home_team == player_team, sum(kicks), NA),
         Home.Marks = ifelse(match_home_team == player_team, sum(marks), NA),
         Home.MarksInside50 = ifelse(match_home_team == player_team, sum(marks_inside_fifty), NA),
         Home.Disposal = ifelse(match_home_team == player_team, sum(disposals), NA),
         Home.Tackles = ifelse(match_home_team == player_team, sum(tackles), NA),
         Home.Inside50 = ifelse(match_home_team == player_team, sum(inside_fifties), NA),
         Home.ContestedPossession = ifelse(match_home_team == player_team, sum(contested_possessions), NA),
         Home.UncontestedPossession = ifelse(match_home_team == player_team, sum(uncontested_possessions), NA),
         Home.FreekickFor = ifelse(match_home_team == player_team, sum(free_kicks_for), NA),
         Home.FreekickAgainst = ifelse(match_home_team == player_team, sum(free_kicks_against), NA)) %>%
  filter(row_number() == 1) %>% 
  filter(!is.na(Home.Kicks)) %>% #Only need to filter on one column
  rename(Round = match_round, Home.team = match_home_team, Away.team = match_away_team, Home.score = match_home_team_score) %>%
  ungroup() %>% 
  select(Round, Home.team, Home.score, Home.Kicks, Home.Marks, Home.MarksInside50, Home.Disposal, Home.Tackles, Home.Inside50, Home.ContestedPossession, Home.UncontestedPossession, Home.FreekickFor, Home.FreekickAgainst)

afl2024_finals_away_dt <- afl2024_new %>% 
  filter(match_round %in% c('Finals Week 1', 'Semi Finals', 'Preliminary Finals', 'Grand Final')) %>%
  group_by(match_round, player_team) %>%
  mutate(Away.Kicks = ifelse(match_away_team == player_team, sum(kicks), NA),
         Away.Marks = ifelse(match_away_team == player_team, sum(marks), NA),
         Away.MarksInside50 = ifelse(match_away_team == player_team, sum(marks_inside_fifty), NA),
         Away.Disposal = ifelse(match_away_team == player_team, sum(disposals), NA),
         Away.Tackles = ifelse(match_away_team == player_team, sum(tackles), NA),
         Away.Inside50 = ifelse(match_away_team == player_team, sum(inside_fifties), NA),
         Away.ContestedPossession = ifelse(match_away_team == player_team, sum(contested_possessions), NA),
         Away.UncontestedPossession = ifelse(match_away_team == player_team, sum(uncontested_possessions), NA),
         Away.FreekickFor = ifelse(match_away_team == player_team, sum(free_kicks_for), NA),
         Away.FreekickAgainst = ifelse(match_away_team == player_team, sum(free_kicks_against), NA)) %>%
  filter(row_number() == 1) %>% 
  filter(!is.na(Away.Kicks)) %>% 
  rename(Round = match_round, Home.team = match_home_team, Away.team = match_away_team, Away.score = match_away_team_score) %>%
  ungroup() %>% 
  select(Round, Away.team, Away.score, Away.Kicks, Away.Marks, Away.MarksInside50, Away.Disposal, Away.Tackles, Away.Inside50, Away.ContestedPossession, Away.UncontestedPossession, Away.FreekickFor, Away.FreekickAgainst)

afl2024_finals_dt <- afl2024_finals_home_dt %>% 
  cbind(afl2024_finals_away_dt) %>%
  select(c(1,2,15,3,16,4,17,5,18,6,19,7,20,8,21,9,22,10,23,11,24,12,25,13,26)) %>% #Column Index
  as_tibble() %>% 
  mutate(Outcome = factor(ifelse(Home.score > Away.score, 'Win', 'Lose')),
         Score = factor(ifelse(Home.score > Away.score, 1, 0)),
         Margin = Home.score - Away.score)

#Important metrics Random Forest
afl_rf_spec2 <- rand_forest() %>%
  set_mode('regression') %>%
  set_engine('ranger', importance = "impurity")

afl_rf_grid <- grid_regular(tree_depth(),
                             min_n(),
                             learn_rate(),
                             levels = 5)

afl_rf_wf2 <- workflow() %>% 
  add_formula(Margin ~ Home.Kicks + Home.Marks + Home.MarksInside50 + Home.Disposal
              + Home.Tackles + Home.Inside50 + Home.ContestedPossession  
              + Home.UncontestedPossession + Home.FreekickFor + Home.FreekickAgainst
              + Away.Kicks + Away.Marks + Away.MarksInside50 + Away.Disposal + Away.Tackles
              + Away.Inside50 + Away.ContestedPossession + Away.UncontestedPossession 
              + Away.FreekickFor + Away.FreekickAgainst) %>% #Select factors contributing to wins
  add_model(afl_rf_spec2)

afl_rf_folds <- vfold_cv(afl2024_home_away_dt, strata = Margin, v = 5)

afl_rf_res <- tune_grid(
  afl_rf_wf2,
  resamples = afl_rf_folds,
  grid = afl_rf_grid,
  control = control_grid(save_pred = T)
)

#Select best parameters for rsq
best_rsq_rf <- select_best(afl_rf_res, metric = "rsq")

#Select best parameters for rmse
best_rmse_rf <- select_best(afl_rf_res, metric = "rmse")


#Finalize workflow
afl_final_rf_rsq <- finalize_workflow(
  afl_rf_wf2,
  best_rsq_rf
)

afl_final_rf_rmse <- finalize_workflow(
  afl_rf_wf2,
  best_rmse_rf
)

#Important metrics Boosted Tree
afl_xgb_spec2 <- boost_tree() %>%
  set_mode('regression') %>%
  set_engine('xgboost')

afl_xgb_grid <- grid_regular(tree_depth(),
                             min_n(),
                             learn_rate(),
                             levels = 5)

afl_xgb_wf2 <- workflow() %>% 
  add_formula(Margin ~ Home.Kicks + Home.Marks + Home.MarksInside50 + Home.Disposal
              + Home.Tackles + Home.Inside50 + Home.ContestedPossession  
              + Home.UncontestedPossession + Home.FreekickFor + Home.FreekickAgainst
              + Away.Kicks + Away.Marks + Away.MarksInside50 + Away.Disposal + Away.Tackles
              + Away.Inside50 + Away.ContestedPossession + Away.UncontestedPossession 
              + Away.FreekickFor + Away.FreekickAgainst) %>% #Select factors contributing to wins
  add_model(afl_xgb_spec2)

afl_xgb_folds <- vfold_cv(afl2024_home_away_dt, strata = Margin, v = 5)

afl_xgb_res <- tune_grid(
  afl_xgb_wf2,
  resamples = afl_xgb_folds,
  grid = afl_xgb_grid,
  control = control_grid(save_pred = T)
)

#Select best parameters for rsq
best_rsq <- select_best(afl_xgb_res, metric = "rsq")

#Select best parameters for rmse
best_rmse <- select_best(afl_xgb_res, metric = "rmse")


#Finalize workflow
afl_final_xgb_rsq <- finalize_workflow(
  afl_xgb_wf2,
  best_rsq
)

afl_final_xgb_rmse <- finalize_workflow(
  afl_xgb_wf2,
  best_rmse
)

#Simulation

#match combinations AFL 2024 finals
grid <- gtools::permutations(n = 8, r = 2, c('GEE',
                                     'PORT',
                                     'SYD',
                                     'CARL',
                                     'GWS',
                                     'BRI',
                                     'HAW',
                                     'WES'), 
                     repeats.allowed = FALSE) %>% 
  as.data.frame()

#Probability Using Decision Tree Model
Home <- 
  afl2024_home_away_dt %>% 
  group_by(Home.team) %>% 
  summarise(Home.Kicks = mean(Home.Kicks),
            Home.Marks = mean(Home.Marks),
            Home.MarksInside50 = mean(Home.MarksInside50),
            Home.Disposal = mean(Home.Disposal),
            Home.Tackles = mean(Home.Tackles),
            Home.Inside50 = mean(Home.Inside50),
            Home.ContestedPossession = mean(Home.ContestedPossession),
            Home.UncontestedPossession = mean(Home.UncontestedPossession),
            Home.FreekickFor = mean(Home.FreekickFor),
            Home.FreekickAgainst = mean(Home.FreekickAgainst)) %>% 
  rename(team = Home.team)%>% 
  filter(team %in% c("Geelong",
                     "Port Adelaide",
                     "Sydney",
                     "Greater Western Sydney",
                     "Carlton",
                     "Brisbane Lions",
                     "Hawthorn",
                     "Western Bulldogs")) %>% 
  mutate(team = case_when(
    team == 'Brisbane Lions' ~ 'BRI',
    team == 'Port Adelaide' ~ 'PORT',
    team == 'Western Bulldogs' ~ 'WES',
    team == 'Carlton' ~ 'CARL',
    team == 'Geelong' ~ 'GEE',
    team == 'Greater Western Sydney' ~ 'GWS',
    team == 'Hawthorn' ~ 'HAW',
    team == 'Sydney' ~ 'SYD'
  ))

Away <- 
  afl2024_home_away_dt %>% 
  group_by(Away.team) %>% 
  summarise(Away.Kicks = mean(Away.Kicks),
            Away.Marks = mean(Away.Marks),
            Away.MarksInside50 = mean(Away.MarksInside50),
            Away.Disposal = mean(Away.Disposal),
            Away.Tackles = mean(Away.Tackles),
            Away.Inside50 = mean(Away.Inside50),
            Away.ContestedPossession = mean(Away.ContestedPossession),
            Away.UncontestedPossession = mean(Away.UncontestedPossession),
            Away.FreekickFor = mean(Away.FreekickFor),
            Away.FreekickAgainst = mean(Away.FreekickAgainst)) %>% 
  rename(team = Away.team)%>% 
  filter(team %in% c("Geelong",
                     "Port Adelaide",
                     "Sydney",
                     "Greater Western Sydney",
                     "Carlton",
                     "Brisbane Lions",
                     "Hawthorn",
                     "Western Bulldogs")) %>% 
  mutate(team = case_when(
    team == 'Brisbane Lions' ~ 'BRI',
    team == 'Port Adelaide' ~ 'PORT',
    team == 'Western Bulldogs' ~ 'WES',
    team == 'Carlton' ~ 'CARL',
    team == 'Geelong' ~ 'GEE',
    team == 'Greater Western Sydney' ~ 'GWS',
    team == 'Hawthorn' ~ 'HAW',
    team == 'Sydney' ~ 'SYD'
  )) %>% 
  rename(opp = team)

grid <- 
  grid %>% 
  rename(team = V1,
         opp = V2) %>% 
  left_join(Home) %>% 
  left_join(Away) %>% 
  mutate_if(is.numeric, round, 0)

#Prediction Process Decision Tree
DT_grid_prob <-
  augment(afl_dt_fit1, new_data = grid) %>% 
  select(team, opp, .pred_1, .pred_0) %>% 
  rename(teamProb = 3,
         oppProb = 4)


#List
factor = list("Home.team",                  
              "Away.team",                  
              "Home.score",                
              "Away.score",                 
              "Home.Kicks",                 
              "Away.Kicks",                 
              "Home.Marks",                
              "Away.Marks",                 
              "Home.MarksInside50",         
              "Away.MarksInside50",         
              "Home.Disposal",             
              "Away.Disposal",              
              "Home.Tackles",               
              "Away.Tackles",               
              "Home.Inside50",             
              "Away.Inside50",              
              "Home.ContestedPossession",   
              "Away.ContestedPossession",   
              "Home.UncontestedPossession",
              "Away.UncontestedPossession",
              "Home.FreekickFor",
              "Away.FreekickFor",
              "Home.FreekickAgainst",
              "Away.FreekickAgainst"
              )



ui <- fluidPage(
  tabsetPanel(
    tabPanel("ELO Model", fluid = TRUE,
    titlePanel("Comparing ELO ratings"),
    sidebarLayout(
      sidebarPanel(
        
        # k input
        sliderInput("k_selector",
                    label = "choose value of k",
                    min = 10,
                    max = 50,
                    value = 25),
        
        # selectInput
        selectInput("team_select1",
                    label = "Choose first team:",
                    choices = c("",unique(afl2024_home_away$Home.team))
        ),
        
        selectInput("team_select2",
                    label = "Choose second team:",
                    choices = c("",unique(afl2024_home_away$Home.team))
        )
      ),
      
      mainPanel(
        plotOutput("elo_plot", width = "550px", height = "300px"),
        tableOutput("elo_table")
      )
    )
    ),
    tabPanel("Decision Tree Model", fluid = TRUE,
             sidebarPanel(
               selectInput("DT_metrics",
                  label = "Metrics (select all that apply)",
                  choices = factor,
                  selected = c("Home.Marks"),
                  multiple = T),
               
               sliderInput("DT_complexity",
                           label = "Choose Decision Tree Complexity:",
                           min = 0.001,
                           max = 0.1,
                           value = 0.036
             ),
               
               selectInput("DT_finals_Round",
                          label = "Finals Round:",
                          choices = c("",unique(afl2024_finals_dt$Round)),
                          selected = c("",unique(afl2024_finals_dt$Round)),
                          multiple = T
                          ),
               sliderInput("DT_simulation",
                         label = "Select Simulation Number:",
                         min = 1,
                         max = 200,
                         value = 100)
             ),
             mainPanel(
                tableOutput("DT_simulation_table"),
                tableOutput("DT_finals")
              )
             
             ),
    tabPanel("Random Forest Model", fluid = TRUE,
             sidebarPanel(
               selectInput("RF_metrics",
                           label = "Metrics (select all that apply)",
                           choices = factor,
                           selected = c("Home.Marks"),
                           multiple = T),
               
               sliderInput("RF_mtry",
                           label = "Choose Number of Predictors Randomly Sampled:",
                           min = 1,
                           max = 50,
                           value = 20
               ),
               
               sliderInput("RF_trees",
                           label = "Choose Minimum Number of Trees:",
                           min = 100,
                           max = 1000,
                           value = 455
               ),
               
               sliderInput("RF_min_n",
                           label = "Choose Minimum Number of Points:",
                           min = 1,
                           max = 50,
                           value = 10
               ),
               
               selectInput("RF_finals_Round",
                           label = "Finals Round:",
                           choices = c("",unique(afl2024_finals_dt$Round)),
                           selected = c("",unique(afl2024_finals_dt$Round)),
                           multiple = T
                           ),
               
               sliderInput("RF_simulation",
                           label = "Select Simulation Number:",
                           min = 1,
                           max = 200,
                           value = 100)
             ),
             mainPanel(
               tableOutput("RF_simulation_table"),
               tableOutput("RF_finals")
             )
             
             ),
    tabPanel("Boosted Tree Model", fluid = TRUE,
             sidebarPanel(
               selectInput("Boosted_metrics",
                           label = "Metrics (select all that apply)",
                           choices = factor,
                           selected = c("Home.Marks"),
                           multiple = T),
               
               sliderInput("Boosted_treeDepth",
                           label = "Choose the Maximum Depth of the Tree:",
                           min = 1,
                           max = 50,
                           value = 8
               ),
               
               sliderInput("Boosted_learnRate",
                           label = "Choose the Learning Rate of the Boosting Algorithm:",
                           min = 0.0001,
                           max = 0.3,
                           value = 0.1
               ),
               
               sliderInput("Boosted_min_n",
                           label = "Choose Minimum Number of Points:",
                           min = 1,
                           max = 50,
                           value = 21
               ),
               
               selectInput("Boosted_finals_Round",
                           label = "Finals Round:",
                           choices = c("",unique(afl2024_finals_dt$Round)),
                           selected = c("",unique(afl2024_finals_dt$Round)),
                           multiple = T
               ),
               sliderInput("Boost_simulation",
                           label = "Select Simulation Number:",
                           min = 1,
                           max = 200,
                           value = 100)
             ),
             mainPanel(
               tableOutput("Boost_simulation_table"),
               tableOutput("Boosted_finals")
             )
             
             
             
    )
    )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # Elo plot
  output$elo_plot <- renderPlot({
    
    elo2 <- PlayerRatings::elo(afl2024_full[c("Round", "Home.team","Away.team","Score")], 
                               history = T,
                               kfac = input$k_selector)
    elo2_df <- elo2$history %>% 
      as.data.frame() %>% 
      select(1:28) %>% 
      rownames_to_column() %>% 
      rename(Team = rowname) %>% 
      pivot_longer(cols = 2:29,
                   values_to = "Rating") %>% 
      group_by(Team) %>% 
      mutate(Week = 1:length(Team))
    
    elo2_df %>%
      filter(Team %in% c(input$team_select1, input$team_select2)) %>% 
      ggplot(aes(Week, Rating, col = Team)) +
      geom_line()
  })
  
  # Elo table (for different k)
  output$elo_table <- renderTable({
    elo2 <- PlayerRatings::elo(afl2024_full[c("Round", "Home.team","Away.team","Score")], 
                               history = T,
                               kfac = input$k_selector)
    elo2$ratings %>% as.data.frame()
  })
  
  #Decision Tree Panel
  
  afl_dt_spec1 <- reactive({
    decision_tree(cost_complexity = input$DT_complexity) %>%
      set_mode('classification') %>%
      set_engine('rpart')
  })

    afl_dt_fit1 <- reactive({
      decision_tree(cost_complexity = input$DT_complexity) %>%
        set_mode('classification') %>%
        set_engine('rpart') %>%
      fit(as.formula(paste('Score', '~', paste(input$DT_metrics, collapse = '+'))), 
          data = afl2024_home_away_dt) %>%
        augment(new_data = afl2024_finals_dt) %>% 
        select(c(1:6,30)) %>%
        as.data.frame()
  })
    
    
    afl_dt_home_away <- reactive({
      afl_dt_fit1()
    })
    
    afl_dt_finals <- reactive({
      afl_dt_fit1() %>%
      as.data.frame() %>%
      filter(Round %in% c(input$DT_finals_Round))
    })
  
    #Simulation Decision Tree Model
    
    standings_temp_DT = data.frame()
    
    DT_model_simulation <- reactive({
      for(i in 1:input$DT_simulation){
        week1 <- data.frame(
          finalID = c('QF1', 'QF2', 'EF1', 'EF2'),
          team = c('SYD', 'PORT', 'BRI', 'WES'),
          opp = c('GWS', 'GEE', 'CARL', 'HAW')
        )
        week1 <- left_join(week1, DT_grid_prob, by = c("team", "opp"))
        week1_output <- data.frame()
        QF1 <- week1[1, ]
        QF2 <- week1[2, ]
        EF1 <- week1[3, ]
        EF2 <- week1[4, ]
        
        QF1_res <- sample(c('team', 'opp'), size = 1, prob = c(QF1$teamProb, QF1$oppProb))
        QF2_res <- sample(c('team', 'opp'), size = 1, prob = c(QF2$teamProb, QF2$oppProb))
        EF1_res <- sample(c('team', 'opp'), size = 1, prob = c(EF1$teamProb, EF1$oppProb))
        EF2_res <- sample(c('team', 'opp'), size = 1, prob = c(EF2$teamProb, EF2$oppProb))
        
        QF1 <- QF1 %>% 
          mutate(winner = ifelse(QF1_res == 'team', QF1$team, QF1$opp),
                 loser = ifelse(QF1_res == 'team', QF1$opp, QF1$team))
        QF2 <- 
          QF2 %>% 
          mutate(winner = ifelse(QF2_res == 'team', QF2$team, QF2$opp),
                 loser = ifelse(QF2_res == 'team', QF2$opp, QF2$team))
        
        EF1 <- 
          EF1 %>% 
          mutate(winner = ifelse(EF1_res == 'team', EF1$team, EF1$opp),
                 loser = ifelse(EF1_res == 'team', EF1$opp, EF1$team))
        
        EF2 <- 
          EF2 %>% 
          mutate(winner = ifelse(EF2_res == 'team', EF2$team, EF2$opp),
                 loser = ifelse(EF2_res == 'team', EF2$opp, EF2$team))
        
        week1_output <- 
          week1_output %>% 
          bind_rows(
            QF1,
            QF2,
            EF1,
            EF2
          )
        
        Q1W <- week1_output[1,6]
        Q1L <- week1_output[1,7]
        
        Q2W <- week1_output[2,6]
        Q2L <- week1_output[2,7]
        
        E1W <- week1_output[3,6]
        E1L <- week1_output[3,7]
        
        E2W <- week1_output[4,6]
        E2L <- week1_output[4,7]
        
        # Week 2
        week2 <- data.frame(
          finalID = c('SF1', 'SF2'), #Semi-final1 and Semi-final2
          team = c(Q1L, Q2L),
          opp = c(E1W, E2W)
        )
        
        week2 <- left_join(week2, DT_grid_prob, by = c("team", "opp"))
        
        SF1 <- week2[1, ]
        SF2 <- week2[2, ]
        
        SF1_res <- sample(c('team', 'opp'), size = 1, prob = c(SF1$teamProb, SF1$oppProb))
        SF2_res <- sample(c('team', 'opp'), size = 1, prob = c(SF2$teamProb, SF2$oppProb))
        
        SF1 <- 
          SF1 %>% 
          mutate(winner = ifelse(SF1_res == 'team', SF1$team, SF1$opp),
                 loser = ifelse(SF1_res == 'team', SF1$opp, SF1$team))
        
        SF2 <- 
          SF2 %>% 
          mutate(winner = ifelse(SF2_res == 'team', SF2$team, SF2$opp),
                 loser = ifelse(SF2_res == 'team', SF2$opp, SF2$team))
        
        week2_output <- data.frame()
        week2_output <- 
          week2_output %>% 
          bind_rows(
            SF1,
            SF2
          ) 
        
        SF1W <- week2_output[1, 6]
        SF1L <- week2_output[1, 7]
        
        SF2W <- week2_output[2, 6]
        SF2L <- week2_output[2, 7]
        
        # Week 3
        week3 <- data.frame(
          finalID = c('PF1', 'PF2'), # Prelim-final1 and Prelim-final2
          team = c(Q1W, Q2W),
          opp = c(SF2W, SF1W)
        )
        
        week3 <- left_join(week3, DT_grid_prob, by = c("team", "opp"))
        
        PF1 <- week3[1, ]
        PF2 <- week3[2, ]
        
        PF1_res <- sample(c('team', 'opp'), size = 1, prob = c(PF1$teamProb, PF1$oppProb))
        PF2_res <- sample(c('team', 'opp'), size = 1, prob = c(PF2$teamProb, PF2$oppProb))
        
        PF1 <- 
          PF1 %>% 
          mutate(winner = ifelse(PF1_res == 'team', PF1$team, PF1$opp),
                 loser = ifelse(PF1_res == 'team', PF1$opp, PF1$team))
        
        PF2 <- 
          PF2 %>% 
          mutate(winner = ifelse(PF2_res == 'team', PF2$team, PF2$opp),
                 loser = ifelse(PF2_res == 'team', PF2$opp, PF2$team))
        
        week3_output <- data.frame()
        
        week3_output <- 
          week3_output %>% 
          bind_rows(
            PF1,
            PF2
          )
        
        PF1W <- week3_output[1, 6]
        PF1L <- week3_output[1, 7] # Eliminated
        
        PF2W <- week3_output[2, 6]
        PF2L <- week3_output[2, 7] # Eliminated
        
        # Week 4
        week4 <- data.frame(
          finalID = c('GF'),
          team = c(PF1W),
          opp = c(PF2W)
        )
        
        week4 <- left_join(week4, DT_grid_prob, by = c("team", "opp"))
        GF <- week4[1, ]
        GF_res <- sample(c('team', 'opp'), size = 1, prob = c(GF$teamProb, GF$oppProb))
        GF <- 
          GF %>% 
          mutate(winner = ifelse(GF_res == 'team', GF$team, GF$opp),
                 loser = ifelse(GF_res == 'team', GF$opp, GF$team))
        
        week4_output <- data.frame()
        
        week4_output <- 
          week4_output %>% 
          bind_rows(
            GF
          )
        
        GFW <- week4_output[1, 6] # Winners
        GFL <- week4_output[1, 7] # Runner-ups
        
        #Bind rows based on condition
        if(i == 1){
          standings_DT <- data.frame(
            simulation_id = i,
            Eliminated = c('Eliminated EF', 'Eliminated EF', 'Eliminated SF', 'Eliminated SF', 'Eliminated PF', 'Eliminated PF', 'RunnerUp', 'Winner'),
            Team = c(E1L, E2L, SF1L, SF2L, PF1L, PF2L, GFL, GFW)
          )
        }
        else{
          standings_temp_DT <- data.frame(
            simulation_id = i,
            Eliminated = c('Eliminated EF', 'Eliminated EF', 'Eliminated SF', 'Eliminated SF', 'Eliminated PF', 'Eliminated PF', 'RunnerUp', 'Winner'),
            Team = c(E1L, E2L, SF1L, SF2L, PF1L, PF2L, GFL, GFW)
          )
        }
        
        standings_DT <- bind_rows(
          standings_DT,
          standings_temp_DT
        )
      }
      standings_DT %>% 
        group_by(Eliminated, Team) %>% 
        summarise(
          Total = n()
        ) %>% 
        pivot_wider(
          names_from = Eliminated,
          values_from = c(Total),
          values_fill = 0
        )
    })
    
  output$DT_finals <- renderTable({
    afl_dt_finals()
  })
  
  output$DT_simulation_table <- renderTable({
    DT_model_simulation()
  })

  #Random Forest Panel
  
  afl_rf_fit1 <- reactive({
    rand_forest(mtry = input$RF_mtry,
                  trees = input$RF_trees,
                  min_n = input$RF_min_n) %>%
      set_mode('regression') %>%
      set_engine('ranger') %>%
      fit(as.formula(paste('Margin', '~', paste(input$RF_metrics, collapse = '+'))), 
          data = afl2024_home_away_dt) %>%
      augment(new_data = afl2024_finals_dt) %>% 
      select(c(1:5,30)) %>%
      as.data.frame()
  })
  
  
  afl_dt_home_away <- reactive({
    afl_rf_fit1()
  })
  
  afl_rf_finals <- reactive({
    afl_rf_fit1() %>%
      as.data.frame() %>%
      filter(Round %in% c(input$RF_finals_Round))
  })
  
  #Prediction Process Random Forest
  RF_grid_prob <-
    rand_forest(
      mtry = 13,
      trees = 1328,
      min_n = 7) %>%
    set_mode('classification') %>%
    set_engine('ranger') %>%
    fit(Score ~ Home.Kicks + Home.Marks + Home.MarksInside50 + Home.Disposal + Home.Tackles +   
          Home.Inside50 + Home.ContestedPossession + Home.UncontestedPossession + Home.FreekickFor + Home.FreekickAgainst + Away.Kicks + 
          Away.FreekickFor + Away.FreekickAgainst + Away.Marks + Away.MarksInside50 + Away.Disposal + Away.Tackles + Away.Inside50 + 
          Away.ContestedPossession + Away.UncontestedPossession + Away.FreekickFor + Away.FreekickAgainst, data = afl2024_home_away_dt) %>%
    augment(., new_data = grid) %>% 
    select(team, opp, .pred_1, .pred_0) %>% 
    rename(teamProb = 3,
           oppProb = 4)
  
  #Prediction Process Boosted Tree
  xGBoost_grid_prob <-
    boost_tree(
      min_n = 2,
      tree_depth = 4,
      learn_rate = 0.1) %>%
    set_mode('classification') %>%
    set_engine('xgboost') %>%
    fit(Score ~ Home.Kicks + Home.Marks + Home.MarksInside50 + Home.Disposal + Home.Tackles +   
          Home.Inside50 + Home.ContestedPossession + Home.UncontestedPossession + Home.FreekickFor + Home.FreekickAgainst + Away.Kicks + 
          Away.FreekickFor + Away.FreekickAgainst + Away.Marks + Away.MarksInside50 + Away.Disposal + Away.Tackles + Away.Inside50 + 
          Away.ContestedPossession + Away.UncontestedPossession + Away.FreekickFor + Away.FreekickAgainst, data = afl2024_home_away_dt) %>%
    augment(., new_data = grid) %>% 
    select(team, opp, .pred_1, .pred_0) %>% 
    rename(teamProb = 3,
           oppProb = 4)
  
  #Simulation Random Forest Model
  
  standings_temp_RF = data.frame()
  RF_model_simulation <- reactive({
    for(i in 1:input$RF_simulation){
      week1 <- data.frame(
        finalID = c('QF1', 'QF2', 'EF1', 'EF2'),
        team = c('SYD', 'PORT', 'BRI', 'WES'),
        opp = c('GWS', 'GEE', 'CARL', 'HAW')
      )
      week1 <- left_join(week1, RF_grid_prob, by = c("team", "opp"))
      week1_output <- data.frame()
      QF1 <- week1[1, ]
      QF2 <- week1[2, ]
      EF1 <- week1[3, ]
      EF2 <- week1[4, ]
      
      QF1_res <- sample(c('team', 'opp'), size = 1, prob = c(QF1$teamProb, QF1$oppProb))
      QF2_res <- sample(c('team', 'opp'), size = 1, prob = c(QF2$teamProb, QF2$oppProb))
      EF1_res <- sample(c('team', 'opp'), size = 1, prob = c(EF1$teamProb, EF1$oppProb))
      EF2_res <- sample(c('team', 'opp'), size = 1, prob = c(EF2$teamProb, EF2$oppProb))
      
      QF1 <- QF1 %>% 
        mutate(winner = ifelse(QF1_res == 'team', QF1$team, QF1$opp),
               loser = ifelse(QF1_res == 'team', QF1$opp, QF1$team))
      QF2 <- 
        QF2 %>% 
        mutate(winner = ifelse(QF2_res == 'team', QF2$team, QF2$opp),
               loser = ifelse(QF2_res == 'team', QF2$opp, QF2$team))
      
      EF1 <- 
        EF1 %>% 
        mutate(winner = ifelse(EF1_res == 'team', EF1$team, EF1$opp),
               loser = ifelse(EF1_res == 'team', EF1$opp, EF1$team))
      
      EF2 <- 
        EF2 %>% 
        mutate(winner = ifelse(EF2_res == 'team', EF2$team, EF2$opp),
               loser = ifelse(EF2_res == 'team', EF2$opp, EF2$team))
      
      week1_output <- 
        week1_output %>% 
        bind_rows(
          QF1,
          QF2,
          EF1,
          EF2
        )
      
      Q1W <- week1_output[1,6]
      Q1L <- week1_output[1,7]
      
      Q2W <- week1_output[2,6]
      Q2L <- week1_output[2,7]
      
      E1W <- week1_output[3,6]
      E1L <- week1_output[3,7]
      
      E2W <- week1_output[4,6]
      E2L <- week1_output[4,7]
      
      # Week 2
      week2 <- data.frame(
        finalID = c('SF1', 'SF2'), #Semi-final1 and Semi-final2
        team = c(Q1L, Q2L),
        opp = c(E1W, E2W)
      )
      
      week2 <- left_join(week2, RF_grid_prob, by = c("team", "opp"))
      
      SF1 <- week2[1, ]
      SF2 <- week2[2, ]
      
      SF1_res <- sample(c('team', 'opp'), size = 1, prob = c(SF1$teamProb, SF1$oppProb))
      SF2_res <- sample(c('team', 'opp'), size = 1, prob = c(SF2$teamProb, SF2$oppProb))
      
      SF1 <- 
        SF1 %>% 
        mutate(winner = ifelse(SF1_res == 'team', SF1$team, SF1$opp),
               loser = ifelse(SF1_res == 'team', SF1$opp, SF1$team))
      
      SF2 <- 
        SF2 %>% 
        mutate(winner = ifelse(SF2_res == 'team', SF2$team, SF2$opp),
               loser = ifelse(SF2_res == 'team', SF2$opp, SF2$team))
      
      week2_output <- data.frame()
      week2_output <- 
        week2_output %>% 
        bind_rows(
          SF1,
          SF2
        ) 
      
      SF1W <- week2_output[1, 6]
      SF1L <- week2_output[1, 7]
      
      SF2W <- week2_output[2, 6]
      SF2L <- week2_output[2, 7]
      
      # Week 3
      week3 <- data.frame(
        finalID = c('PF1', 'PF2'), # Prelim-final1 and Prelim-final2
        team = c(Q1W, Q2W),
        opp = c(SF2W, SF1W)
      )
      
      week3 <- left_join(week3, RF_grid_prob, by = c("team", "opp"))
      
      PF1 <- week3[1, ]
      PF2 <- week3[2, ]
      
      PF1_res <- sample(c('team', 'opp'), size = 1, prob = c(PF1$teamProb, PF1$oppProb))
      PF2_res <- sample(c('team', 'opp'), size = 1, prob = c(PF2$teamProb, PF2$oppProb))
      
      PF1 <- 
        PF1 %>% 
        mutate(winner = ifelse(PF1_res == 'team', PF1$team, PF1$opp),
               loser = ifelse(PF1_res == 'team', PF1$opp, PF1$team))
      
      PF2 <- 
        PF2 %>% 
        mutate(winner = ifelse(PF2_res == 'team', PF2$team, PF2$opp),
               loser = ifelse(PF2_res == 'team', PF2$opp, PF2$team))
      
      week3_output <- data.frame()
      
      week3_output <- 
        week3_output %>% 
        bind_rows(
          PF1,
          PF2
        )
      
      PF1W <- week3_output[1, 6]
      PF1L <- week3_output[1, 7] # Eliminated
      
      PF2W <- week3_output[2, 6]
      PF2L <- week3_output[2, 7] # Eliminated
      
      # Week 4
      week4 <- data.frame(
        finalID = c('GF'),
        team = c(PF1W),
        opp = c(PF2W)
      )
      
      week4 <- left_join(week4, RF_grid_prob, by = c("team", "opp"))
      GF <- week4[1, ]
      GF_res <- sample(c('team', 'opp'), size = 1, prob = c(GF$teamProb, GF$oppProb))
      GF <- 
        GF %>% 
        mutate(winner = ifelse(GF_res == 'team', GF$team, GF$opp),
               loser = ifelse(GF_res == 'team', GF$opp, GF$team))
      
      week4_output <- data.frame()
      
      week4_output <- 
        week4_output %>% 
        bind_rows(
          GF
        )
      
      GFW <- week4_output[1, 6] # Winners
      GFL <- week4_output[1, 7] # Runner-ups
      
      #Bind rows based on condition
      if(i == 1){
        standings_RF <- data.frame(
          simulation_id = i,
          Eliminated = c('Eliminated EF', 'Eliminated EF', 'Eliminated SF', 'Eliminated SF', 'Eliminated PF', 'Eliminated PF', 'RunnerUp', 'Winner'),
          Team = c(E1L, E2L, SF1L, SF2L, PF1L, PF2L, GFL, GFW)
        )
      }
      else{
        standings_temp_RF <- data.frame(
          simulation_id = i,
          Eliminated = c('Eliminated EF', 'Eliminated EF', 'Eliminated SF', 'Eliminated SF', 'Eliminated PF', 'Eliminated PF', 'RunnerUp', 'Winner'),
          Team = c(E1L, E2L, SF1L, SF2L, PF1L, PF2L, GFL, GFW)
        )
      }
      
      standings_RF <- bind_rows(
        standings_RF,
        standings_temp_RF
      )
    }
    standings_RF %>% 
      group_by(Eliminated, Team) %>% 
      summarise(
        Total = n()
      ) %>% 
      pivot_wider(
        names_from = Eliminated,
        values_from = c(Total),
        values_fill = 0
      )
  })
  
  
  output$RF_finals <- renderTable({
    afl_rf_finals()
  })
  
  output$RF_simulation_table <- renderTable({
    RF_model_simulation()
  })
  
  #Boosted Tree Panel
  afl_boosted_fit1 <- reactive({
    boost_tree(tree_depth = input$Boosted_treeDepth,
               learn_rate = input$Boosted_learnRate,
                min_n = input$Boosted_min_n) %>%
      set_mode('regression') %>%
      set_engine('xgboost') %>%
      fit(as.formula(paste('Margin', '~', paste(input$Boosted_metrics, collapse = '+'))), 
          data = afl2024_home_away_dt) %>%
      augment(new_data = afl2024_finals_dt) %>% 
      select(c(1:5,30)) %>%
      as.data.frame()
  })
  
  #Simulation Boosted Tree Model
  
  standings_temp_XGBoost = data.frame()
  
  Boost_model_simulation <- reactive({
    for(i in 1:input$Boost_simulation){
      week1 <- data.frame(
        finalID = c('QF1', 'QF2', 'EF1', 'EF2'),
        team = c('SYD', 'PORT', 'BRI', 'WES'),
        opp = c('GWS', 'GEE', 'CARL', 'HAW')
      )
      week1 <- left_join(week1, xGBoost_grid_prob, by = c("team", "opp"))
      week1_output <- data.frame()
      QF1 <- week1[1, ]
      QF2 <- week1[2, ]
      EF1 <- week1[3, ]
      EF2 <- week1[4, ]
      
      QF1_res <- sample(c('team', 'opp'), size = 1, prob = c(QF1$teamProb, QF1$oppProb))
      QF2_res <- sample(c('team', 'opp'), size = 1, prob = c(QF2$teamProb, QF2$oppProb))
      EF1_res <- sample(c('team', 'opp'), size = 1, prob = c(EF1$teamProb, EF1$oppProb))
      EF2_res <- sample(c('team', 'opp'), size = 1, prob = c(EF2$teamProb, EF2$oppProb))
      
      QF1 <- QF1 %>% 
        mutate(winner = ifelse(QF1_res == 'team', QF1$team, QF1$opp),
               loser = ifelse(QF1_res == 'team', QF1$opp, QF1$team))
      QF2 <- 
        QF2 %>% 
        mutate(winner = ifelse(QF2_res == 'team', QF2$team, QF2$opp),
               loser = ifelse(QF2_res == 'team', QF2$opp, QF2$team))
      
      EF1 <- 
        EF1 %>% 
        mutate(winner = ifelse(EF1_res == 'team', EF1$team, EF1$opp),
               loser = ifelse(EF1_res == 'team', EF1$opp, EF1$team))
      
      EF2 <- 
        EF2 %>% 
        mutate(winner = ifelse(EF2_res == 'team', EF2$team, EF2$opp),
               loser = ifelse(EF2_res == 'team', EF2$opp, EF2$team))
      
      week1_output <- 
        week1_output %>% 
        bind_rows(
          QF1,
          QF2,
          EF1,
          EF2
        )
      
      Q1W <- week1_output[1,6]
      Q1L <- week1_output[1,7]
      
      Q2W <- week1_output[2,6]
      Q2L <- week1_output[2,7]
      
      E1W <- week1_output[3,6]
      E1L <- week1_output[3,7]
      
      E2W <- week1_output[4,6]
      E2L <- week1_output[4,7]
      
      # Week 2
      week2 <- data.frame(
        finalID = c('SF1', 'SF2'), #Semi-final1 and Semi-final2
        team = c(Q1L, Q2L),
        opp = c(E1W, E2W)
      )
      
      week2 <- left_join(week2, xGBoost_grid_prob, by = c("team", "opp"))
      
      SF1 <- week2[1, ]
      SF2 <- week2[2, ]
      
      SF1_res <- sample(c('team', 'opp'), size = 1, prob = c(SF1$teamProb, SF1$oppProb))
      SF2_res <- sample(c('team', 'opp'), size = 1, prob = c(SF2$teamProb, SF2$oppProb))
      
      SF1 <- 
        SF1 %>% 
        mutate(winner = ifelse(SF1_res == 'team', SF1$team, SF1$opp),
               loser = ifelse(SF1_res == 'team', SF1$opp, SF1$team))
      
      SF2 <- 
        SF2 %>% 
        mutate(winner = ifelse(SF2_res == 'team', SF2$team, SF2$opp),
               loser = ifelse(SF2_res == 'team', SF2$opp, SF2$team))
      
      week2_output <- data.frame()
      week2_output <- 
        week2_output %>% 
        bind_rows(
          SF1,
          SF2
        ) 
      
      SF1W <- week2_output[1, 6]
      SF1L <- week2_output[1, 7]
      
      SF2W <- week2_output[2, 6]
      SF2L <- week2_output[2, 7]
      
      # Week 3
      week3 <- data.frame(
        finalID = c('PF1', 'PF2'), # Prelim-final1 and Prelim-final2
        team = c(Q1W, Q2W),
        opp = c(SF2W, SF1W)
      )
      
      week3 <- left_join(week3, xGBoost_grid_prob, by = c("team", "opp"))
      
      PF1 <- week3[1, ]
      PF2 <- week3[2, ]
      
      PF1_res <- sample(c('team', 'opp'), size = 1, prob = c(PF1$teamProb, PF1$oppProb))
      PF2_res <- sample(c('team', 'opp'), size = 1, prob = c(PF2$teamProb, PF2$oppProb))
      
      PF1 <- 
        PF1 %>% 
        mutate(winner = ifelse(PF1_res == 'team', PF1$team, PF1$opp),
               loser = ifelse(PF1_res == 'team', PF1$opp, PF1$team))
      
      PF2 <- 
        PF2 %>% 
        mutate(winner = ifelse(PF2_res == 'team', PF2$team, PF2$opp),
               loser = ifelse(PF2_res == 'team', PF2$opp, PF2$team))
      
      week3_output <- data.frame()
      
      week3_output <- 
        week3_output %>% 
        bind_rows(
          PF1,
          PF2
        )
      
      PF1W <- week3_output[1, 6]
      PF1L <- week3_output[1, 7] # Eliminated
      
      PF2W <- week3_output[2, 6]
      PF2L <- week3_output[2, 7] # Eliminated
      
      # Week 4
      week4 <- data.frame(
        finalID = c('GF'),
        team = c(PF1W),
        opp = c(PF2W)
      )
      
      week4 <- left_join(week4, xGBoost_grid_prob, by = c("team", "opp"))
      GF <- week4[1, ]
      GF_res <- sample(c('team', 'opp'), size = 1, prob = c(GF$teamProb, GF$oppProb))
      GF <- 
        GF %>% 
        mutate(winner = ifelse(GF_res == 'team', GF$team, GF$opp),
               loser = ifelse(GF_res == 'team', GF$opp, GF$team))
      
      week4_output <- data.frame()
      
      week4_output <- 
        week4_output %>% 
        bind_rows(
          GF
        )
      
      GFW <- week4_output[1, 6] # Winners
      GFL <- week4_output[1, 7] # Runner-ups
      
      #Bind rows based on condition
      if(i == 1){
        standings_xGBoost <- data.frame(
          simulation_id = i,
          Eliminated = c('Eliminated EF', 'Eliminated EF', 'Eliminated SF', 'Eliminated SF', 'Eliminated PF', 'Eliminated PF', 'RunnerUp', 'Winner'),
          Team = c(E1L, E2L, SF1L, SF2L, PF1L, PF2L, GFL, GFW)
        )
      }
      else{
        standings_temp_XGBoost <- data.frame(
          simulation_id = i,
          Eliminated = c('Eliminated EF', 'Eliminated EF', 'Eliminated SF', 'Eliminated SF', 'Eliminated PF', 'Eliminated PF', 'RunnerUp', 'Winner'),
          Team = c(E1L, E2L, SF1L, SF2L, PF1L, PF2L, GFL, GFW)
        )
      }
      
      standings_xGBoost <- bind_rows(
        standings_xGBoost,
        standings_temp_XGBoost
      )
    }
    standings_xGBoost %>% 
      group_by(Eliminated, Team) %>% 
      summarise(
        Total = n()
      ) %>% 
      pivot_wider(
        names_from = Eliminated,
        values_from = c(Total),
        values_fill = 0
      )
  })
  
  
  afl_dt_home_away <- reactive({
    afl_boosted_fit1()
  })
  
  afl_boosted_finals <- reactive({
    afl_boosted_fit1() %>%
      as.data.frame() %>%
      filter(Round %in% c(input$Boosted_finals_Round))
  })
  
  output$Boosted_finals <- renderTable({
    afl_boosted_finals()
  })
  
  output$Boost_simulation_table <- renderTable({
    Boost_model_simulation()

  })
}

# Run the application 
shinyApp(ui = ui, server = server)