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

afl2024 <- fitzRoy::fetch_player_stats_afltables(season = 2024)
afl2024_new <- fitzRoy::fetch_player_stats_fryzigg(season = 2024)

#Prepare Data
afl2024_home_away <- afl2024 %>% 
  group_by(Round, Home.team) %>% 
  summarise(Away.team = Away.team,
            Ave_Kicks = mean(Kicks),
            Ave_Marks = mean(Marks),
            Margin = Home.score - Away.score, # create margin variable
            Score = ifelse(Margin > 0, 1, 0)) %>% # create win/lose variable 
  unique() %>% 
  filter(Round %in% c(1:25)) %>%  # only include regular rounds
  ungroup()

afl2024_home_away$Round <- as.numeric(afl2024_home_away$Round)

afl2024_final <- afl2024_new %>% 
  group_by(match_round, match_home_team) %>% 
  summarise(Home.team = match_home_team,
            Away.team = match_away_team,
            Round = match_round,
            Ave_Kicks = mean(kicks),
            Ave_Marks = mean(marks),
            Margin = match_home_team_score - match_away_team_score, # create margin variable
            Score = ifelse(Margin > 0, 1, 0)) %>% # create win/lose variable 
  unique() %>% 
  filter(Round %in% c('Finals Week 1', 'Semi Finals', 'Preliminary Finals', 'Grand Final')) %>%  # only include Finals
  ungroup()

#Elo Model - Win probability
elo_score <- function(initial_elos, k, data){
  
  # obtain elo ratings
  elo <- elo::elo.run(formula = Score ~ Home.team + Away.team,
                      initial_elos = initial_elos,
                      k = k,
                      data = data) %>%
    as.data.frame()
  
  data <- data %>% 
    mutate(p.A = elo$p.A) %>% 
    mutate(pred = ifelse(p.A > .5, 1, 0))
  
  cm <- caret::confusionMatrix(data = factor(data$pred, levels = c(0,1)),
                               reference = factor(data$Score, levels = c(0,1)))
  
  return(cm$overall["Accuracy"])
  
}

# Create a grid
params <- expand.grid(init = seq(1000, 5000, by = 50),
                      kfac = seq(10, 50, by = 1))

# Apply function
params$accuracy <- mapply(elo_score, params$init, params$kfac, MoreArgs = list(data = afl2024_home_away))

# What was the best combination? Was it unique?
subset(params, accuracy == max(params$accuracy))

# Apply test parameters on the train data
elo_res <- elo_score(1000, 25, afl2024_home_away)
# Best result: Initial elo rating 1000 to 3000, k factor of any number from 25 to 33, with predictive accuracy of 0.613.

#Use best combination on Test set
elo_res_test <- elo_score(1000, 25, afl2024_final) #Predictive accuracy of 0.625


afl_elo_train <- elo.run(formula = Score ~ Home.team + Away.team,
                         initial_elos = 1000,
                         k = 25,
                         data = afl2024_home_away)

#Final Elo rating
afl_final_elos <- final.elos(afl_elo_train) %>% 
  as.data.frame() %>% 
  rownames_to_column()

#Prepare Data for Models
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
         Home.UncontestedPossession = ifelse(Home.team == Playing.for, sum(Uncontested.Possessions), NA)) %>%
  filter(row_number() == 1) %>% 
  filter(!is.na(Home.Kicks)) %>% #Only need to filter one column
  ungroup() %>% 
  select(Round, Home.team, Home.score, Home.Kicks, Home.Marks, Home.MarksInside50, Home.Disposal, Home.Tackles, Home.Inside50, Home.ContestedPossession, Home.UncontestedPossession)

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
         Away.UncontestedPossession = ifelse(Away.team == Playing.for, sum(Uncontested.Possessions), NA)) %>%
  filter(row_number() == 1) %>% 
  filter(!is.na(Away.Kicks)) %>% 
  ungroup() %>% 
  select(Round, Away.team, Away.score, Away.Kicks, Away.Marks, Away.MarksInside50, Away.Disposal, Away.Tackles, Away.Inside50, Away.ContestedPossession, Away.UncontestedPossession)

afl2024_home_away_dt <- afl2024_home_dt %>% 
  cbind(afl2024_away_dt) %>%
  select(c(1,2,13,3,14,4,15,5,16,6,17,7,18,8,19,9,20,10,21,11,22)) %>% #Column Index
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
         Home.UncontestedPossession = ifelse(match_home_team == player_team, sum(uncontested_possessions), NA)) %>%
  filter(row_number() == 1) %>% 
  filter(!is.na(Home.Kicks)) %>% #Only need to filter on one column
  rename(Round = match_round, Home.team = match_home_team, Away.team = match_away_team, Home.score = match_home_team_score) %>%
  ungroup() %>% 
  select(Round, Home.team, Home.score, Home.Kicks, Home.Marks, Home.MarksInside50, Home.Disposal, Home.Tackles, Home.Inside50, Home.ContestedPossession, Home.UncontestedPossession)

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
         Away.UncontestedPossession = ifelse(match_away_team == player_team, sum(uncontested_possessions), NA)) %>%
  filter(row_number() == 1) %>% 
  filter(!is.na(Away.Kicks)) %>% 
  rename(Round = match_round, Home.team = match_home_team, Away.team = match_away_team, Away.score = match_away_team_score) %>%
  ungroup() %>% 
  select(Round, Away.team, Away.score, Away.Kicks, Away.Marks, Away.MarksInside50, Away.Disposal, Away.Tackles, Away.Inside50, Away.ContestedPossession, Away.UncontestedPossession)

afl2024_finals_dt <- afl2024_finals_home_dt %>% 
  cbind(afl2024_finals_away_dt) %>%
  select(c(1,2,13,3,14,4,15,5,16,6,17,7,18,8,19,9,20,10,21,11,22)) %>% #Column Index
  as_tibble() %>% 
  mutate(Outcome = factor(ifelse(Home.score > Away.score, 'Win', 'Lose')),
         Score = factor(ifelse(Home.score > Away.score, 1, 0)),
         Margin = Home.score - Away.score)

#Decision Tree - Win Probability
afl_dt_spec1 <- decision_tree() %>%
  set_mode('classification') %>%
  set_engine('rpart')

afl_dt_fit1 <- afl_dt_spec1 %>%
  fit(Score ~ Home.Kicks + Home.Marks + Home.MarksInside50 + Home.Disposal + Home.Tackles + Home.Inside50 + Home.ContestedPossession + Home.UncontestedPossession + Away.Kicks + Away.Marks + Away.MarksInside50 + Away.Disposal + Away.Tackles + Away.Inside50 + Away.ContestedPossession + Away.UncontestedPossession, data = afl2024_home_away_dt) 

afl_dt_fit1 %>%
  extract_fit_engine() %>% 
  rpart.plot()

#Determine Model Accuracy
augment(afl_dt_fit1, new_data = afl2024_home_away_dt) %>%
  conf_mat(truth = Score, estimate = .pred_class)

augment(afl_dt_fit1, new_data = afl2024_home_away_dt) %>% 
  accuracy(truth = Score, estimate = .pred_class)

augment(afl_dt_fit1, new_data = afl2024_finals_dt)

DT_folds <- vfold_cv(afl2024_home_away_dt)
DT_grid <- grid_regular(cost_complexity(range = c(-3,-1)), levels = 10)

afl_dt_spec2 <- decision_tree() %>%
  set_mode("classification") %>%
  set_engine("rpart") %>% 
  set_args(cost_complexity = tune())

dt_workflow <- workflow() %>% 
  add_model(afl_dt_spec2) %>% 
  add_formula(Score ~ Home.Kicks + Home.Marks + Home.MarksInside50 + Home.Disposal + Home.Tackles + Home.Inside50 + Home.ContestedPossession + Home.UncontestedPossession + Away.Kicks + Away.Marks + Away.MarksInside50 + Away.Disposal + Away.Tackles + Away.Inside50 + Away.ContestedPossession + Away.UncontestedPossession)

DT2_results <- tune_grid(dt_workflow,
                         resamples = DT_folds,
                         grid = DT_grid,
                         metrics = metric_set(accuracy))

#Select best complexity
autoplot(DT2_results)
best_DT_complexity <- select_best(DT2_results)

#Select best workflow
afl_dt_final <- finalize_workflow(dt_workflow, best_DT_complexity)
afl_dt_final_fit <- fit(afl_dt_final, data = afl2024_home_away_dt)

#Use tuned workflow on Training set
augment(afl_dt_final_fit, new_data = afl2024_home_away_dt) %>% 
  conf_mat(truth = Score, estimate = .pred_class)

augment(afl_dt_final_fit, new_data = afl2024_home_away_dt) %>% 
  accuracy(truth = Score, estimate = .pred_class)

#Use tuned workflow on Test set
augment(afl_dt_final_fit, new_data = afl2024_finals_dt) %>% 
  conf_mat(truth = Score, estimate = .pred_class)

augment(afl_dt_final_fit, new_data = afl2024_finals_dt) %>%
  accuracy(truth = Score, estimate = .pred_class)

#Dataset For Margin Model
afl2024_home_away_margin <- afl2024_home_away_dt

afl2024_finals_margin <- afl2024_finals_dt

#Data Folds on Training set
rf_folds = vfold_cv(afl2024_home_away_margin)

#Random Forest - Margin Model
afl_rf_spec1 <- rand_forest() %>%
  set_mode('regression') %>%
  set_engine('ranger')

afl_rf_fit1 <- afl_rf_spec1 %>%
  fit(Margin ~ Home.Kicks + Home.Marks + Home.MarksInside50 + Home.Disposal + Home.Tackles + Home.Inside50 + Home.ContestedPossession + Home.UncontestedPossession + Away.Kicks + Away.Marks + Away.MarksInside50 + Away.Disposal + Away.Tackles + Away.Inside50 + Away.ContestedPossession + Away.UncontestedPossession, data = afl2024_home_away_margin) 

afl_rf_fit1 %>%
  extract_fit_engine()

#Determine Model Accuracy before tuning
#Test set
augment(afl_rf_fit1, new_data = afl2024_home_away_margin) %>%
  rsq(Margin, .pred)

augment(afl_rf_fit1, new_data = afl2024_home_away_margin) %>% 
  rmse(Margin, .pred)

#Training set
augment(afl_rf_fit1, new_data = afl2024_finals_margin) %>%
  rsq(Margin, .pred)

augment(afl_rf_fit1, new_data = afl2024_finals_margin) %>% 
  rmse(Margin, .pred)

#Tune Model
# Create new spec
afl_rf_spec2 <- rand_forest(mtry = tune(),
                            trees = tune(),
                            min_n = tune()) %>% 
  set_mode("regression") %>% 
  set_engine("ranger")

# Tune grid
afl_rf_tune_results <- tune_grid(
  afl_rf_spec2,
  Margin  ~ .,
  resamples = rf_folds,
  grid = 10,
  metrics = metric_set(rsq, rmse)
)

# Choose best model
best_rmse <- select_best(afl_rf_tune_results, metric = "rmse")
best_rsq <- select_best(afl_rf_tune_results, metric = "rsq")
afl_rf_spec_final <- finalize_model(afl_rf_spec2, best_rmse)
afl_rf_spec_final_rsq <- finalize_model(afl_rf_spec2, best_rsq)

# final results
afl_rf_fit_final <- afl_rf_spec_final %>%
  fit(Margin ~ Home.Kicks + Home.Marks + Home.MarksInside50 + Home.Disposal + Home.Tackles + Home.Inside50 + Home.ContestedPossession + Home.UncontestedPossession + Away.Kicks + Away.Marks + Away.MarksInside50 + Away.Disposal + Away.Tackles + Away.Inside50 + Away.ContestedPossession + Away.UncontestedPossession, data = afl2024_home_away_margin) 

#Test set
augment(afl_rf_fit_final, new_data = afl2024_home_away_margin) %>%
  rsq(Margin, .pred)

augment(afl_rf_fit_final, new_data = afl2024_home_away_margin) %>% 
  rmse(Margin, .pred)

#Training set
augment(afl_rf_fit_final, new_data = afl2024_finals_margin) %>%
  rsq(Margin, .pred)

augment(afl_rf_fit_final, new_data = afl2024_finals_margin) %>% 
  rmse(Margin, .pred)

#Boosted Tree - Margin Model

#Before Tuning
afl_xgb_spec1 <- boost_tree() %>%
  set_mode('regression') %>%
  set_engine('xgboost')

#Fit Training set
afl_xgb_fit1 <- afl_xgb_spec1 %>%
  fit(Margin ~ Home.Kicks + Home.Marks + Home.MarksInside50 + Home.Disposal + Home.Tackles + Home.Inside50 + Home.ContestedPossession + Home.UncontestedPossession + Away.Kicks + Away.Marks + Away.MarksInside50 + Away.Disposal + Away.Tackles + Away.Inside50 + Away.ContestedPossession + Away.UncontestedPossession, data = afl2024_home_away_margin)

augment(afl_xgb_fit1, new_data = afl2024_home_away_margin) %>%
  rsq(Margin, .pred)

augment(afl_xgb_fit1, new_data = afl2024_home_away_margin) %>% 
  rmse(Margin, .pred)

#Fit on Test Set
augment(afl_xgb_fit1, new_data = afl2024_finals_margin) %>%
  rsq(Margin, .pred)

augment(afl_xgb_fit1, new_data = afl2024_finals_margin) %>% 
  rmse(Margin, .pred)

#After Tuning
afl_xgb_spec2 <- boost_tree(min_n = tune(),
                            learn_rate = tune(),
                            tree_depth = tune()) %>%
  set_mode('regression') %>%
  set_engine('xgboost')

afl_xgb_grid <- grid_regular(tree_depth(),
                             min_n(),
                             learn_rate(),
                             levels = 5)

afl_xgb_wf2 <- workflow() %>% 
  add_formula(Margin ~ Home.Kicks + Home.Marks + Home.MarksInside50 + Home.Disposal + Home.Tackles + Home.Inside50 + Home.ContestedPossession + Home.UncontestedPossession + Away.Kicks + Away.Marks + Away.MarksInside50 + Away.Disposal + Away.Tackles + Away.Inside50 + Away.ContestedPossession + Away.UncontestedPossession) %>% #Select factors contributing to wins
  add_model(afl_xgb_spec2)

afl_xgb_folds <- vfold_cv(afl2024_home_away_margin, strata = Margin, v = 5)

afl_xgb_res <- tune_grid(
  afl_xgb_wf2,
  resamples = afl_xgb_folds,
  grid = afl_xgb_grid,
  control = control_grid(save_pred = T)
)

#Select best parameters for rsq
afl_xgb_res %>% 
  collect_metrics() %>% 
  filter(.metric == "rsq") %>%
  select(mean, min_n:learn_rate) %>%
  pivot_longer(min_n:learn_rate,
               values_to = "value",
               names_to = "parameter"
  ) %>%
  ggplot(aes(value, mean, color = parameter)) +
  geom_point(alpha = 0.8, show.legend = FALSE) +
  facet_wrap(~parameter, scales = "free_x") +
  labs(x = NULL, y = "rsq")

show_best(afl_xgb_res, metric = "rsq")
best_rsq <- select_best(afl_xgb_res, metric = "rsq")
best_rsq

#Select best parameters for rmse
afl_xgb_res %>% 
  collect_metrics() %>% 
  filter(.metric == "rmse") %>%
  select(mean, min_n:learn_rate) %>%
  pivot_longer(min_n:learn_rate,
               values_to = "value",
               names_to = "parameter"
  ) %>%
  ggplot(aes(value, mean, color = parameter)) +
  geom_point(alpha = 0.8, show.legend = FALSE) +
  facet_wrap(~parameter, scales = "free_x") +
  labs(x = NULL, y = "RMSE")

show_best(afl_xgb_res, metric = "rmse")
best_rmse <- select_best(afl_xgb_res, metric = "rmse")
best_rmse

#Finalize workflow
afl_final_xgb_rsq <- finalize_workflow(
  afl_xgb_wf2,
  best_rsq
)

afl_final_xgb_rsq %>%
  fit(data = afl2024_home_away_margin) %>%
  pull_workflow_fit() %>%
  vip(geom = "point")

afl_final_xgb_rmse <- finalize_workflow(
  afl_xgb_wf2,
  best_rmse
)

afl_final_xgb_rmse %>%
  fit(data = afl2024_home_away_margin) %>%
  pull_workflow_fit() %>%
  vip(geom = "point")

#Fit model using optimal parameters
afl_xgb_spec3 <- boost_tree(  
  min_n = 2,
  tree_depth = 4,
  learn_rate = 0.1) %>%
  set_mode('regression') %>%
  set_engine('xgboost')

afl_xgb_fit3 <- afl_xgb_spec3 %>%
  fit(Margin ~ Home.Kicks + Home.Marks + Home.MarksInside50 + Home.Disposal + Home.Tackles + Home.Inside50 + Home.ContestedPossession + Home.UncontestedPossession + Away.Kicks + Away.Marks + Away.MarksInside50 + Away.Disposal + Away.Tackles + Away.Inside50 + Away.ContestedPossession + Away.UncontestedPossession, data = afl2024_home_away_margin)

#Fit on Training Set
augment(afl_xgb_fit3, new_data = afl2024_home_away_margin) %>%
  rsq(Margin, .pred)

augment(afl_xgb_fit3, new_data = afl2024_home_away_margin) %>% 
  rmse(Margin, .pred)

#Fit on Test Set
augment(afl_xgb_fit3, new_data = afl2024_finals_margin) %>%
  rsq(Margin, .pred)

augment(afl_xgb_fit3, new_data = afl2024_finals_margin) %>% 
  rmse(Margin, .pred)

#Simulation

#Finals Team
afl_elos_top8 <- 
  afl_final_elos %>% 
  filter(rowname %in% c("Geelong",
                        "Port Adelaide",
                        "Sydney",
                        "Greater Western Sydney",
                        "Carlton",
                        "Brisbane Lions",
                        "Hawthorn",
                        "Western Bulldogs")) %>% 
  mutate(rowname = case_when(
    rowname == 'Brisbane Lions' ~ 'BRI',
    rowname == 'Port Adelaide' ~ 'PORT',
    rowname == 'Western Bulldogs' ~ 'WES',
    rowname == 'Carlton' ~ 'CARL',
    rowname == 'Geelong' ~ 'GEE',
    rowname == 'Greater Western Sydney' ~ 'GWS',
    rowname == 'Hawthorn' ~ 'HAW',
    rowname == 'Sydney' ~ 'SYD'
  ))

#match combinations AFL 2024 finals
grid <- permutations(n = 8, r = 2, c('GEE',
                                     'PORT',
                                     'SYD',
                                     'CARL',
                                     'GWS',
                                     'BRI',
                                     'HAW',
                                     'WES'), 
                     repeats.allowed = FALSE) %>% 
  as.data.frame()
grid

#Probability Using Elo Model
elo_grid_prob <- 
  grid %>% 
  group_by(V1, V2) %>% 
  mutate(
    elo_team = afl_elos_top8$.[afl_elos_top8$rowname == V1],
    elo_opp = afl_elos_top8$.[afl_elos_top8$rowname == V2]
  ) %>%
  mutate(team_Prob = elo.prob(elo_team, elo_opp),
         opp_Prob = 1 - team_Prob) %>%
  select(-c('elo_team', 'elo_opp')) %>%
  rename(team = V1, opp = V2)


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
            Home.UncontestedPossession = mean(Home.UncontestedPossession)) %>% 
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
            Away.UncontestedPossession = mean(Away.UncontestedPossession)) %>% 
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
  augment(afl_dt_final_fit, new_data = grid) %>% 
  select(team, opp, .pred_1, .pred_0) %>% 
  rename(teamProb = 3,
         oppProb = 4)
DT_grid_prob

#Prediction Process Random Forest
RF_grid_prob <-
  rand_forest(
    mtry = 13,
    trees = 1328,
    min_n = 7) %>%
  set_mode('classification') %>%
  set_engine('ranger') %>%
  fit(Score ~ Home.Kicks + Home.Marks + Home.MarksInside50 + Home.Disposal + Home.Tackles +   Home.Inside50 + Home.ContestedPossession + Home.UncontestedPossession + Away.Kicks + Away.Marks + Away.MarksInside50 + Away.Disposal + Away.Tackles + Away.Inside50 + Away.ContestedPossession + Away.UncontestedPossession, data = afl2024_home_away_margin) %>%
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
  fit(Score ~ Home.Kicks + Home.Marks + Home.MarksInside50 + Home.Disposal + Home.Tackles +   Home.Inside50 + Home.ContestedPossession + Home.UncontestedPossession + Away.Kicks + Away.Marks + Away.MarksInside50 + Away.Disposal + Away.Tackles + Away.Inside50 + Away.ContestedPossession + Away.UncontestedPossession, data = afl2024_home_away_margin) %>%
  augment(., new_data = grid) %>% 
  select(team, opp, .pred_1, .pred_0) %>% 
  rename(teamProb = 3,
         oppProb = 4)

#Simulation ELO Model

standings_temp = data.frame()

for(i in 1:100){
  week1 <- data.frame(
    finalID = c('QF1', 'QF2', 'EF1', 'EF2'),
    team = c('SYD', 'PORT', 'BRI', 'WES'),
    opp = c('GWS', 'GEE', 'CARL', 'HAW')
  )
  week1 <- left_join(week1, elo_grid_prob, by = c("team", "opp"))
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
  
  week2 <- left_join(week2, elo_grid_prob, by = c("team", "opp"))
  
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
  
  week3 <- left_join(week3, elo_grid_prob, by = c("team", "opp"))
  
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
  
  week4 <- left_join(week4, elo_grid_prob, by = c("team", "opp"))
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
  
  #Probability
  Elo_probability <- bind_rows()
  
  #Bind rows based on condition
  if(i == 1){
    standings_elo <- data.frame(
      simulation_id = i,
      Eliminated = c('Eliminated EF', 'Eliminated EF', 'Eliminated SF', 'Eliminated SF', 'Eliminated PF', 'Eliminated PF', 'RunnerUp', 'Winner'),
      Team = c(E1L, E2L, SF1L, SF2L, PF1L, PF2L, GFL, GFW)
    )
  }
  else{
    standings_temp <- data.frame(
      simulation_id = i,
      Eliminated = c('Eliminated EF', 'Eliminated EF', 'Eliminated SF', 'Eliminated SF', 'Eliminated PF', 'Eliminated PF', 'RunnerUp', 'Winner'),
      Team = c(E1L, E2L, SF1L, SF2L, PF1L, PF2L, GFL, GFW)
    )
  }
  
  standings_elo <- bind_rows(
    standings_elo,
    standings_temp
  )
}

#Arrange into Table format
Elo_table <- standings_elo %>% 
  group_by(Eliminated, Team) %>% 
  summarise(
    Total = n()
  ) %>% 
  pivot_wider(
    names_from = Eliminated,
    values_from = c(Total),
    values_fill = 0
  )

#Probability of Winning for every match
Elo_probability <- bind_rows(week1_output, week2_output, week3_output, week4_output)


#Simulation Decision Tree Model

standings_temp = data.frame()

for(i in 1:100){
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
    standings_temp <- data.frame(
      simulation_id = i,
      Eliminated = c('Eliminated EF', 'Eliminated EF', 'Eliminated SF', 'Eliminated SF', 'Eliminated PF', 'Eliminated PF', 'RunnerUp', 'Winner'),
      Team = c(E1L, E2L, SF1L, SF2L, PF1L, PF2L, GFL, GFW)
    )
  }
  
  standings_DT <- bind_rows(
    standings_DT,
    standings_temp
  )
}

#Arrange into Table format
DT_table <- standings_DT %>% 
  group_by(Eliminated, Team) %>% 
  summarise(
    Total = n()
  ) %>% 
  pivot_wider(
    names_from = Eliminated,
    values_from = c(Total),
    values_fill = 0
  )

#Probability of Winning for every match
DT_probability <- bind_rows(week1_output, week2_output, week3_output, week4_output)


#Simulation Random Forest Model

standings_temp = data.frame()

for(i in 1:100){
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
    standings_temp <- data.frame(
      simulation_id = i,
      Eliminated = c('Eliminated EF', 'Eliminated EF', 'Eliminated SF', 'Eliminated SF', 'Eliminated PF', 'Eliminated PF', 'RunnerUp', 'Winner'),
      Team = c(E1L, E2L, SF1L, SF2L, PF1L, PF2L, GFL, GFW)
    )
  }
  
  standings_RF <- bind_rows(
    standings_RF,
    standings_temp
  )
}

#Arrange into Table format
RF_table <- standings_RF %>% 
  group_by(Eliminated, Team) %>% 
  summarise(
    Total = n()
  ) %>% 
  pivot_wider(
    names_from = Eliminated,
    values_from = c(Total),
    values_fill = 0
  )

#Probability of Winning for every match
RF_probability <- bind_rows(week1_output, week2_output, week3_output, week4_output)


#Simulation Boosted Tree Model

standings_temp = data.frame()

for(i in 1:100){
  week1 <- data.frame(
    finalID = c('QF1', 'QF2', 'EF1', 'EF2'),
    team = c('SYD', 'PORT', 'BRI', 'WES'),
    opp = c('GWS', 'GEE', 'CARL', 'HAW')
  )
  week1 <- left_join(week1, elo_grid_prob, by = c("team", "opp"))
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
  
  week2 <- left_join(week2, elo_grid_prob, by = c("team", "opp"))
  
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
  
  week3 <- left_join(week3, elo_grid_prob, by = c("team", "opp"))
  
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
  
  week4 <- left_join(week4, elo_grid_prob, by = c("team", "opp"))
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
    standings_temp <- data.frame(
      simulation_id = i,
      Eliminated = c('Eliminated EF', 'Eliminated EF', 'Eliminated SF', 'Eliminated SF', 'Eliminated PF', 'Eliminated PF', 'RunnerUp', 'Winner'),
      Team = c(E1L, E2L, SF1L, SF2L, PF1L, PF2L, GFL, GFW)
    )
  }
  
  standings_xGBoost <- bind_rows(
    standings_xGBoost,
    standings_temp
  )
}

#Arrange into Table format
xGBoost_table <- standings_xGBoost %>% 
  group_by(Eliminated, Team) %>% 
  summarise(
    Total = n()
  ) %>% 
  pivot_wider(
    names_from = Eliminated,
    values_from = c(Total),
    values_fill = 0
  )
#Probability of Winning for every match
xGBoost_probability <- bind_rows(week1_output, week2_output, week3_output, week4_output)