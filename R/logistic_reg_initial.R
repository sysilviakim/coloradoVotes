source(here::here("R", "utilities.R"))

# Import history wide data =====================================================
if (nrows == 100) {
  load(here("data", "tidy", "voter_history_wide_sample_final.RData"))
} else {
  load(here("data", "tidy", "voter_history_wide_full.RData"))
}

# Set up =======================================================================
# Check distribution; and format data to remove na values in gen2020: 
reg_data <- subset(wide_profile_temp, !is.na(gen2020)) %>%
  mutate(birth_year = as.integer(birth_year),
         gen2020 = as.factor(gen2020)) %>%
  filter(gender != "unknown") 

# Plot distribution: 
reg_data %>%
  ggplot(aes(gen2020)) +
  geom_bar()

# Also a table to see the disparity in precise numbers
reg_data %>% 
  group_by(gen2020) %>% 
  tally()
# As expected there are significantly more mail votes than in person, so trying
# three approaches. 

# Model 1 ======================================================================
# Split data as it is to have a default model
set.seed(1234)
voting_split <- initial_split(reg_data)
voting_test <- testing(voting_split)
voting_train <- training(voting_split)

# Creat a logistic reg model
vote_model <- logistic_reg() %>%
  fit(gen2020 ~ gender + party + birth_year, data = voting_train)

# Check model
vote_model$fit %>%
  summary()

# Confusion Matrix
augment(vote_model, new_data = voting_test) %>%
  conf_mat(estimate = .pred_class, truth = gen2020)
# Absolutely terrible model, it predicted everything as mail in, no in person
# predictions. Nonetheless, expected. 

# Model 2 ======================================================================
# This time use stratified sampling
voting_split_1 <- initial_split(reg_data, strata = gen2020)
voting_test_1 <- testing(voting_split)
voting_train_1 <- training(voting_split)

# Creat a logistic reg model--only include gender and birth year this time
vote_model_1 <- logistic_reg() %>%
  set_engine("glm") %>%
  fit(gen2020 ~ gender + birth_year, data = voting_train)

# Also create the model with baseR code for ROC curve: 
vote_model_base <- glm(gen2020 ~ gender + birth_year, 
                       family=binomial(link='logit'),
                       voting_train)

# Check model
vote_model_1$fit %>%
  summary()
# Confusion Matrix
augment(vote_model_1, new_data = voting_test_1) %>%
  conf_mat(estimate = .pred_class, truth = gen2020)
# Still Bad. Nothing changed. All predictions are mail-in. 

# Attempt 3 ====================================================================
# Trying basic weighting based on distribution: 
summary(reg_data$gen2020)/nrow(reg_data)

# Add weights:
reg_data_wt <- reg_data %>%
  mutate(wt = case_when(
    gen2020 == 0 ~ 4.6,
    gen2020 == 1 ~ 95.4
  ))

# Regression
vote_model_2 <- glm(gen2020 ~ gender + birth_year, weights = reg_data$wt, 
                    data = reg_data, family = binomial("logit"))

confusion_matrix(vote_model_2)
# No change. 

# Trying to see if adjusting probability threshold might change something:
augment(vote_model_1, new_data = voting_test_1) %>%
  select(.pred_0, .pred_1, gen2020) %>%
  arrange(desc(.pred_1)) %>%
  view()

# Build ROC curve
pred <- predict(vote_model_base, voting_test, type = "response")
predic <- prediction(pred, voting_test$gen2020)
perf <- performance(predic, measure = "tpr", x.measure = "fpr")

# Make an ROC plot
plot(perf)
# Seems awfully close to the x = y line. 
auc(voting_test$gen2020,pred)
# As expected has a very low score: 0.6374 

# Maybe changing the threshold to 6%, 5%, or 4%? 

# Trying different thresholds: 
pred_6 <- ifelse(pred > 0.06,1,0)
auc(voting_test$gen2020,pred_6)

pred_5 <- ifelse(pred > 0.05,1,0)
auc(voting_test$gen2020,pred_5)

pred_4 <- ifelse(pred > 0.04,1,0)
auc(voting_test$gen2020,pred_4)
# It stops at 0.6032 here; not worth going below. The model got worse compared
# to the 0.5 threshold. 

# Set to thresholds: 
conf_vote <- augment(vote_model_1, new_data = voting_test_1) %>%
  mutate(pred_6 = case_when(
    .pred_1 > 0.06 ~ "1",
    TRUE ~ "0"
  ),
  pred_5 = case_when(
    .pred_1 > 0.05 ~ "1",
    TRUE ~ "0"
  ),
  pred_4 = case_when(
    .pred_1 > 0.04 ~ "1",
    TRUE ~ "0"))

# Check confusion matrices: 
conf_vote %>%
  conf_mat(estimate = pred_6, truth = gen2020) %>%
  autoplot(type = "heatmap")

conf_vote %>%
  conf_mat(estimate = pred_5, truth = gen2020) %>%
  autoplot(type = "heatmap")

conf_vote %>%
  conf_mat(estimate = pred_4, truth = gen2020) %>%
  autoplot(type = "heatmap")

# The trade-off for adjusting the threshold to a lower bound is too high;
# it is making marginally higher in-person predictions for a substantial number 
# of wrong mail-in predictions. 
# Although, it was very surprising to see that party wasn't a major predictor in 
# whether the person voted in person or not--perhaps a finding to report? 
# Age and gender were significant predictors, but they did not help much  with 
# the classification model. 



