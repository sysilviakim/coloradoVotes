source(here::here("R", "utilities.R"))

# Import history wide data =====================================================
if (nrows == 100) {
  load(here("data", "tidy", "voter_history_wide_sample_final.RData"))
} else {
  load(here("data", "tidy", "voter_history_wide_full.RData"))
}

# Set up =======================================================================
# Clean data: 
reg_data <- subset(wide_profile_temp, !is.na(gen2020)) %>%
  filter(gender != "unknown") %>%
  mutate(birth_year = as.numeric(birth_year),
         gen2020 = as.factor(gen2020),
         gender_en = ifelse(gender %in% "female", 1, 0),
         gender_en = as.numeric(gender_en)) %>%
  select(gen2020, gender_en, birth_year)

# Set model specs: 
log_spec <- logistic_reg() %>%
  set_engine("glm") %>%
  set_mode("classification")

# Set workflow: 
log_wf <- workflow() %>%
  add_model(log_spec) %>%
  add_formula(gen2020 ~ gender_en + birth_year)

# Create a split; 
set.seed(1234)
vote_split <- initial_split(reg_data, prop = 0.5)
vote_train <- training(vote_split)
vote_test <- testing(vote_split)

# Check distribution: 
prop.table(table(reg_data$gen2020))
# 0         1 
# 0.9542503 0.0457497 

prop.table(table(vote_test$gen2020))
# 0          1 
# 0.94864227 0.05135773 

prop.table(table(vote_train$gen2020))
# 0          1 
# 0.95985832 0.04014168 
# Splits are representative. 

# Models =======================================================================
# Simple logistic regression: 
vote_log <- fit(log_wf, data = vote_train)

# Confusion matrix: 
augment(vote_log, new_data = vote_test) %>%
  conf_mat(estimate = .pred_class, truth = gen2020)
#           Truth
# Prediction    0    1
#          0 1607   87
#          1    0    0
# Even I can make this prediction; this is not a great model. 

# Trying different balancing methods: 
# Oversampling: 
vote_over <- ovun.sample(gen2020 ~ ., vote_train,
                         method = "over", p = 0.5)$data

prop.table(table(vote_over$gen2020))
# 0         1 
# 0.5007699 0.4992301 

# Undersampling: 
vote_under <- ovun.sample(gen2020 ~ ., vote_train,
                          method = "under", p = 0.5)$data

prop.table(table(vote_under$gen2020))
# 0         1 
# 0.5142857 0.4857143 

# Both: 
vote_both <- ovun.sample(gen2020 ~ ., vote_train,
                         method = "both", N = nrow(vote_train), p = 0.5)$data

prop.table(table(vote_under$gen2020))
# 0         1 
# 0.5142857 0.4857143 

# ROSE:
vote_rose <- ROSE(gen2020 ~ ., vote_train)$data

prop.table(table(vote_rose$gen2020))
# 0         1 
# 0.4769776 0.5230224 

# SMOTE;
vote_smote <- smote(gen2020~., vote_train, perc.over = 1, perc.under = 2)

prop.table(table(vote_smote$gen2020))
# 0   1 
# 0.5 0.5 

# Fit different models: 
model_under <- fit(log_wf, data = vote_under)
model_over <- fit(log_wf, data = vote_over)
model_both <- fit(log_wf, data = vote_both)
model_rose <- fit(log_wf, data = vote_rose)
model_smote <- fit(log_wf, data = vote_smote)

# Area under ROCs: 
predict_1 <- augment(model_under, vote_test) %>%
  pull(.pred_1)
roc.curve(vote_test$gen2020, predict_1)
# Area under the curve (AUC): 0.627

predict_2 <- augment(model_over, vote_test) %>%
  pull(.pred_1)
roc.curve(vote_test$gen2020, predict_2)
# Area under the curve (AUC): 0.627

predict_3 <- augment(model_both, vote_test) %>%
  pull(.pred_1)
roc.curve(vote_test$gen2020, predict_3)
# Area under the curve (AUC): 0.627

predict_4 <- augment(model_rose, vote_test) %>%
  pull(.pred_1)
roc.curve(vote_test$gen2020, predict_4)
# Area under the curve (AUC): 0.627

predict_4 <- augment(model_smote, vote_test) %>%
  pull(.pred_1)
roc.curve(vote_test$gen2020, predict_4)
# Area under the curve (AUC): 0.626

# They aren't that better than the initial model--0.627 is very low. 




