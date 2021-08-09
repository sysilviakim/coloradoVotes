source(here::here("R", "utilities.R"))
library(gbm)
library(caret)
orig <- loadRData(here("data", "tidy", "multiclass_complete.Rda"))

df <- orig %>% select(-county, -in_person_vote_date)
assert_that(!any(is.na(df)))

# Modeling switchers ===========================================================
df <- df %>%
  filter(
    ## Some pattern, either mail or in person, before 2020 cycle
    gen2018 != "Not voted" | pri2018 != "Not voted" | 
      gen2016 != "Not voted" | pri2016 != "Not voted" | 
      gen2014 != "Not voted" | pri2014 != "Not voted"
  ) %>%
  filter(
    ## Did vote in gen 2020
    gen2020 != "Not voted"
  ) %>%
  mutate(
    switcher = case_when(
      gen2020 == "In person" & (
        gen2018 == "Mail" | pri2018 == "Mail" | 
          gen2016 == "Mail" | pri2016 == "Mail" | 
          gen2014 == "Mail" | pri2014 == "Mail"
      ) ~ "Yes",
      TRUE ~ "No"
    )
  ) %>%
  filter(!(switcher == "No" & gen2020 == "In person")) %>%
  mutate(switcher = factor(switcher))

prop(df, "switchers")

# Setup: downsampling ==========================================================
t1 <- multiclass_train_prep(df, y = "switcher")
t2 <- downSample_custom(t1, p = 0.2, majority = "No", y = "switcher")
fname <- here("output", "ranger_caret_prAUC_downsample_20_switchers.Rda")
t2$traind <- t2$traind %>% select(-gen2020)

# Run model (or export) ========================================================
if (!file.exists(fname)) {
  model_down <- train(
    switcher ~ .,
    ## Given prSummary, AUC = prAUC
    data = t2$traind, trControl = t2$tc, method = "ranger", metric = "AUC"
  )
  save(model_down, file = fname)
} else {
  load(fname)
}

# Performance summary ==========================================================
temp2 <- pred_df(t2, model_down, y = "switcher")
multiClassSummary(temp2, lev = levels(t2$test$switcher))

# Importance ===================================================================
varImp(model_down)
pdf_varimp(
  model_down,
  filename = here("fig", "gbm_caret_prAUC_downsample_10_switcher_varimp.pdf"),
  font = "CM Roman",
  labels = rev(c(
    "Voted by mail at pri. 2020",
    "Voted by mail at gen. 2016",
    "Registration date",
    "Republican",
    "Voted by mail at gen. 2018",
    "Year of birth",
    "Did not vote at gen. 2016",
    "Voted by mail at gen. 2014",
    "Democrat",
    "Did not vote at gen. 2014"
  )),
  width = 5, height = 3
)

## yardstick figures
x <- roc_curve(temp2, obs, No)
pdf(
  here("fig", "gbm_caret_prAUC_downsample_10_roc_curve_switcher.pdf"),
  width = 3, height = 3
)
print(pdf_default(autoplot(x)))
dev.off()

x <- pr_curve(temp2, obs, No)
pdf(
  here("fig", "gbm_caret_prAUC_downsample_10_pr_curve_switcher.pdf"),
  width = 3, height = 3
)
print(pdf_default(autoplot(x)))
dev.off()

