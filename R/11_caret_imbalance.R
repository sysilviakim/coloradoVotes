source(here::here("R", "utilities.R"))
library(caret)
library(gbm)
library(fontcm)
orig <- loadRData(here("data", "tidy", "multiclass_complete.Rda"))
df <- orig %>% select(-county, -in_person_vote_date)
assert_that(!any(is.na(df)))

# prop(
#   df %>%
#     mutate(
#       party = as.character(party),
#       party = case_when(party == "dem" | party == "rep" ~ party, TRUE ~ "oth")
#     ),
#   c("gen2020", "party")
# )

# df %>%
#   group_by(county_full) %>%
#   group_split() %>%
#   `names<-`({.} %>% map(~ .x$county_full[1]) %>% unlist()) %>%
#   map(~ prop(.x, "gen2020"))

# Parameters ===================================================================
dp <- 0.1 # 0.2
metric <- "prAUC"
alg <- "ranger" # "gbm"
yvar <- "gen2020"

# Setup: downsampling ==========================================================
t1 <- multiclass_train_prep(df)
t2 <- downSample_custom(t1, p = dp)
dim(t2$traind)
prop(t2$traind, yvar)
fname <- here(
  "output",
  paste0(alg, "_caret_", metric, "_downsample_", dp * 100, ".Rda")
)

# Run model (or export) ========================================================
if (!file.exists(fname)) {
  model_down <- train(
    gen2020 ~ .,
    data = t2$traind, trControl = t2$tc, method = alg, metric = metric
  )
  save(model_down, file = fname)
} else {
  load(fname)
}

# Performance summary ==========================================================
temp2 <- pred_df(t2, model_down)
multiClassSummary(temp2, lev = levels(t2$test[[yvar]]))

# Importance ===================================================================
varImp(model_down)

## Export; manual label added
pdf_varimp(
  model_down,
  filename = here(
    "fig", 
    paste0(alg, "_caret_", metric, "_downsample_", dp * 100, "_varimp.pdf")
  ),
  font = "CM Roman",
  # labels = rev(
  #   c(
  #     "Reg. date",
  #     "Voted by mail at pri. 2020",
  #     "Inactive",
  #     "Did not vote at gen. 2016",
  #     "Voted by mail at gen. 2018",
  #     "Did not vote at pri. 2020",
  #     "Voted by mail at gen. 2016",
  #     "Did not vote at gen. 2018",
  #     "Year of birth",
  #     "Republican"
  #   )
  # ),
  width = 5, height = 3
)

## yardstick figures
x <- roc_curve(temp2, obs, In_person, Mail, Not_voted)
x <- x %>% mutate(.level = gsub("_", " ", .level))
pdf(
  here(
    "fig", 
    paste0(alg, "_caret_", metric, "_downsample_", dp * 100, "_roc_curve.pdf")
  ),
  width = 6, height = 3
)
print(pdf_default(autoplot(x)))
dev.off()

x <- pr_curve(temp2, obs, In_person, Mail, Not_voted)
x <- x %>% mutate(.level = gsub("_", " ", .level))
pdf(
  here(
    "fig", 
    paste0(alg, "_caret_", metric, "_downsample_", dp * 100, "_pr_curve.pdf")
  ),
  width = 6, height = 3
)
print(pdf_default(autoplot(x)))
dev.off()
