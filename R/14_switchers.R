source(here::here("R", "utilities.R"))
library(gbm)
library(caret)
orig <- loadRData(here("data", "tidy", "switcher.Rda"))

df <- orig %>% select(-in_person_vote_date)
assert_that(!any(is.na(df)))
prop(df, "switchers")

# Parameters ===================================================================
## dp = how much to downsample the majority class?
dp <- c(0.05, 0.1, 0.2)
metric <- "AUC"
alg <- c("gbm", "ranger")
yvar <- "switcher"

# Setup and run downsampling ===================================================
perf_list <- vector("list", length = length(alg))
names(perf_list) <- alg
perf_list <- perf_list %>%
  imap(~ set_names(vector("list", length = length(dp)), nm = dp))

for (algx in alg) {
  for (dpx in dp) {
    ## Data prep
    t1 <- multiclass_train_prep(df, y = "switcher")
    t2 <- downSample_custom(t1, p = dpx, majority = "No", y = "switcher")
    t2$traind <- t2$traind %>% select(-gen2020)

    dim(t2$traind)
    prop(t2$traind, yvar)
    fname <- here(
      "output",
      paste0(algx, "_caret_", metric, "_downsample_", dpx * 100, "_switch.Rda")
    )
    
    ## Run and save
    if (!file.exists(fname)) {
      model_down <- train(
        switcher ~ .,
        data = t2$traind, trControl = t2$tc, method = algx, metric = metric
      )
      save(model_down, file = fname)
    } else {
      load(fname)
    }
    
    gc(reset = TRUE)
    
    # Performance summary
    temp2 <- pred_df(t2, model_down)
    perf_list[[algx]][[dpx]] <- 
      multiClassSummary(temp2, lev = levels(t2$test[[yvar]]))
    save(perf_list, file = here("output", "perf_list_switcher.Rda"))
    
    message(paste0("Task finished: ", algx, ", ", dpx, "."))
  }
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

