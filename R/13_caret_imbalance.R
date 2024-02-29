source(here::here("R", "utilities.R"))
df <- loadRData(here("data", "tidy", "gen2020_onehot.Rda")) %>%
  ## listwise deletion
  filter(!is.na(distance))
assert_that(!any(is.na(df)))

# Parameters ===================================================================
## dp = how much to downsample the majority class?
dp <- seq(0.05, 0.25, by = 0.05)
metric <- "prAUC"
alg <- c("gbm", "ranger") ## things refused to run for ranger > 0.1
yvar <- "gen2020"

# Setup and run downsampling ===================================================
perf_list <- vector("list", length = length(alg))
names(perf_list) <- alg
perf_list <- perf_list %>%
  imap(~ set_names(vector("list", length = length(dp)), nm = dp))

set.seed(123)
library(parallel)
no_cores <- floor(detectCores() / 2)
library(doParallel)
cl <- makeCluster(no_cores)
registerDoParallel(cl)

for (dpx in dp) {
  t1 <- multiclass_train_prep(df)
  t2 <- downSample_custom(t1, p = dpx)
  save(
    t2,
    file = here("data", "tidy", paste0("downsample_list_", dpx * 100, ".Rda"))
  )
}

for (algx in alg) {
  for (dpx in dp) {
    ## Data prep
    load(here("data", "tidy", paste0("downsample_list_", dpx * 100, ".Rda")))
    dim(t2$traind)
    prop(t2$traind, yvar)
    fname <- here(
      "output",
      paste0(algx, "_caret_", metric, "_downsample_", dpx * 100, ".Rda")
    )

    ## Run and save
    if (!file.exists(fname)) {
      model_down <- train(
        gen2020 ~ .,
        data = t2$traind, trControl = t2$tc, method = algx, metric = metric
      )
      save(model_down, file = fname)
    } else {
      load(fname)
    }

    gc(reset = TRUE)

    # Performance summary
    temp2 <- pred_df(t2, model_down)
    perf_list[[algx]][[as.character(dpx)]] <-
      multiClassSummary(temp2, lev = levels(t2$test[[yvar]]))
    save(perf_list, file = here("output", "perf_list.Rda"))

    message(paste0("Task finished: ", algx, ", ", dpx, "."))
  }
}

stopCluster(cl)
registerDoSEQ()

# Performance assessment =======================================================
load(here("output", "perf_list.Rda"))
temp <- perf_list %>%
  map_dfr(
    function(x) {
      x %>%
        compact() %>%
        imap_dfr(~ enframe(.x) %>% mutate(dp = .y))
    },
    .id = "algorithm"
  ) %>%
  pivot_wider(
    id_cols = c("dp", "algorithm"), names_from = "name", values_from = "value"
  )

temp %>% arrange(desc(prAUC))

print(
  xtable(
    temp %>%
      filter(algorithm == "gbm") %>%
      select(
        -algorithm, -contains("Value"), -contains("Balanced"), -Mean_Recall
      ),
    digits = 3
  ),
  file = here("tab", "perf_summ.tex"),
  include.rownames = FALSE, booktabs = TRUE, floating = FALSE
)

print(
  xtable(
    temp %>%
      filter(dp < 0.15) %>%
      rename(Algorithm = algorithm) %>%
      select(
        -contains("Value"), -contains("Balanced"), -Mean_Recall
      ),
    digits = 3
  ),
  file = here("tab", "perf_summ_appendix.tex"),
  include.rownames = FALSE, booktabs = TRUE, floating = FALSE
)

## gradient boosting with 25% downsampling best under current comp. limit; load
algx <- "gbm"
dpx <- 0.25
load(here(
  "output",
  paste0(algx, "_caret_", metric, "_downsample_", dpx * 100, ".Rda")
))
load(here("data", "tidy", paste0("downsample_list_", dpx * 100, ".Rda")))
prop(t2$traind, "gen2020")
temp2 <- pred_df(t2, model_down)

# Importance ===================================================================
varImp(model_down)
lab <- varImp(model_down)$importance %>%
  arrange(desc(Overall)) %>%
  head(10) %>%
  rownames()

## Export; manual label added
pdf_varimp(
  model_down,
  filename = here(
    "fig",
    paste0(algx, "_caret_", metric, "_downsample_", dpx * 100, "_varimp.pdf")
  ),
  font = "CM Roman",
  labels = varimp_labels %>%
    filter(name %in% lab) %>%
    mutate(name = factor(name, levels = lab)) %>%
    arrange(desc(name)) %>%
    .$label,
  width = 5, height = 3
)

## Version 2: reviewer request
lab <- varImp(model_down)$importance %>%
  arrange(desc(Overall)) %>%
  head(20) %>%
  rownames()

pdf_varimp(
  model_down,
  filename = here(
    "fig",
    paste0(algx, "_caret_", metric, "_downsample_", dpx * 100, "_varimp_v2.pdf")
  ),
  font = "CM Roman",
  labels = varimp_labels %>%
    filter(name %in% lab) %>%
    mutate(name = factor(name, levels = lab)) %>%
    arrange(desc(name)) %>%
    .$label %>%
    ## Manual adjustment
    str_pad(width = 57, pad = " "),
  width = 7, height = 3, n_max = 20
)

## yardstick figures
x <- roc_curve(temp2, obs, In_person, Mail, Not_voted)
x <- x %>% mutate(.level = gsub("_", " ", .level))
pdf(
  here(
    "fig",
    paste0(algx, "_caret_", metric, "_downsample_", dpx * 100, "_roc_curve.pdf")
  ),
  width = 6, height = 3
)
print(pdf_default(autoplot(x)))
dev.off()

## https://yardstick.tidymodels.org/reference/pr_auc.html
## debug(yardstick:::pr_auc_multiclass)
x <- pr_curve(temp2, obs, In_person, Mail, Not_voted)
x <- x %>% mutate(.level = gsub("_", " ", .level))
pdf(
  here(
    "fig",
    paste0(algx, "_caret_", metric, "_downsample_", dpx * 100, "_pr_curve.pdf")
  ),
  width = 6, height = 3
)
print(pdf_default(autoplot(x)))
dev.off()
