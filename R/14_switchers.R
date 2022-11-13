source(here::here("R", "utilities.R"))
df <- loadRData(here("data", "tidy", "switcher_onehot.Rda"))

# Parameters ===================================================================
## dp = how much to downsample the majority class?
dp <- seq(0.05, 0.25, by = 0.05)
metric <- "prAUC"
alg <- c("gbm", "ranger")
yvar <- "switcher"

# Setup and run downsampling ===================================================
perf_list <- vector("list", length = length(alg))
names(perf_list) <- alg
perf_list <- perf_list %>%
  imap(~ set_names(vector("list", length = length(dp)), nm = dp))

for (dpx in dp) {
  t1 <- multiclass_train_prep(df, y = "switcher")
  t2 <- downSample_custom(t1, p = dpx, majority = "No", y = "switcher")
  save(
    t2,
    file = here(
      "data", "tidy", paste0("downsample_list_switcher_", dpx * 100, ".Rda")
    )
  )
}

for (algx in alg) {
  for (dpx in dp) {
    ## Data prep
    load(here(
      "data", "tidy", paste0("downsample_list_switcher_", dpx * 100, ".Rda")
    ))
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
    temp2 <- pred_df(t2, model_down, y = "switcher")
    perf_list[[algx]][[as.character(dpx)]] <-
      multiClassSummary(temp2, lev = levels(t2$test[[yvar]]))
    save(perf_list, file = here("output", "perf_list_switcher.Rda"))

    message(paste0("Task finished: ", algx, ", ", dpx, "."))
  }
}

# Performance assessment =======================================================
load(here("output", "perf_list_switcher.Rda"))
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
      select(-algorithm, -contains("Value"), -contains("Balanced"), -Recall),
    digits = 3
  ),
  file = here("tab", "perf_summ_switcher.tex"),
  include.rownames = FALSE, booktabs = TRUE, floating = FALSE
)

print(
  xtable(
    temp %>% 
      rename(Algorithm = algorithm) %>% 
      select(-contains("Value"), -contains("Balanced"), -Recall),
    digits = 3
  ),
  file = here("tab", "perf_summ_switcher_appendix.tex"),
  include.rownames = FALSE, booktabs = TRUE, floating = FALSE
)

## gradient boosting with 15% downsampling best under current seq; load
algx <- "gbm"
dpx <- 0.15
load(here(
  "output",
  paste0(algx, "_caret_", metric, "_downsample_", dpx * 100, "_switch.Rda")
))
load(here(
  "data", "tidy", paste0("downsample_list_switcher_", dpx * 100, ".Rda")
))
temp2 <- pred_df(t2, model_down, y = "switcher")

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
    paste0(
      algx, "_caret_", metric, "_downsample_", dpx * 100, "_varimp_switch.pdf"
    )
  ),
  font = "CM Roman",
  labels = varimp_labels %>%
    filter(name %in% lab) %>%
    mutate(name = factor(name, levels = lab)) %>%
    arrange(desc(name)) %>%
    .$label,
  width = 5, height = 3
)

## yardstick figures
x <- roc_curve(temp2, obs, No)
pdf(
  here(
    "fig",
    paste0(
      algx, "_caret_", metric, "_downsample_", dpx * 100,
      "_roc_curve_switch.pdf"
    )
  ),
  width = 3, height = 3
)
print(pdf_default(autoplot(x)))
dev.off()

x <- pr_curve(temp2, obs, No)
pdf(
  here(
    "fig",
    paste0(
      algx, "_caret_", metric, "_downsample_", dpx * 100, "_pr_curve_switch.pdf"
    )
  ),
  width = 3, height = 3
)
print(pdf_default(autoplot(x)))
dev.off()
