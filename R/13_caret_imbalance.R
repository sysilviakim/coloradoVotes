source(here::here("R", "utilities.R"))
library(caret)
library(gbm)
library(fontcm)

# Load and setup for filling NA values =========================================
orig <- loadRData(here("data", "tidy", "multiclass_county_collapsed.Rda"))
prop(orig, "gen2020")
# gen2020
#    Mail In person Not voted
#    83.0       5.3      11.7

df <- orig %>%
  ungroup() %>%
  select(
    gen2020, party, age, congressional, permanent_mail_in_voter, county,
    status, county_full, registration_date, ## in_person_vote_date
    c(contains("gen"), contains("pri")), age_groups
  ) %>%
  select(-contains("reg_")) %>%
  filter(!is.na(congressional)) %>% ## 386 observations
  mutate(gender = ifelse(is.na(gender), "missing", gender)) %>%
  mutate(
    age_groups = ifelse(is.na(age_groups), "missing", as.character(age_groups))
  ) %>%
  mutate(
    registration_date = as.Date("2020-11-06") - registration_date,
    registration_date = as.numeric(registration_date)
  ) %>%
  mutate(across(where(is.character), as.factor)) %>%
  select(-county) %>%
  rename(county = county_full)

## Age has 220k missing obs
df %>% filter(is.na(age)) %>% prop(., "party")
# party
#   dem  oth  rep
#  29.3 40.9 29.7
df %>% filter(!is.na(age)) %>% prop(., "party")
# party
#   dem  oth  rep
#  30.9 40.8 28.3

df %>% filter(is.na(age)) %>% prop(., "gen2020")
# gen2020
#    Mail In person Not voted
#     0.1       0.0      99.9
df %>% filter(!is.na(age)) %>% prop(., "gen2020")
# gen2020
#    Mail In person Not voted
#    88.4       5.7       6.0

## Not particularly bound by party but mostly those who have not voted
## Would lose half the nonvoters; thus use age brackets instead

df <- df %>% select(-age)
assert_that(!any(is.na(df)))
nrow(df) ## 3.7 mill conditional on turnout at least once during 2014--2020

# Add county-level variables ===================================================
county_covd <- loadRData(here("data", "tidy", "co_county_covid.Rda")) %>%
  mutate(county = tolower(county)) %>%
  ## First vote-by-mail date
  filter(date == as.Date("2020-10-09")) %>%
  select(county, cases_per_10k, deaths_per_10k)

county_pres <- loadRData(here("data", "tidy", "co_county_pres_wide.Rda")) %>%
  filter(year == 2016) %>%
  ungroup() %>%
  select(-year) %>%
  rename(dem_2016 = dem, rep_2016 = rep, oth_2016 = oth) %>%
  mutate(winner_2016 = ifelse(dem_2016 > rep_2016, "dem", "rep"))

assert_that(
  all(sort(county_pres$county) == sort(unique(df$county_full)))
)
assert_that(
  all(sort(county_covd$county) == sort(unique(df$county_full)))
)

df <- left_join(left_join(df, county_covd), county_pres)
assert_that(!any(is.na(df)))

# Parameters ===================================================================
## dp = how much to downsample the majority class?
dp <- c(0.05, 0.1, 0.2)
metric <- "prAUC"
alg <- c("gbm", "ranger")
yvar <- "gen2020"

# Setup and run downsampling ===================================================
perf_list <- vector("list", length = length(alg))
names(perf_list) <- alg
perf_list <- perf_list %>%
  imap(~ set_names(vector("list", length = length(dp)), nm = dp))

for (algx in alg) {
  for (dpx in dp) {
    ## Data prep
    t1 <- multiclass_train_prep(df)
    t2 <- downSample_custom(t1, p = dpx)
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
    perf_list[[algx]][[dpx]] <-
      multiClassSummary(temp2, lev = levels(t2$test[[yvar]]))
    save(perf_list, file = here("output", "perf_list.Rda"))

    message(paste0("Task finished: ", algx, ", ", dpx, "."))
  }
}

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
