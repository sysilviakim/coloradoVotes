source(here::here("R", "utilities.R"))
library(broom)
library(nnet)
library(stargazer)
library(lmtest)
library(sandwich)
library(estimatr)

load(here("data", "tidy", "gen2020_onehot.Rda"))
load(here("data", "tidy", "switcher_onehot.Rda"))

## Unit conversion!!! Meter to mile
df_onehot <- df_onehot %>%
  mutate(distance = distance / 1609.344) %>%
  filter(!is.na(distance))
df_onehot_switcher <- df_onehot_switcher %>%
  mutate(distance = distance / 1609.344) %>%
  filter(!is.na(distance))

# Discrepancy betwen official tally ============================================
## Using `2020GEPrecinctLevelTurnoutPosted.xlsx`
## total ballots = 3,291,661 as opposed to VF data = 3,280,335
## 11,326 voters short (not too bad!)
## sum of active voters = 3,803,765 as opposed to 3,713,023
## 90,742 voters short (could improve!)

## while I would like to take the discrete choice panel approach,
## very little info that changes; not like the travel mode/cereal choice problem
## age, gender, yob, res. address, party; hardly ever changes
## inappropriate

# Multinomial logit for voting mode ============================================
## Execute
fname <- here("output", "multinom_gen2020.Rda")
if (!file.exists(fname)) {
  ## mnl <- multinom(gen2020 ~ ., data = df_onehot)
  ## This causes an issue with the summary.multinom
  mnl <- multinom(gen2020 ~ ., data = df_onehot, maxit = 1000)
  save(mnl, file = fname)
} else {
  load(fname)
}

## Coefficients, prediction
summ <- summary(mnl)
z <- summ$coefficients / summ$standard.errors
p <- (1 - pnorm(abs(z), 0, 1)) * 2
p
exp(coef(mnl))

## Terrible prediction due to class imbalance (not out-sample)
pred <- predict(mnl)
formatC(prop.table(table(pred)) * 100, format = "f", digits = 5) %>%
  .[[2]] %>%
  write(here("tab", "multinom_inperson_prediction_perc.tex"))

xvar <- names(as_tibble(coef(mnl)))
stargazer(
  mnl,
  ## se = se_switcher,
  covariate.labels = varimp_labels %>%
    filter(name %in% xvar) %>%
    filter(!grepl("county", name)) %>%
    mutate(
      name = factor(
        name,
        levels = setdiff(xvar, "(Intercept)")
      )
    ) %>%
    arrange(name) %>%
    .$label,
  omit = c(xvar[grepl("county", xvar)], "Constant"),
  dep.var.labels.include = FALSE,
  header = FALSE, model.numbers = FALSE,
  dep.var.caption = "Turnout/Voting Mode",
  out = here("tab", "mnl_reg.tex"), float = FALSE,
  omit.stat = c("f", "ser"), star.cutoffs = c(0.05, 0.01, 0.001),
  no.space = TRUE, notes = "County fixed controls omitted."
)

# Binary LPM for switcher ======================================================
fname <- here("output", "lm_switcher.Rda")
if (!file.exists(fname)) {
  lm_switcher <- lm(
    switcher ~ .,
    data = df_onehot_switcher %>%
      mutate(switcher = as.numeric(switcher) - 1) %>%
      ## let's keep not county indicators but county-level cont. variables
      select(-contains("county")) %>%
      ## edit: delete the differentiation of mail vs. in-person
      ## leave only whether the voter did not vote in that election
      select(-contains("_in_person"))
  )
  save(lm_switcher, file = fname)
} else {
  load(fname)
}

## Prediction
pred <- case_when(
  predict(lm_switcher) > 0.5 ~ "switcher",
  TRUE ~ "non-switcher"
)
table(pred) ## everyone is a non-switcher
table(df_onehot_switcher$switcher)
# formatC(prop.table(table(pred)) * 100, format = "f", digits = 5) %>%
#   .[[2]] %>%
#   write(here("tab", "lm_switcher_prediction_perc.tex"))

## Export summary
cov1 <- vcovHC(lm_switcher, type = "HC1")
robust_se <- sqrt(diag(cov1))
stargazer(
  lm_switcher,
  se = list(NULL, robust_se),
  covariate.labels = varimp_labels %>%
    filter(name %in% names(coef(lm_switcher))) %>%
    filter(!grepl("county", name)) %>%
    mutate(
      name = factor(
        name,
        levels = setdiff(names(coef(lm_switcher)), "(Intercept)")
      )
    ) %>%
    arrange(name) %>%
    .$label,
  omit = c(
    names(coef(lm_switcher))[grepl("county", names(coef(lm_switcher)))],
    "Constant"
  ),
  dep.var.labels.include = FALSE,
  header = FALSE, model.numbers = FALSE,
  dep.var.caption = "Switching to In-person Voting",
  out = here("tab", "lm_switcher.tex"), float = FALSE,
  omit.stat = c("f", "ser"), star.cutoffs = c(0.05, 0.01, 0.001),
  no.space = TRUE, notes = "County fixed controls omitted."
)
