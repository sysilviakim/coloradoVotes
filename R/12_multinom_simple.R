source(here::here("R", "utilities.R"))
library(nnet)
load(here("data", "tidy", "gen2020_onehot.Rda"))
load(here("data", "tidy", "switcher_onehot.Rda"))

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

# Multinomial logit ============================================================

## culprit = county_full (original variable): does not converge
## inter_logit <- multinom(gen2020 ~ 1 + county_full, data = df)
## based on table(df$county, df$congressional), choose congressional
## and drop counties

fname <- here("output", "multinom_gen2020.Rda")
if (!file.exists(fname)) {
  mnl <- multinom(gen2020 ~ ., data = df_onehot)
  save(mnl, file = fname)
} else {
  load(fname)
}

fname <- here("output", "multinom_switcher.Rda")
if (!file.exists(fname)) {
  mnl <- multinom(switcher ~ ., data = df_onehot_switcher, maxit = 1000)
  save(mnl, file = fname)
} else {
  load(fname)
}

# Coefficients, prediction, visreg =============================================
summ <- summary(mnl)
z <- summ$coefficients / summ$standard.errors
p <- (1 - pnorm(abs(z), 0, 1)) * 2
p
exp(coef(mnl))

## Terrible prediction due to class imbalance (not out-sample)
## Predicts 0.15488% of voters will vote in person
## Predicts 0.00819% of voters will switch to in person
pred <- predict(mnl)
formatC(prop.table(table(pred)) * 100, format = "f", digits = 5) %>%
  .[[2]] %>%
  write(here("tab", "multinom_simple_inperson_prediction_perc.tex"))

# p1 <- visreg(
#   mnl,
#   "party", collapse = TRUE, overlay = TRUE,
#   ylab = "Probability", ylim = c(0, 1), ## partial = FALSE, rug = 2
#   gg = TRUE
#   ## scale = "response" <- does not work
# )
#
# delete_layers(p1, "GeomRug")
