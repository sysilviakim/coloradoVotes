source(here::here("R", "utilities.R"))
library(nnet)
load(here("data", "tidy", "multinom_county_collapsed.Rda"))

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

# Missing values ===============================================================
prop(df, "gen2020")
nrow(df %>% filter(is.na(gender)))
nrow(df %>% filter(is.na(yob))) ## significant obs. missing year of birth
nrow(df %>% filter(is.na(party)))
nrow(df %>% filter(is.na(congressional)))
nrow(df %>% filter(is.na(permanent_mail_in_voter)))

# Create listwise deleted data =================================================
df <- df %>% ungroup()
df_listwise <- df %>%
  select(
    gen2020, party, yob, congressional, permanent_mail_in_voter,
    c(contains("gen"), contains("pri"))
  ) %>%
  complete.cases()

df_listwise <- df[df_listwise, ] %>%
  select(
    gen2020, party, yob, congressional, permanent_mail_in_voter,
    c(contains("gen"), contains("pri"))
  )
assert_that(!any(is.na(df_listwise)))
prop(df_listwise, "gen2020") ## not changed substantially
save(df_listwise, file = here("data", "tidy", "multinom_complete.Rda"))

# Multinomial logit ============================================================

## culprit = county_full (original variable): does not converge
## inter_logit <- multinom(gen2020 ~ 1 + county_full, data = df)
## based on table(df$county, df$congressional), choose congressional
## and drop counties

fname <- here("output", "multinom.Rda")
if (!file.exists(fname)) {
  mnl <- multinom(gen2020 ~ ., data = df_listwise)
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
pred <- predict(mnl)
prop.table(table(pred))

# p1 <- visreg(
#   mnl,
#   "party", collapse = TRUE, overlay = TRUE,
#   ylab = "Probability", ylim = c(0, 1), ## partial = FALSE, rug = 2
#   gg = TRUE
#   ## scale = "response" <- does not work
# )
#
# delete_layers(p1, "GeomRug")
