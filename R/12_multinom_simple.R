source(here::here("R", "utilities.R"))
library(broom)
library(nnet)
library(stargazer)
library(lmtest)
library(sandwich)
library(estimatr)

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

# Multinomial logit for voting mode ============================================
## Execute
fname <- here("output", "multinom_gen2020.Rda")
if (!file.exists(fname)) {
  ## mnl <- multinom(gen2020 ~ ., data = df_onehot)
  ## This causes an issue with the summary.multinom
  mnl <- multinom(
    gen2020 ~ party_oth + party_rep +
      congressional_congressional_2 + congressional_congressional_3 +
      congressional_congressional_4 + congressional_congressional_5 +
      congressional_congressional_6 + congressional_congressional_7 +
      permanent_mail_in_voter_yes + status_inactive + countyalamosa +
      countyarapahoe + countyarchuleta + countybaca +
      countybent + countyboulder + countybroomfield +
      countychaffee + countycheyenne + countyclear_creek +
      countyconejos + countycostilla + countycrowley +
      countycuster + countydelta + countydenver +
      countydolores + countydouglas + countyeagle +
      countyel_paso + countyelbert + countyfremont +
      countygarfield + countygilpin + countygrand +
      countygunnison + countyhinsdale + countyhuerfano +
      countyjackson + countyjefferson + countykiowa +
      countykit_carson + countyla_plata + countylake +
      countylarimer + countylas_animas + countylincoln +
      countylogan + countymesa + countymineral +
      countymoffat + countymontezuma + countymontrose +
      countymorgan + countyotero + countyouray +
      countypark + countyphillips + countypitkin +
      countyprowers + countypueblo + countyrio_blanco +
      countyrio_grande + countyroutt + countysaguache +
      countysan_juan + countysan_miguel + countysedgwick +
      countysummit + countyteller + countywashington +
      countyweld + countyyuma + registration_date +
      gen2018_in_person + gen2018_not_voted + gen2016_in_person +
      gen2016_not_voted + gen2014_in_person + gen2014_not_voted +
      gender_male + gender_missing + pri2020_in_person +
      pri2020_not_voted + pri2018_in_person + pri2018_not_voted +
      pri2016_in_person + pri2016_not_voted + pri2014_in_person +
      pri2014_not_voted + age_groups_gen_x_41_56 + age_groups_gen_z_18_24 +
      age_groups_milennial_25_40 + age_groups_missing + age_groups_silent_75 +
      cases_per_10k + deaths_per_10k + rep_2016 +
      oth_2016 + winner_2016rep,
    data = df_onehot,
    maxit = 1000
  )
  save(mnl, file = fname)
} else {
  load(fname)
}

## Coefficients, prediction, visreg
summ <- summary(mnl)
z <- summ$coefficients / summ$standard.errors
p <- (1 - pnorm(abs(z), 0, 1)) * 2
p
exp(coef(mnl))

## Terrible prediction due to class imbalance (not out-sample)
## Predicts 0.15488% of voters will vote in person
pred <- predict(mnl)
formatC(prop.table(table(pred)) * 100, format = "f", digits = 5) %>%
  .[[2]] %>%
  write(here("tab", "multinom_inperson_prediction_perc.tex"))

# Binary logit for switcher ====================================================
fname <- here("output", "lm_switcher.Rda")
if (!file.exists(fname)) {
  lm_switcher <- lm(
    switcher ~ ., 
    data = df_onehot_switcher %>%
      mutate(switcher = as.numeric(switcher) - 1) %>%
      ## will be dropped
      select(-contains("per_10k"), -contains("_2016"))
  )
  save(lm_switcher, file = fname)
} else {
  load(fname)
}

## Predicts 0.00004% of voters will switch to in person
pred <- predict(lm_switcher)
formatC(prop.table(table(pred)) * 100, format = "f", digits = 5) %>%
  .[[2]] %>%
  write(here("tab", "lm_switcher_prediction_perc.tex"))

## Export summary
# se_switcher <- coeftest(
#   lm_switcher,
#   vcov = vcovHC(lm_switcher, type = "HC0")
# )[, "Std. Error"]
stargazer(
  lm_switcher,
  ## se = se_switcher,
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
