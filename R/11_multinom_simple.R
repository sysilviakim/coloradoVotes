source(here::here("R", "utilities.R"))
library(nnet)
load(here("data", "tidy", "multiclass_county_collapsed.Rda"))

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
    gen2020, party, yob, congressional, permanent_mail_in_voter, county,
    status, county_full, registration_date, ## in_person_vote_date
    c(contains("gen"), contains("pri"))
  ) %>%
  complete.cases()
df_listwise <- df[df_listwise, ]
round(nrow(df_listwise) / nrow(df) * 100, digits = 1) ## 99.4%

df_listwise <- df_listwise %>%
  select(
    gen2020, party, yob, congressional, permanent_mail_in_voter, county,
    status, county_full, registration_date, in_person_vote_date,
    c(contains("gen"), contains("pri"))
  ) %>%
  mutate(in_person_vote_date = mdy(in_person_vote_date)) %>%
  mutate(
    registration_date = as.Date("2020-11-06") - registration_date,
    registration_date = as.numeric(registration_date)
  )

assert_that(!any(is.na(df_listwise %>% select(-in_person_vote_date))))
prop(df_listwise, "gen2020") ## not changed substantially

# Add county-level variables ===================================================
county_covd <- loadRData(here("data", "tidy", "co_county_covid.Rda")) %>%
  mutate(county = tolower(county)) %>%
  ## First vote-by-mail date
  filter(date == as.Date("2020-10-09")) %>%
  select(county_full = county, cases_per_10k, deaths_per_10k)

county_pres <- loadRData(here("data", "tidy", "co_county_pres_wide.Rda")) %>%
  filter(year == 2016) %>%
  ungroup() %>%
  select(-year) %>%
  rename(
    dem_2016 = dem, rep_2016 = rep, oth_2016 = oth,
    county_full = county
  ) %>%
  mutate(winner_2016 = ifelse(dem_2016 > rep_2016, "dem", "rep"))

assert_that(
  all(sort(county_pres$county) == sort(unique(df_listwise$county_full)))
)
assert_that(
  all(sort(county_covd$county) == sort(unique(df_listwise$county_full)))
)

df_listwise <- left_join(left_join(df_listwise, county_covd), county_pres)
assert_that(!any(is.na(df_listwise %>% select(-in_person_vote_date))))
save(df_listwise, file = here("data", "tidy", "multiclass_complete.Rda"))

# Multinomial logit ============================================================

## culprit = county_full (original variable): does not converge
## inter_logit <- multinom(gen2020 ~ 1 + county_full, data = df)
## based on table(df$county, df$congressional), choose congressional
## and drop counties

df_listwise <- df_listwise %>%
  ## nnet::multinom cannot handle otherwise
  select(-county, -county_full, -in_person_vote_date, -oth_2016, -status)

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
## Predicts 0.00694% of voters will vote in person
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
