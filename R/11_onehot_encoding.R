source(here::here("R", "utilities.R"))
library(caret)
load(here("data", "tidy", "voter_res_to_box_distance.Rda"))

# Load and setup for filling NA values =========================================
orig <- loadRData(here("data", "tidy", "multiclass_county_collapsed.Rda")) %>%
  left_join(., distance_to_box)
prop(orig, "gen2020")
# gen2020
#    Mail In person Not voted
#    83.0       5.3      11.7

df <- orig %>%
  ungroup() %>%
  select(
    gen2020, party, age, congressional, permanent_mail_in_voter, county,
    status, county_full, registration_date, ## in_person_vote_date
    c(contains("gen"), contains("pri")), age_groups, distance
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
df %>%
  filter(is.na(age)) %>%
  prop(., "party")
# party
#   dem  oth  rep
#  29.3 40.9 29.7

df %>%
  filter(!is.na(age)) %>%
  prop(., "party")
# party
#   dem  oth  rep
#  30.9 40.8 28.3

df %>%
  filter(is.na(age)) %>%
  prop(., "gen2020")
# gen2020
#    Mail In person Not voted
#     0.1       0.0      99.9

df %>%
  filter(!is.na(age)) %>%
  prop(., "gen2020")
# gen2020
#    Mail In person Not voted
#    88.4       5.7       6.0

## Not particularly bound by party but mostly those who have not voted
## Would lose half the nonvoters; thus use age brackets instead

df_under20 <- df %>%
  filter(age < 20) %>%
  select(
    -contains("age"), -contains("2018"), -contains("2016"), -contains("2014")
  )
df <- df %>% select(-age)
## assert_that(!any(is.na(df))) ---> no longer true do to 5% of distance calc
nrow(df) ## 3.7 mill conditional on turnout at least once during 2014--2020

# Add county-level variables ===================================================
load(here("data", "tidy", "co_county_covid_summary.Rda"))
county_pres <- loadRData(here("data", "tidy", "co_county_pres_wide.Rda")) %>%
  filter(year == 2016) %>%
  ungroup() %>%
  select(-year) %>%
  rename(dem_2016 = dem, rep_2016 = rep, oth_2016 = oth) %>%
  mutate(winner_2016 = ifelse(dem_2016 > rep_2016, "dem", "rep")) %>%
  select(-dem_2016)

assert_that(all(sort(county_pres$county) == sort(unique(df$county))))
assert_that(all(sort(unique(county_covd$county)) == sort(unique(df$county))))

df_under20 <- left_join(left_join(df_under20, county_covd), county_pres)
df <- left_join(left_join(df, county_covd), county_pres)
## assert_that(!any(is.na(df)))
## assert_that(!any(is.na(df_under20)))

x <- predict(caret::dummyVars(~., df %>% select(-gen2020), fullRank = TRUE), df)
df_onehot <- as_tibble(x) %>% clean_names()
df_onehot <- bind_cols(df %>% select(gen2020), df_onehot)
save(df_onehot, file = here("data", "tidy", "gen2020_onehot.Rda"))

x <- predict(
  caret::dummyVars(~., df_under20 %>% select(-gen2020), fullRank = TRUE),
  df_under20
)
df_onehot_under20 <- as_tibble(x) %>% clean_names()
df_onehot_under20 <-
  bind_cols(df_under20 %>% select(gen2020), df_onehot_under20)
save(
  df_onehot_under20,
  file = here("data", "tidy", "gen2020_onehot_under20.Rda")
)

# Same one-hot procedure for switcher data =====================================
orig <- loadRData(here("data", "tidy", "switcher.Rda")) %>%
  left_join(., distance_to_box)
prop(orig, "switcher")
# switcher
#   No  Yes
# 96.5  3.5

df <- orig %>%
  ungroup() %>%
  select(
    switcher, party, age, congressional, permanent_mail_in_voter, county,
    status, registration_date, ## in_person_vote_date
    c(contains("gen"), contains("pri")), age_groups, distance
  ) %>%
  select(-contains("reg_")) %>%
  filter(!is.na(congressional)) %>% ## 44 observations
  mutate(gender = ifelse(is.na(gender), "missing", gender)) %>%
  mutate(
    age_groups = ifelse(is.na(age_groups), "missing", as.character(age_groups))
  ) %>%
  mutate(
    registration_date = as.Date("2020-11-06") - registration_date,
    registration_date = as.numeric(registration_date)
  ) %>%
  mutate(across(where(is.character), as.factor)) %>%
  select(-age) %>%
  select(-gen2020)

df <- left_join(left_join(df, county_covd), county_pres)
## assert_that(!any(is.na(df)))

x <-
  predict(caret::dummyVars(~., df %>% select(-switcher), fullRank = TRUE), df)
df_onehot_switcher <- as_tibble(x) %>% clean_names()
df_onehot_switcher <- bind_cols(df %>% select(switcher), df_onehot_switcher)
save(
  df_onehot_switcher,
  file = here("data", "tidy", "switcher_onehot.Rda")
)
