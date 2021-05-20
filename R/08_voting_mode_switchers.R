source(here::here("R", "utilities.R"))
library(msm)

# Import history wide data =====================================================
if (nrows == 100) {
  load(here("data", "tidy", "sample", "voter_history_wide_sample.RData"))
} else {
  load(here("data", "tidy", "voter_history_wide_full.RData"))
}

# Create switchers (init. code) ================================================
df <- voter_history_wide %>%
  select(
    voter_id, gen2020, pri2020, gen2018, pri2018,
    gen2016, pri2016, gen2014, pri2014,
    zip = residential_zip, party, county = county_name
  ) %>%
  arrange(desc(gen2020), voter_id) %>%
  mutate(
    switcher = case_when(
      gen2020 == 1 & (
        (is.na(pri2020) | !is.na(pri2020) & pri2020 == 0) &
          (is.na(gen2018) | !is.na(gen2018) & gen2018 == 0) &
          (is.na(pri2018) | !is.na(pri2018) & pri2018 == 0) &
          (is.na(gen2016) | !is.na(gen2016) & gen2016 == 0) &
          (is.na(pri2016) | !is.na(pri2016) & pri2016 == 0) &
          (is.na(gen2014) | !is.na(gen2014) & gen2014 == 0) &
          (is.na(pri2014) | !is.na(pri2014) & pri2014 == 0)
      ) &
        !(
          is.na(pri2020) & is.na(gen2018) & is.na(pri2018) & is.na(gen2016) &
            is.na(pri2016) & is.na(gen2014) & is.na(pri2014)
        ) ~ 1,
      is.na(gen2020) ~ NA_real_,
      TRUE ~ 0
    )
  )

# By-party conditional probability =============================================
pretty_condprob(df, "switcher", 1, "party", "dem", digits = 2)
pretty_condprob(df, "switcher", 1, "party", "rep", digits = 2)
pretty_condprob(df, "switcher", 1, "party", "uaf", digits = 2)

# Re-construct long data, simplified ===========================================
hist_long <- df %>%
  mutate(
    across(
      c(contains("gen"), contains("pri")),
      ~ case_when(is.na(.x) ~ 2, TRUE ~ .x)
    )
  ) %>%
  pivot_longer(
    cols = c(contains("gen"), contains("pri")), names_to = "election",
    values_to = "state"
  ) %>%
  separate(election, into = c("type", "year"), sep = "20") %>%
  mutate(
    year = case_when(year == "" ~ 20, TRUE ~ as.numeric(year)),
    year = case_when(type == "pri" ~ year - 1, TRUE ~ year)
  ) %>%
  mutate(
    state = case_when(
      state == 0 ~ 1,
      state == 1 ~ 2,
      state == 2 ~ 3
    )
  ) %>%
  arrange(voter_id, year)

# Staying home vs. mail voting vs. in-person voting 3-state MSM ================
statetable.msm(state, voter_id, data = hist_long)
init <- matrix(
  c(0.4, 0.2, 0.4, 0.2, 0.2, 0.6, 0.1, 0.1, 0.8),
  nrow = 3, byrow = TRUE
)
rownames(init) <- colnames(init) <- c("Mail", "In-person", "Nonvoter")

# Run model ====================================================================
full_msm <- msm(
  state ~ year,
  subject = voter_id,
  data = hist_long,
  method = "BFGS",
  control = list(fnscale = 4000, maxit = 10000),
  covariates = ~ party + type,
  qmatrix = init
)

recent_msm <- msm(
  state ~ year,
  subject = voter_id,
  data = hist_long %>% filter(year >= 18),
  method = "BFGS",
  control = list(fnscale = 4000, maxit = 10000),
  covariates = ~ party + type,
  qmatrix = init
)

pmatrix.msm(full_msm)
#                 Mail   In-person  Nonvoter
# Mail      0.45584834 0.009075291 0.5350764
# In-person 0.10544881 0.015974429 0.8785768
# Nonvoter  0.06329851 0.016749392 0.9199521
# t1 not doing anything?

pmatrix.msm(full_msm, covariates = list(party = "rep", type = "gen"))
#                 Mail   In-person  Nonvoter
# Mail      0.40395035 0.006190545 0.5898591
# In-person 0.15819469 0.023048500 0.8187568
# Nonvoter  0.08877164 0.003635028 0.9075933
# Not actually changing with covariate entry

pmatrix.msm(recent_msm)
#                 Mail  In-person  Nonvoter
# Mail      0.37614040 0.01741858 0.6064410
# In-person 0.37830716 0.01812703 0.6035658
# Nonvoter  0.06444978 0.01204890 0.9235013

pmatrix.msm(recent_msm, covariates = list(party = "rep", type = "gen"))
#                Mail  In-person  Nonvoter
# Mail      0.4912323 0.01308536 0.4956824
# In-person 0.3158993 0.07486229 0.6092384
# Nonvoter  0.1484463 0.01450197 0.8370517
