source(here::here("R", "utilities.R"))

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
          is.na(gen2018) & is.na(pri2018) & is.na(gen2016) &
            is.na(pri2016) & is.na(gen2014) & is.na(pri2014)
        )
      ~ 1,
      is.na(gen2020) ~ NA_real_,
      TRUE ~ 0
    )
  )

# By-party conditional probability =============================================
pretty_condprob(df, "switcher", 1, "party", "dem", digits = 2)
pretty_condprob(df, "switcher", 1, "party", "rep", digits = 2)
pretty_condprob(df, "switcher", 1, "party", "uaf", digits = 2)

