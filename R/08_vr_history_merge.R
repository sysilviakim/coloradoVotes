source(here::here("R", "utilities.R"))
load(here("data/tidy/df_joined_tidy.RData"))

# Import history wide data =====================================================
if (nrows == 100) {
  load(here("data", "tidy", "sample", "voter_history_wide_sample.RData"))
} else {
  load(here("data", "tidy", "voter_history_wide_full.RData"))
}

# Check voters not in master VR ================================================
temp <- anti_join(voter_history_wide, df_cleaned, by = "voter_id")
nrow(temp)
head(temp)
assert_that(!any(duplicated(temp$voter_id)))

# Check common variables
intersect(colnames(df_cleaned), colnames(voter_history_wide))

# Merge with voter characteristics =============================================
df <- inner_join(
  voter_history_wide %>% rename(county_hist = county_name, party_hist = party),
  df_cleaned %>%
    mutate(
      party = case_when(
        party == "democrat" | party == "democratic" ~ "dem",
        party == "republican" ~ "rep",
        party == "unaffiliated" ~ "uaf",
        party == "libertarian" ~ "lbr",
        party == "american constitution" ~ "acn",
        party == "green" ~ "grn",
        party == "approval" ~ "apv"
      )
    ) %>%
    select(-election_name, -gen2020_file, -election_date, -election_type) %>%
    select(-c(ballot_style, phone, preference, middle_name), everything()),
  by = "voter_id"
) %>%
  select(-county_hist, -party_hist, everything())

# Check if party and county are the same =======================================
## If VR != hist records, use VR's records
df %>% filter(county_hist != county | party_hist != party) %>% nrow() ## 1.4%

if (nrows == 100) {
  save(df, file = here("data", "tidy", "sample", "merged_sample.RData"))
} else {
  save(df, file = here("data", "tidy", "merged_full.RData"))
}
