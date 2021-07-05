source(here::here("R", "utilities.R"))
if (nrows == 100) {
  load(here("data/tidy/sample/voter_history_long_sample.RData"))
} else {
  load(here("data/tidy/voter_history_long_full.RData"))
}

# Select variables and pivot to wide =========================================== 
wide_temp <- voter_history_long %>%
  filter(election_type != "oth") %>%
  select(voter_id, election, vote_method) %>%
  dedup() %>%
  group_by(voter_id, election) %>%
  ## If both records exist, collapse as in-person voter
  summarise(vote_method = sum(vote_method, na.rm = TRUE)) %>%
  arrange(voter_id, election, vote_method) %>%
  pivot_wider(names_from = election, values_from = vote_method) %>%
  select(
    voter_id, 
    matches(
      cross2(c("gen", "pri"), seq(2020, 2014, by = -2)) %>%
        map_chr(~ paste0(.x, collapse = "")) %>%
        paste(collapse = "|")
    )
  )

assert_that(!any(duplicated(wide_temp$voter_id)))

# Merge it with all other variables ============================================
voter_history_wide <- left_join(
  wide_temp,
  voter_history_long %>%
    group_by(voter_id) %>%
    filter(election_date == max(election_date)) %>%
    select(
      -election, -vote_method, -history_file, -election_date,
      -election_type, -election_name, -voting_method
    ) %>%
    dedup()
)
assert_that(!any(duplicated(voter_history_wide$voter_id)))

if (nrows == 100) {
  save(
    voter_history_wide,
    file = here("data", "tidy", "sample", "voter_history_wide_sample.RData")
  )
} else {
  save(
    voter_history_wide,
    file = here("data", "tidy", "voter_history_wide_full.RData")
  )
}
