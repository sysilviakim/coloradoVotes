source(here::here("R", "utilities.R"))
load(here("data/tidy/df_joined_tidy.RData"))

if (nrows == 100) {
  out <- loadRData(here("data/tidy/full_history_long_sample.RData"))
} else {
  out <- read_fst(here("data", "tidy", "full_history_long.fst"))
}

# Cleaning/standardizing raw import ============================================
out <- out %>%
  mutate(election_date = mdy(election_date)) %>%
  mutate(election_year = year(election_date)) %>%
  mutate(voter_id = as.character(voter_id)) %>%
  filter(election_year >+ 2016)

voter_history_long <- out %>%
  mutate(
    across(c("election_type", "voting_method", "party", "county_name"), tolower)
  ) %>%
  mutate(
    election_year = as.character(election_year),
    election_name = str_c(
      election_year, county_name, "county", election_type, "election",
      sep = " "
    )
  ) %>%
  select(-c(election_year, election_description)) %>%
  # Send file info to the back
  select(-history_file, everything())
  # Fixing the history file column
  # `word` approach is good but too memory-exhaustive
  # mutate(history_file = word(history_file, -1, sep = "/"))

if (nrows == 100) {
  save(
    voter_history_long,
    file = here("data", "tidy", "sample", "voter_history_long_sample.RData")
  )
} else {
  save(
    voter_history_long,
    file = here("data", "tidy", "voter_history_long_full.RData")
  )
}
