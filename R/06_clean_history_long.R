source(here::here("R", "utilities.R"))

if (nrows == 100) {
  out <-
    loadRData(here("data", "tidy", "sample", "full_history_long_sample.RData"))
} else {
  out <- read_fst(here("data", "tidy", "full_history_long.fst"))
}

# Cleaning/standardizing raw import ============================================
out <- out %>%
  mutate(election_date = mdy(election_date)) %>%
  mutate(election_year = year(election_date)) %>%
  mutate(voter_id = as.character(voter_id)) %>%
  filter(election_year >+ 2014) %>%
  filter(election_year %% 2 == 0)

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

# Recoding voting methods ======================================================
prop(voter_history_long, "voting_method", sort = TRUE)

## Direct Recording Electronic (DRE)
mail <- c("mail ballot", "absentee mail", "mail ballot - dre", "absentee carry")
in_person <- c(
  "polling place", "in person", "early voting - dre", "early voting",
  "in person - dre", "vote center", "vote center - dre"
)

voter_history_long <- voter_history_long %>%
  mutate(
    election_type = recode(
      election_type,
      general = "gen",
      primary = "pri",
      .default = "oth"
    ),
    vote_method = case_when(
      ## eventually, 0 = did not vote, 1 = mail, 2 = vote in person
      voting_method %in% mail ~ 1,
      voting_method %in% in_person ~ 2,
      TRUE ~ NA_real_
    )
  )

# Column to aggregate election by year =========================================
# (removing county-level dependency for the wide pivot)
voter_history_long <- voter_history_long %>%
  mutate(election = str_c(election_type, year(election_date)))

# Final output
head(voter_history_long) %>% select(-history_file)

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
