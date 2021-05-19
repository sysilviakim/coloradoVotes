source(here::here("R", "utilities.R"))
if (nrows == 100) {
  load(here("data/tidy/sample/voter_history_long_sample.RData"))
} else {
  load(here("data/tidy/voter_history_long_full.RData"))
}

voter_history_long %>%
  select(gender, county_name, election_type, party, voting_method) %>%
  map(., unique)

# Recoding party/gender ========================================================
voter_history_long <- voter_history_long %>%
  mutate(
    party = na_if(party, "no data"),
    party = na_if(party, " "),
    party = recode(
      party,
      democratic = "dem",
      republican = "rep",
      unaffiliated = "uaf",
      dem = "dem",
      rep = "rep",
      uaf = "uaf",
      .default = "third party"
    ),
    gender = recode(
      gender,
      male = "male",
      female = "female",
      .default = "unknown"
    )
  ) %>%
  mutate(voting_method = na_if(voting_method, ""))

# Recoding voting methods ======================================================
mail <- c("mail ballot", "absentee mail", "early voting")
in_person <- c(
  "polling place", "in person", "early voting - dre",
  "in person - dre", "vote center", "vote center - dre"
)
unsure <- c("absentee carry", "mail ballot - dre")

voter_history_long <- voter_history_long %>%
  mutate(
    vote_method = case_when(
      (voting_method %in% unsure) ~ 0,
      (voting_method %in% mail) ~ 1,
      (voting_method %in% in_person) ~ 2
    )
  )

# Changing from long to wide ===================================================
# Column to ggregate election by year (removing county-level depedency for the 
# wide pivot)
voter_history_long <- voter_history_long %>%
  mutate(election = str_c(election_type, year(election_date)))

# Pivot to wide format
voter_history_wide <- voter_history_long %>%
  mutate(row = row_number()) %>%
  pivot_wider(names_from = election, values_from = vote_method) %>%
  select(-row)
# The only issue is there are multiple observations per voter (one for each 
# election)

# Collapsing the rows to create one row per unique voter
identifiers <- c("voter_id", "first_name", "middle_name", "last_name", 
                 "gender", "residential_zip")

remove <- c("history_file", "election_type", "election_date", "voting_method", 
            "party", "county_name", "election_name")

# Attempt
voter_history_wide <- voter_history_wide %>%
  select(-all_of(remove)) %>%
  group_by_at(identifiers) %>%
  summarise_all(na.omit)

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
