source(here::here("R", "utilities.R"))
load(here("data/tidy/df_joined_tidy.RData"))
if (nrows == 100) {
  load(here("data/tidy/sample/voter_history_long_sample.RData"))
  out <- voter_history_long
} else {
  out <- read_fst(here("data", "tidy", "full_history_long.fst"))
}

# Cleaning/standardizing raw import ============================================
out <- out %>%
  mutate(election_date = mdy(election_date)) %>%
  mutate(election_year = year(election_date)) %>%
  mutate(voter_id = as.character(voter_id))

out <- out %>%
  mutate(election_date = case_when(
    str_detect(history_file, "^Master.*_[0-9]{2}.txt$") & 
      election_date == "1900-01-01" ~ 
      mdy(str_extract(history_file, "[0-9]{2}_[0-9]{2}_[0-9]{4}")),
    str_detect(history_file, "^Master.*[0-9]{4}.txt$") & 
      election_date == "1900-01-01" ~ 
      mdy(str_extract(history_file, "[0-9]{8}")),   
    str_detect(history_file, "^EX.*") & 
      election_date == "1900-01-01" ~ 
      as.Date(str_extract(history_file, "[0-9]{4}"), format = "%Y"), 
    TRUE ~ election_date
  ))

out <- out %>%
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
  # Fixing the history file column
  mutate(history_file = word(history_file, -1, sep = fixed("/"))) 

# Selecting relevant variables from df_cleaned:
df_join_long <- df_cleaned %>%
  rename(
    history_file = file,
    voting_method = vote_method,
    county_name = county
  ) %>%
  select(
    history_file, voter_id, election_type, election_date, voting_method,
    party, county_name, election_name
  )

# However, the voter information in this file only goes as far as identifying
# the voter ID, and county of the person. Adding in address, zip code, gender,
# and yob.

voter_info <- df_cleaned %>%
  select(voter_id, first_name, middle_name, last_name, gender, residential_zip)

voter_history_long <- inner_join(voter_info, out)

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
