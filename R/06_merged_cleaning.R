source(here::here("R", "utilities.R"))
load(here("data/tidy/df_joined_tidy.RData"))
if (nrows == 100) {
  load(here("data/tidy/full_history_long_sample.RData"))
  out <- voter_history_long
} else {
  out <- read_fst(here("data", "tidy", "full_history_long.fst"))
}

# Cleaning/standardizing raw import ============================================
out <- out %>%
  mutate(election_date = mdy(election_date)) %>%
  mutate(election_year = year(election_date)) %>%
  mutate(voter_id = as.character(voter_id)) %>%
  filter(election_year >+ 2016)

# out <- out %>%
#   mutate(election_date = case_when(
#     str_detect(out$history_file, "^Master.*_[0-9]{2}.txt$") &
#       election_date == "1900-01-01" ~
#       mdy(str_extract(history_file, "[0-9]{2}_[0-9]{2}_[0-9]{4}")),
#     str_detect(history_file, "^Master.*[0-9]{4}.txt$") &
#       election_date == "1900-01-01" ~
#       mdy(str_extract(history_file, "[0-9]{8}")),
#     str_detect(history_file, "^EX.*") &
#       election_date == "1900-01-01" ~
#       as.Date(str_extract(history_file, "[0-9]{4}"), format = "%Y"),
#     TRUE ~ election_date
#   )) 

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
df_cleaned <- df_cleaned %>%
  rename(
    history_file = file,
    voting_method = vote_method,
    county_name = county
  ) 

# CHeck common variables: 
intersect(colnames(df_cleaned), colnames(out))

# Adjust accordingly 
voter_info <- df_cleaned %>%
  select(-c(party, election_type, election_date, ballot_style))

voter_history_long <- inner_join(df_cleaned, out)

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
