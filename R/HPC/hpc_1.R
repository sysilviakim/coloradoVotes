zz <- file("error.Rout", open = "wt")
sink(zz, type = "message")

library(tidyverse, lib.loc = "/home/ab6668a/R/x86_64-pc-linux-gnu-library/4.0/")
library(fst, lib.loc = "/home/ab6668a/R/x86_64-pc-linux-gnu-library/4.0/")
library(stringr, lib.loc = "/home/ab6668a/R/x86_64-pc-linux-gnu-library/4.0/")
library(here, lib.loc = "/home/ab6668a/R/x86_64-pc-linux-gnu-library/4.0/")
library(lubridate, lib.loc = "/home/ab6668a/R/x86_64-pc-linux-gnu-library/4.0/")
load(here("/home/ab6668a/df_joined_tidy.RData"))
out <- read_fst(here("/home/ab6668a/full_history_long.fst"))

# Cleaning/standardizing raw import ============================================
out$group <- 1:nrow(out) %% 2 + 1

out <- split(out, out$group)
  
out <- out[[1]]

out <- out %>%
  mutate(election_date = mdy(election_date)) %>%
  mutate(election_year = year(election_date)) %>%
  mutate(voter_id = as.character(voter_id)) %>%
  filter(election_year >= 2016)

# out_sample <- out_sample %>%
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
#   )) %>%
#   filter(election_year > 2012)

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
    voting_method = vote_method,
    county_name = county
  ) 
# However, the voter information in this file only goes as far as identifying
# the voter ID, and county of the person. Adding in address, zip code, gender,
# and yob.
intersect(colnames(df_cleaned), colnames(out))

# Adjust accordingly
voter_info <- df_cleaned %>%
  select(-c(party, election_type, election_date, ballot_style, county_name,
            election_name, voting_method))

# Join
voter_history_long <- inner_join(voter_info, out, by = "voter_id")


write_fst(voter_history_long, "voter_history_long_v1.fst")

closeAllConnections()