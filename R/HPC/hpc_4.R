zz <- file("error_1.Rout", open = "wt")
sink(zz, type = "message")

library(tidyverse, lib.loc = "/home/ab6668a/R/x86_64-pc-linux-gnu-library/4.0/")
library(fst, lib.loc = "/home/ab6668a/R/x86_64-pc-linux-gnu-library/4.0/")
library(here, lib.loc = "/home/ab6668a/R/x86_64-pc-linux-gnu-library/4.0/")
library(lubridate, lib.loc = "/home/ab6668a/R/x86_64-pc-linux-gnu-library/4.0/")
library(scrubr, lib.loc = "/home/ab6668a/R/x86_64-pc-linux-gnu-library/4.0/")
voter_history_long <- read_fst(here("/home/ab6668a/voter_history_long_full_hpc.fst"))

# Recoding party/gender ========================================================
voter_history_long %>%
  select(gender, county_name, election_type, party, voting_method) %>%
  map(., unique)

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
    ),
    election_type = recode(
      election_type,
      general = "gen",
      primary = "pri",
      .default = "oth"
    )
  ) %>%
  filter(!is.na(election_type) & election_type != "oth")

# Recoding voting methods ======================================================
# prop(voter_history_long, "voting_method", sort = TRUE)

## Direct Recording Electronic (DRE)
mail <- c("mail ballot", "absentee mail", "mail ballot - dre", "absentee carry")
in_person <- c(
  "polling place", "in person", "early voting - dre", "early voting",
  "in person - dre", "vote center", "vote center - dre"
)

voter_history_long <- voter_history_long %>%
  mutate(
    vote_method = case_when(
      voting_method %in% mail ~ 0,
      voting_method %in% in_person ~ 1,
      TRUE ~ NA_real_
    )
  )

# Column to aggregate election by year =========================================
# (removing county-level dependency for the wide pivot)
voter_history_long <- voter_history_long %>%
  mutate(election = str_c(election_type, year(election_date)))

write_fst(voter_history_long, "voter_history_long_clean.fst")

closeAllConnections()
