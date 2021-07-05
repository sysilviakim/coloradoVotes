zz <- file("error_1.Rout", open = "wt")
sink(zz, type = "message")

library(tidyverse, lib.loc = "/home/ab6668a/R/x86_64-pc-linux-gnu-library/4.0/")
library(fst, lib.loc = "/home/ab6668a/R/x86_64-pc-linux-gnu-library/4.0/")
library(here, lib.loc = "/home/ab6668a/R/x86_64-pc-linux-gnu-library/4.0/")
library(lubridate, lib.loc = "/home/ab6668a/R/x86_64-pc-linux-gnu-library/4.0/")
library(scrubr, lib.loc = "/home/ab6668a/R/x86_64-pc-linux-gnu-library/4.0/")
voter_history_long <- read_fst(here("/home/ab6668a/voter_history_long_full_hpc.fst"))

# Split =======================================================================
voter_history_long$group <- 1:nrow(voter_history_long) %% 10 + 1

voter_history_long <- split(voter_history_long, voter_history_long$group)

voter_history_long <- voter_history_long[[1]]
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
# Select variables and pivot to wide =========================================== 
wide_temp <- voter_history_long %>%
    select(voter_id, election, vote_method) %>%
    dedup() %>%
    group_by(voter_id, election) %>%
    ## If both records exist, collapse as in-person voter
    summarise(vote_method = sum(vote_method, na.rm = TRUE)) %>%
    arrange(voter_id, election, vote_method) %>%
    pivot_wider(names_from = election, values_from = vote_method) %>%
    select(
      voter_id, 
      cross2(c("gen", "pri"), seq(2020, 2016, by = -2)) %>%
        map_chr(~ paste0(.x, collapse = ""))
    )
  
  # assert_that(!any(duplicated(wide_temp$voter_id)))
  
# Merge it with all other variables ============================================
voter_history_long <- voter_history_long %>%
  group_by(voter_id) %>%
  filter(election_date == max(election_date)) %>%
  select(
    -election, -vote_method, -history_file, -election_date,
    -election_type, -election_name, -voting_method
  ) %>%
  dedup()  

voter_history_wide <- left_join(
    wide_temp,
    voter_history_long,
    by = "voter_id"
  )
  # assert_that(!any(duplicated(voter_history_wide$voter_id)))

write_fst(voter_history_wide, "voter_history_wide_hpc.fst")

closeAllConnections()
