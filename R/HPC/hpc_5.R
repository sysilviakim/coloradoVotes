error <- file("error.Rout", open = "wt")
sink(error, type = "message")

library(tidyverse, lib.loc = "/home/ab6668a/R/x86_64-pc-linux-gnu-library/4.0/")
library(fst, lib.loc = "/home/ab6668a/R/x86_64-pc-linux-gnu-library/4.0/")
library(here, lib.loc = "/home/ab6668a/R/x86_64-pc-linux-gnu-library/4.0/")
library(lubridate, lib.loc = "/home/ab6668a/R/x86_64-pc-linux-gnu-library/4.0/")
library(scrubr, lib.loc = "/home/ab6668a/R/x86_64-pc-linux-gnu-library/4.0/")
library(rlist, lib.loc = "/home/ab6668a/R/x86_64-pc-linux-gnu-library/4.0/")
voter_history_long <- read_fst(here("/home/ab6668a/voter_history_long_clean.fst"))

# Split =======================================================================
voter_history_long$group <- 1:nrow(voter_history_long) %% 10 + 1

voter_history_long <- split(voter_history_long, voter_history_long$group)

wide_temp <- vector("list", length(10))

voter_history_wide <- vector("list", length(10))
# Select variables and pivot to wide =========================================== 
for (i in 1:10) {
wide_temp[[i]] <- voter_history_long[[i]] %>%
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
voter_history_wide[[i]] <- left_join(
  wide_temp[[i]],
  voter_history_long[[i]] %>%
    group_by(voter_id) %>%
    filter(election_date == max(election_date)) %>%
    select(
      -election, -vote_method, -history_file, -election_date,
      -election_type, -election_name, -voting_method
    ) %>%
    dedup()
)
# assert_that(!any(duplicated(voter_history_wide$voter_id)))
}

voter_history_wide <- rbind(voter_history_wide)

write_fst(voter_history_wide, "voter_history_wide_hpc.fst")

closeAllConnections()
