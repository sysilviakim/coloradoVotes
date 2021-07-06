source(here::here("R", "utilities.R"))
if (nrows == 100) {
  load(here("data/tidy/sample/voter_history_long_sample.RData"))
} else {
  load(here("data/tidy/voter_history_long_full.RData"))
}

# Select variables and pivot to wide ===========================================
## To save memory, split into groups and perform one by one in a loop
## 64 uneven sized groups
wide_list <- voter_history_long %>%
  group_by(county_name) %>%
  group_split()

## Cannot directly overwrite
voter_history_wide <- wide_temp <- vector("list", length = length(wide_list))

for (i in seq(length(wide_list))) {
  ## First, relocate variable order after selection ----------------------------
  wide_temp[[i]] <- wide_list[[i]] %>%
    filter(election_type != "oth") %>%
    select(voter_id, election, vote_method) %>%
    dedup() %>%
    group_by(voter_id, election) %>%
    ## If both records exist, collapse as in-person voter
    summarise(vote_method = sum(vote_method, na.rm = TRUE)) %>%
    arrange(voter_id, election, vote_method) %>%
    pivot_wider(names_from = election, values_from = vote_method) %>%
    relocate(
      any_of(c("voter_id", cross2(c("gen", "pri"), seq(2020, 2014, by = -2)) %>%
        map_chr(~ paste0(.x, collapse = ""))))
    )
  assert_that(!any(duplicated(wide_temp[[i]]$voter_id)))
  
  ## Merge it with all other variables -----------------------------------------
  long_dedup <- voter_history_long %>%
    group_by(voter_id) %>%
    filter(election_date == max(election_date)) %>%
    select(
      -election, -vote_method, -history_file, -election_date,
      -election_type, -election_name, -voting_method
    ) %>%
    ## Selects most recent county record
    dedup()
  
  voter_history_wide[[i]] <- left_join(wide_temp[[i]], long_dedup)
  assert_that(!any(duplicated(voter_history_wide$voter_id)))
  
  print(paste0(i, "-th county finished."))
}

# Regroup into single dataframe, resolve duplicates ============================
voter_history_wide <- voter_history_wide %>% bind_rows()

## resolve duplicate IDs by merging records: voters who moved across counties
temp <- voter_history_wide %>%
  group_by(voter_id) %>%
  mutate(across(where(is.numeric), ~ sum(.x, na.rm = TRUE))) %>%
  dedup() %>%
  ## Problem: IDs like 601567863, has general records in both counties
  ## or changed party in the middle
  ## Only 5 of those; choose latter
  slice(n())

assert_that(!any(duplicated(temp$voter_id)))

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
