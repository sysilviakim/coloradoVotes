source(here::here("R", "utilities.R"))

master_vr <- read_fst(here("data/tidy/master_vr.fst"))
load(here("data/tidy/elect_list.RData"))

# Crosswalk between files ======================================================
names(elect_list$ballots)
names(elect_list$returned)
names(elect_list$cured)
names(elect_list$undelivered)
names(master_vr)

# Although there are common variables, safest bet is the ID ====================
common_vars <- elect_list %>%
  map(names) %>%
  Reduce(intersect, .)
# [1] "voter_id" "last_name"   "first_name"  "middle_name" "name_suffix" "party"

nrow(inner_join(master_vr, elect_list$returned, by = "voter_id"))
# 3286791
nrow(inner_join(master_vr, elect_list$returned, by = common_vars))
# 3280590
nrow(inner_join(master_vr, elect_list$returned))
# 3265620

# In that case, any voter not in master VR file? Yes... use full_join ==========
elect_list %>%
  map(
    ~ anti_join(.x, master_vr, by = "voter_id") %>%
      nrow()
  ) %>%
  unlist()
# ballots    returned       cured undelivered
# 19909       19793         364         125

# Form a master file ===========================================================
df <- master_vr %>%
  mutate_all(trimws) %>%
  # Clean out redundant files at a later file
  full_join(., elect_list$ballots, suffix = c("", "_blt"), by = "voter_id") %>%
  full_join(., elect_list$returned, suffix = c("", "_rtn"), by = "voter_id") %>%
  full_join(., elect_list$cured, suffix = c("", "_cur"), by = "voter_id") %>%
  full_join(., elect_list$undelivered, suffix = c("", "_und"), by = "voter_id")

# Voting history for 2020 ======================================================
# Aggregate columns: for now, simply delete what's not in master VR
temp <- df %>%
  select(party, vote_method) %>%
  mutate(
    party2 = case_when(
      party == "dem" ~ "dem",
      party == "rep" ~ "rep"
    ),
    party4 = case_when(
      party == "dem" ~ "dem",
      party == "rep" ~ "rep",
      party == "uaf" ~ "unaffiliated",
      is.na(party) ~ NA_character_,
      TRUE ~ "third-party"
    )
  )

# Crosstabs
prop(temp, c("vote_method", "party2"))
prop(temp, c("vote_method", "party4"))

# Save data ====================================================================
if (nrows == -1) {
  # save(df, file = here("data/tidy/joined_full.RData"))
  write_fst(df, here("data/tidy/joined_full.fst"))
} else {
  set.seed(123)
  df <- df %>% sample_n(10000)
  save(df, file = here("data/tidy/sample/joined_sample_10k.RData"))
}
