source("R/00_utilities.R")

load("data/tidy/master_vr.RData")
load("data/tidy/elect_list.RData")

# Crosswalk between files ======================================================
names(elect_list$ballots)
names(elect_list$returned)
names(elect_list$cured)
names(elect_list$undelivered)
names(master_vr)

# Although there are common variables, safest bet is the ID ====================
common_vars <- elect_list %>% map(names) %>% Reduce(intersect, .)
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
      nrow
  ) %>%
  unlist()
# ballots    returned       cured undelivered 
# 19909       19793         364         125

# Form a master file ===========================================================
df <- master_vr %>%
  mutate_all(trimws) %>%
  full_join(., elect_list$ballots, suffix = c("", "_blt"), by = "voter_id") %>%
  full_join(., elect_list$returned, suffix = c("", "_rtn"), by = "voter_id") %>%
  full_join(., elect_list$cured, suffix = c("", "_cur"), by = "voter_id") %>%
  full_join(., elect_list$undelivered, suffix = c("", "_und"), by = "voter_id")

if (nrows == -1) {
  save(df, file = "data/tidy/joined_full.RData")
} else {
  df <- df %>% sample_n(10000)
  save(df, file = "data/tidy/joined_sample_10k.RData")
}

# Aggregate columns: for now, simply delete what's not in master VR
temp <- df %>% 
  select(-matches("_blt$|_rtn$|_cur$|_und$")) %>%
  mutate(
    party2 = case_when(
      party == "dem" ~ "dem", 
      party == "rep" ~ "rep"
    ),
    party4 = case_when(
      party == "dem" ~ "dem", 
      party == "rep" ~ "rep",
      party == "uaf" ~ "unaffiliated",
      TRUE ~ "third-party"
    )
  )
# df %>%
#   rowwise() %>%
#   mutate(
#     first_name = nchar_longest(
#       c(first_name, first_name_blt, first_name_cur, first_name_rtn, first_name_und)
#     ),
#     last_name = nchar_longest(
#       c(last_name, last_name_blt, last_name_cur, last_name_rtn, last_name_und)
#     ),
#     middle_name = nchar_longest(
#       c(middle_name, middle_name_blt, middle_name_cur, middle_name_rtn, middle_name_und)
#     )
#   )

# Voting history for 2020 ======================================================
round(prop.table(table(temp$vote_method, temp$party2)) * 100, digits = 1)
round(prop.table(table(temp$vote_method, temp$party4)) * 100, digits = 1)



