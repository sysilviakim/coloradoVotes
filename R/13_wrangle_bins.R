source(here::here("R", "utilities.R"))
load("data/tidy/sample/merged_sample.RData")

# Wrangle ----------------------------------------------------------------------
# Criteria for binning registration dates: 
# - Election Day Registration: If registration day coincides with 
# gen2014/16/18/20, pri2014/16/18/20
# - Within 15 days of the election date
# - Within 30 Days of the election date 
# - Anything more than 30, coded as other 
# Ideally when the columns are coalesced there is only one non-NA value. But, 
# on the off chance they re registered, I ordered them chronologically to 
# pull their latest registration time.  
# Also binned age--don't have any specific logic behind them, went off age groups
# that generally seem to be used. 
df <- df %>%
  mutate(registration_date = mdy(registration_date)) %>%
  mutate(party = case_when(
    party %in% "dem" | party %in% "democratic" ~ "dem",
    party %in% "rep" | party %in% "republican" ~ "rep",
    TRUE ~ "oth"
  )) %>%
  mutate(age = 2021 - as.integer(yob)) %>%
  mutate(age_grp = case_when(
    age >= 18 & age <= 29 ~ "18 - 29",
    age >= 30 & age <= 44 ~ "30-44",
    age >= 45 & age <= 59 ~ "45-59",
    age >= 60 ~ "60+"
  )) %>%
  mutate(reg_gen2020 = as.numeric(mdy("11/03/2020") - registration_date),
         reg_gen2016 = as.numeric(mdy("11/08/2016") - registration_date),
         reg_gen2018 = as.numeric(mdy("11/06/2018") - registration_date),
         reg_gen2014 = as.numeric(mdy("11/04/2014") - registration_date),
         reg_pri2020 = as.numeric(mdy("06/30/2020") - registration_date),
         reg_pri2016 = as.numeric(mdy("06/28/2016") - registration_date),
         reg_pri2018 = as.numeric(mdy("06/26/2018") - registration_date),
         reg_pri2014 = as.numeric(mdy("06/24/2014") - registration_date)) %>%
  mutate(across(c(reg_gen2020, reg_gen2018, reg_gen2016, reg_gen2014,
                reg_pri2020, reg_pri2018, reg_pri2016, reg_pri2014),
         ~ case_when(
           . == 0 ~ "EDR",
           . > 15 & . <= 30 ~ "30Days",
           . > 0 & . <= 15 ~ "15Days"
         ))) %>%
  mutate(reg_bin = coalesce(reg_gen2020, reg_pri2020, reg_gen2018, reg_pri2018, 
                            reg_gen2016, reg_pri2016, reg_gen2014, reg_pri2014),
         reg_bin = replace_na(reg_bin, "30+"))

# Imputing political leanings --------------------------------------------------  
primary_history <- read.table(
  here(
    "data/raw/EX-002 Voting History Files", "20180719",
    "EX-002_2018_Primary_Supplemental_Vote_History",
    "EX-002_2018_Primary_Supplemental_Vote_History.txt"
  ),
  header = TRUE,
  sep = "|"
) %>%
  clean_names() %>%
  select(voted_party, received_party_ballot, voter_id) %>%
  mutate(voter_id = as.character(voter_id),
         across(everything(), tolower)) %>%
  mutate(voted_party = na_if(voted_party, ""),
         received_party_ballot = na_if(received_party_ballot, ""))

df_temp <- left_join(df, primary_history, by = "voter_id")

# Create a column with leanings: 
# Leanings are imputed from: 
# - Party voted for (priority)
# - Party of the ballot they recieved (if party voted for is not available)
df_test <- df_temp %>%
  mutate(party_test = case_when(
    party %in% "oth" & !is.na(voted_party) ~ voted_party,
    party %in% "oth" & is.na(voted_party) & !is.na(received_party_ballot) ~ 
      received_party_ballot,
    TRUE ~ party
  )) 

ggplot(df_test, aes(party)) +
  geom_bar()

# Comparing now: 
# Prop table without imputing: 
table(df$party)/nrow(df) 
# dem       oth       rep 
# 0.3632265 0.2915832 0.3451904

# Prop table after imputing: 
table(df_test$party_test)/nrow(df_test)
# dem       oth       rep 
# 0.4218437 0.1913828 0.3867735 
# Seems to work! 

# Add challenged/rejected column -----------------------------------------------
# Based on the name, and the lack of a column that tells if they were cured or 
# not, I am assuming this data just refers to those ballots that were rejected
# and NOT cured; since the only information regarding rejections provided is a 
# rejection reason
rejected_files <- list.files(path =  here(
  "data/raw/CE-077_Rejected_Cure/Archive"
  ), 
  pattern = "*.txt",
  full.names = TRUE)

out <- vector("list", length(rejected_files))

# Import, and clean at the same time 
for (i in 2:length(rejected_files)) {
  out[[i]] <- rejected_files[[i]] %>%
    map_dfr(
      ~ read.table(.x, sep = "|", header = TRUE, quote = "", fill = TRUE)
    ) %>%
    clean_names() %>%
    select(voter_id, reject_reason) %>%
    # Adding a collumn of 1s to later add 0s for those voters who weren't reject. 
    mutate(rejected = rep(1, n()),
           reject_reason = tolower(reject_reason))
}

# Collapse list into one df
rejected_voters <- list.rbind(out)

# Check duplicate values
table(duplicated(rejected_voters$voter_id))

# Join again with this new information: 
df_joined <- left_join(df_temp, rejected_ballots, by = "voter_id") %>%
  # Replace NA with 0 (not challenged)
  mutate(rejected = replace_na(rejected, "0"))

# The sample is too small to catch any of these new voters, so checking on
# the full file: 
load("data/tidy/merged_full.RData")
# Checking: 
nrow(inner_join(
  select(df, voter_id),
  select(rejected_voters, voter_id),
  by = "voter_id"
))

# [1] 55693 
# Seems to work. 








  