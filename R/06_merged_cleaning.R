source(here::here("R", "utilities.R"))
load(here("data/tidy/df_joined_tidy.RData"))
load(here("data/tidy/file_list_history.Rda"))

out <- vector("list", length(file_list))

for (i in 1:length(file_list)) {
  out[[i]] <- file_list[[i]] %>%
    set_names(.) %>%
    map_dfr(
      ~ read.table(.x, sep = ",", header = TRUE, nrows = 1000),
      .id = "history_file"
    ) %>%
    clean_names()
}
out <- list.rbind(out)

# Cleaning the out file to standardize a few things: 
out <- out %>%
  mutate(election_date = mdy(election_date)) %>%
  mutate(election_year = year(election_date)) %>%
  mutate(voter_id = as.character(voter_id))

convert <- c("election_type", "voting_method", "party", "county_name")

out <- out %>%
  mutate_at(vars(convert), tolower) %>%
  mutate(
    election_year = as.character(election_year),
    election_name = str_c(election_year, county_name, "county", election_type,
                          "election",
                          sep = " "
    )
  ) %>%
  select(-c(election_year, election_description)) %>%
  mutate(history_file = word(history_file, -1, sep = fixed("/"))) # Fixing the 
# history file column. 

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

save(voter_history_long, file = here("data", "tidy", 
                                     "voter_history_long_sample.RData"))

