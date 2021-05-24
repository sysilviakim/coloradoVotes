source(here::here("R", "utilities.R"))

# Import history wide data =====================================================
if (nrows == 100) {
  load(here("data", "tidy", "sample", "voter_history_wide_sample.RData"))
} else {
  load(here("data", "tidy", "voter_history_wide_full.RData"))
}

# Import and merge General 2020 Voter Profiles =================================
files <- list.files(path = here("data", "raw", "EX-003 Master Voter List",
                                "2020_General"), pattern = "*.txt",
                    full.names = TRUE)

# Using the same proccess used for vf: 
out <- vector("list", length(files))

# Loop to merge
for (i in 1:length(files)) {
  out[[i]] <- files[[i]] %>%
    set_names(.) %>%
    map_dfr(
      ~ read.table(.x, sep = ",", header = TRUE),
      .id = "gen2020_file"
    ) %>%
    clean_names()
}

# Bind 
voter_profile <- list.rbind(out)

# Save 
save(voter_profile, file = here("data", "tidy", "voter_profile_2020.Rda"))

# Set up =======================================================================
# Pull relevant variables, and format: 
voter_join <- voter_profile %>%
  mutate(voter_id = as.character(voter_id)) %>%
  select(voter_id, gender, birth_year, county, party, congressional) %>%
  mutate(across(everything(), tolower))

# Join with wide data: 
wide_profile <- right_join(voter_join, voter_history_wide, by = "voter_id")

# Cleaning =====================================================================
# The prioritize 2020 general columns: 
wide_profile_temp <- wide_profile %>%
  mutate(party = case_when(
    !is.na(party.x) ~ party.x,
    is.na(party.x) ~ party.y
  ),
  county = case_when(
    !is.na(county) ~ county,
    is.na(county) ~ county_name
  ),
  gender = case_when(
    !is.na(gender.x) ~ gender.x,
    is.na(gender.x) ~ gender.y
  )) %>%
  select(-party.x, -party.y, -county_name, -gender.x, -gender.y) %>%
  select(voter_id, first_name, middle_name, last_name, birth_year, gender, 
         party, residential_zip, county, congressional, everything())

# save
if (nrows == 100) {
  save(
    wide_profile_temp, file = here("data", "tidy", 
                            "voter_history_wide_sample_final.RData"))
} else {
  save(wide_profile_temp, file = here("data", "tidy", 
                              "voter_history_wide_full_final.RData"))
}


