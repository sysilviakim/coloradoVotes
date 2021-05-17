source(here::here("R", "utilities.R"))
load(here("data/tidy/voter_history_long_sample.RData"))

voter_history_long %>%
  select(gender, county_name, election_type, party, voting_method) %>%
  map(., unique)

# Everything seems fine except the party variable: 
voter_history_long <- voter_history_long %>%
  mutate(party = na_if(party, "no data"),
         party = na_if(party, " "),
         party = recode(party, democratic = "dem", 
                        republican = "rep",
                        unaffiliated = "uaf", 
                        dem = "dem", 
                        rep = "rep",
                        uaf = "uaf",
                        .default = "third party"), 
         gender = recode(gender, male = "male",
                         female = "female",
                         .default = "unknown")) %>% 
  mutate(voting_method = na_if(voting_method, "")) 

# Next, grouping voting methods: 
mail <- c("mail ballot", "absentee mail", "early voting")
in_person <- c("polling place", "in person", "early voting - dre", 
               "in person - dre", "vote center", "vote center - dre")
unsure <- c("absentee carry", "mail ballot - dre")

voter_history_long <- voter_history_long %>%
  mutate(vote_method = case_when(
    (voting_method %in% unsure) ~ 0,
    (voting_method %in% mail) ~ 1,
    (voting_method %in% in_person) ~ 2
  )) 

# Changing into long format: 
voter_history_wide <- voter_history_long %>%
  mutate(row = row_number()) %>%
  pivot_wider(names_from = election_name, values_from = vote_method) %>%
  select(-row)

save(voter_history_wide, file = here("data", "tidy", 
                                     "voter_history_wide_sample.RData"))

