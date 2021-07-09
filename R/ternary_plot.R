source(here::here("R", "utilities.R"))

# Import Data -------------------------------------------------------------

load(here("data", "tidy", "voter_history_wide_sample_final.RData"))

vote_wide <- df
# Set up for plot ---------------------------------------------------------

# Create data with columns for: 1) Gen 2020 voters, 2) Gen 2016 voters, 
# and 3) Gen 2020 Results. All grouped by county. 

# Start by recoding--for visualization purposes: 
vote_wide <- vote_wide %>%
  mutate(gen2020 = case_when(
    is.na(gen2020) ~ "no_turnout",
    gen2020 == 1 ~ "mail_in",
    gen2020 == 2 ~ "in_person",
    gen2020 == 0 ~ "not_voted"
  ),
  gen2016 = case_when(
    is.na(gen2016) ~ "no_turnout",
    gen2016 == 1 ~ "mail_in",
    gen2016 == 2 ~ "in_person",
    gen2016 == 0 ~ "not_voted"
  ))

# Collapse by grouping by county, with counts recorded:
vote_2016 <- vote_wide %>%
  group_by(county) %>% 
  count(gen2016) %>%
  pivot_wider(names_from = gen2016, values_from = n) %>%
  mutate(year = 2016) %>%
  select(-`NA`)

vote_2020 <- vote_wide %>%
  group_by(county) %>% 
  count(gen2020) %>%
  pivot_wider(names_from = gen2020, values_from = n) %>%
  mutate(year = 2020)

# vote_2016 <- rbind(vote_2016, vote_2020)
# rm(vote_2020)

# Now pull gen2020 election results: 
co_pres <- read_csv(paste0("https://raw.githubusercontent.com/",
                           "tonmcg/US_County_Level_Election_Results_08-20/",
                           "master/2020_US_County_Level_Presidential_Results.csv"))

co_pres_2020 <- co_pres %>%
  filter(state_name %in% "Colorado") %>%
  mutate(county = tolower(county_name)) %>%
  mutate(result = case_when(
    per_gop > per_dem ~ "Republican",
    per_dem > per_gop ~ "Democrat"
  )) %>%
  mutate(county = gsub("\\s*\\w*$", "", county)) %>%
  select(county, result)

co_pres_2016 <- read_csv(paste0("https://raw.githubusercontent.com/mkearney/",
                                "presidential_election_county_results_2016/",
                                "master/data/pres.elect16.results.2018.csv")) %>%
  filter(st %in% "CO") %>%
  filter(!is.na(county)) %>%
  select(county, lead) %>%
  dedup() %>%
  mutate(result = case_when(
    lead %in% "Hillary Clinton" ~ "Democrat",
    lead %in% "Donald Trump" ~ "Republican"
  )) %>%
  mutate(county = tolower(county)) %>%
  mutate(county = gsub("\\s*\\w*$", "", county)) %>%
  select(-lead)

# Join to create a full tibble: 
join_2020 <- inner_join(co_pres_2020, vote_2020, by = "county") 
join_2016 <- inner_join(co_pres_2016, vote_2016, by = "county")
t_plot <- rbind(t_plot_1, t_plot_2) %>%
  mutate(out = paste0(result, year))

# Making the plot! 
ggtern(t_plot, aes(not_voted, mail_in, in_person)) +
  geom_point(aes(color = result, shape = as.factor(year))) +
  labs(color = "Outcome", shape = "Year") +
  xlab("No Turnout") +
  ylab("Mail-In") +
  zlab("In-Person") +
  theme_bw() +
  theme_arrownormal() +
  geom_line(aes(group = county), size = 0.2, alpha = 0.1) 


