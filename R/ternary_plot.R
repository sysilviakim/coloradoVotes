source(here::here("R", "utilities.R"))

# Import Data -------------------------------------------------------------

load(here("data", "tidy", "voter_history_wide_sample_final.RData"))

vote_wide <- wide_profile_temp
# Set up for plot ---------------------------------------------------------

# Create data with columns for: 1) Gen 2020 voters, 2) Gen 2016 voters, 
# and 3) Gen 2020 Results. All grouped by county. 

# Start by recoding--for visualization purposes: 
vote_wide <- vote_wide %>%
  mutate(gen2020 = case_when(
    is.na(gen2020) ~ "no_turnout",
    gen2020 == 1 ~ "mail_in",
    gen2020 == 0 ~ "in_person"
  ),
  gen2016 = case_when(
    is.na(gen2016) ~ "no_turnout",
    gen2016 == 1 ~ "mail_in",
    gen2016 == 0 ~ "in_person"
  ))

# Collapse by grouping by county, with counts recorded:
vote_wide_plot <- vote_wide %>%
  group_by(county) %>% 
  count(gen2016) %>%
  pivot_wider(names_from = gen2016, values_from = n) %>%
  mutate(in_person = replace_na(in_person, 0),
         mail_in = replace_na(mail_in, 0),
         no_turnout = replace_na(no_turnout, 0)) %>% 
  mutate(year = 2016)

vote_temp <- vote_wide %>%
  group_by(county) %>% 
  count(gen2020) %>%
  pivot_wider(names_from = gen2020, values_from = n) %>%
  mutate(in_person = replace_na(in_person, 0),
         mail_in = replace_na(mail_in, 0),
         no_turnout = replace_na(no_turnout, 0)) %>% 
  mutate(year = 2020)

vote_wide_plot <- rbind(vote_wide_plot, vote_temp)
rm(vote_temp)

# Now pull gen2020 election results: 
co_pres <- read_csv(paste0("https://raw.githubusercontent.com/",
                           "tonmcg/US_County_Level_Election_Results_08-20/",
                           "master/2020_US_County_Level_Presidential_Results.csv"))

co_pres_1 <- co_pres %>%
  filter(state_name %in% "Colorado") %>%
  mutate(county = tolower(county_name)) %>%
  mutate(result = case_when(
    per_gop > per_dem ~ "Trump",
    per_dem > per_gop ~ "Biden"
  )) %>%
  mutate(county = gsub("\\s*\\w*$", "", county)) %>%
  select(county, result)

# Join to create a full tibble: 
t_plot <- inner_join(co_pres_1, vote_wide_plot, by = "county")


# Making the plot! 
ggtern(t_plot, aes(no_turnout, mail_in, in_person)) +
  geom_point(aes(color = result)) +
  labs(color = "Outcome") +
  xlab("No Turnout") +
  ylab("Mail-In") +
  zlab("In-Person") +
  theme_bw() +
  theme_arrownormal() +
  scale_color_manual(values = c("Biden" = "blue", "Trump" = "red")) +
  geom_line(aes(group = county), size = 0.2)


