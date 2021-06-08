source(here::here("R", "utilities.R"))

# Import Data -------------------------------------------------------------
# The cleaned data doesn't have the specific dates the people cast their votes, 
# going back to the raw joined data for that information:
load("data/tidy/sample/joined_sample_10k.RData")

# Also loading clean data: 
vote_wide <- load(here("data", "tidy", "voter_history_wide_sample_final.RData"))

# Set up data for plot ----------------------------------------------------
# Start with finding and extracting relevant columns: 
df %>% 
  select(vote_method, contains("date")) %>% 
  names()

# Create a column for election-day voting since we don't have that column in
# the cleaned up data: 
plot_data <- df %>%
  mutate(in_person_date = mdy(coalesce(in_person_vote_date_rtn, in_person_vote_date)),
         mail_in_date = mdy(coalesce(mail_ballot_sent_date_rtn, mail_ballot_sent_date))) %>%
  mutate(election_day_vote = case_when(
    in_person_date == mdy("11/03/2020") |
      mail_in_date == mdy("11/03/2020") ~ "election day",
    TRUE ~ "other"
  )) %>%
  select(county, election_day_vote) %>%
  group_by(county) %>%
  table() %>%
  as.tibble() %>%
  filter(election_day_vote %in% "election day") %>%
  rename(election_day = n) %>%
  select(-election_day_vote)

# Create set of values for in-person, and mail-in votes:
plot_data_2 <- vote_wide %>%
  select(county, gen2020) %>%
  group_by(county) %>%
  table() %>%
  as.data.frame() %>%
  pivot_wider(values_from = Freq, names_from = gen2020) %>%
  rename("mail_in" = `0`,
         "in_person" = `1`)

# County wins:
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

  
# Join all three: 
plot_data_full <- inner_join(plot_data, plot_data_2, by = "county") %>%
  inner_join(., co_pres_1, by = "county")

# Plot the data: 
# Middle segment code from http://www.ggtern.com/2014/01/09/segments-user-request/: 
lines <- data.frame(x = c(0.5, 0, 0.5), 
                    y = c(0.5, 0.5, 0), 
                    z = c(0, 0.5, 0.5), 
                    xend = c(1, 1, 1)/3, 
                    yend = c(1, 1, 1)/3, 
                    zend = c(1, 1, 1)/3)

plot <- ggtern(plot_data_full, aes(election_day, mail_in, in_person)) +
  geom_point(aes(color = result)) +
  geom_segment(data = lines, 
               aes(x, y, z, 
                   xend = xend, yend = yend, zend = zend), 
               color = 'black', size = 0.5) +
  labs(color = "Outcome") +
  xlab("Election Day") +
  ylab("Mail-In") +
  zlab("In-Person") +
  theme_bw() +
  theme_arrownormal() +
  scale_color_manual(values = c("Biden" = "blue", "Trump" = "red"))

plot

