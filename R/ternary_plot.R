source(here::here("R", "utilities.R"))
library(ggtern)

# Import Data ==================================================================
vote_wide <-
  loadRData(here("data", "tidy", "voter_history_wide_full.RData")) %>%
  rename(county = county_name)

# Set up for plot ==============================================================

# Create data with columns for: 1) Gen 2020 voters, 2) Gen 2016 voters,
# and 3) Gen 2020 Results. All grouped by county.
# https://www.sos.state.co.us/pubs/elections/Results/Abstract/2016/general/turnout.html

# Collapse by grouping by county, with counts recorded:
vote_2016 <- vote_wide %>%
  group_by(county) %>%
  count(gen2016) %>%
  pivot_wider(names_from = gen2016, values_from = n) %>%
  mutate(year = 2016)

vote_2020 <- vote_wide %>%
  group_by(county) %>%
  count(gen2020) %>%
  pivot_wider(names_from = gen2020, values_from = n) %>%
  mutate(year = 2020)

# Start by recoding--for visualization purposes:
vote_wide <- vote_wide %>%
  mutate(
    gen2020 = case_when(
      gen2020 == 1 ~ "Mail",
      gen2020 >= 2 ~ "In_person",
      gen2020 == 0 ~ "Not_voted"
    ),
    gen2016 = case_when(
      gen2016 == 1 ~ "Mail",
      gen2016 >= 2 ~ "In_person",
      gen2016 == 0 ~ "Not_voted"
    )
  )

# Now pull gen2020, and gen2016 election results:
co_pres_2020 <- read_csv(
  paste0(
    "https://raw.githubusercontent.com/",
    "tonmcg/US_County_Level_Election_Results_08-20/",
    "master/2020_US_County_Level_Presidential_Results.csv"
  )
) %>%
  filter(state_name %in% "Colorado") %>%
  mutate(county = tolower(county_name)) %>%
  mutate(
    result = case_when(
      per_gop > per_dem ~ "Republican",
      per_dem > per_gop ~ "Democrat"
    )
  ) %>%
  mutate(county = gsub("\\s*\\w*$", "", county)) %>%
  select(county, result)

co_pres_2016 <- read_csv(
  paste0(
    "https://raw.githubusercontent.com/mkearney/",
    "presidential_election_county_results_2016/",
    "master/data/pres.elect16.results.2018.csv"
  )
) %>%
  filter(st %in% "CO") %>%
  filter(!is.na(county)) %>%
  select(county, lead) %>%
  dedup() %>%
  mutate(
    result = case_when(
      lead %in% "Hillary Clinton" ~ "Democrat",
      lead %in% "Donald Trump" ~ "Republican"
    )
  ) %>%
  mutate(county = tolower(county)) %>%
  mutate(county = gsub("\\s*\\w*$", "", county)) %>%
  select(-lead)

# Join to create a full tibble:
join_2020 <- inner_join(co_pres_2020, vote_2020, by = "county")
join_2016 <- inner_join(co_pres_2016, vote_2016, by = "county")
t_plot <- rbind(join_2020, join_2016) %>%
  group_by(county) %>%
  mutate(rep_2016 = ifelse(result == "Republican" & year == 2016, 1, 0)) %>%
  mutate(rep_2016 = sum(rep_2016, na.rm = TRUE)) %>%
  ungroup()

# Making the plot!
p <- ggtern(t_plot, aes(`Not voted`, Mail, `In person`)) +
  geom_point(aes(color = result, shape = as.factor(year))) +
  labs(color = "Outcome", shape = "Year") +
  xlab("No Turnout") +
  ylab("Mail") +
  zlab("In Person") +
  theme_bw() +
  theme_arrownormal() +
  geom_line(aes(group = county), size = 0.2, alpha = 0.1) +
  scale_color_brewer(palette = "Set1", direction = -1)

p
p + facet_wrap(~rep_2016)
