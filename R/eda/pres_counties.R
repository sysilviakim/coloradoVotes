# County-level presidential results ============================================
## https://doi.org/10.7910/DVN/VOQCHQ

pres <- read_delim(
  here("data", "raw", "countypres_2000-2020.tab"), delim = "\t"
) %>%
  filter(year >= 2016 & state_po == "CO") %>%
  mutate(
    party = case_when(
      party == "DEMOCRAT" ~ "dem",
      party == "REPUBLICAN" ~ "rep",
      TRUE ~ "oth"
    )
  ) %>%
  rename(county = county_name) %>%
  group_by(year, county, party) %>%
  summarise(vote = sum(candidatevotes))

pres_wide <- pres %>%
  pivot_wider(names_from = "party", values_from = "vote") %>%
  mutate(
    demp = dem / (dem + oth + rep) * 100,
    repp = rep / (dem + oth + rep) * 100,
    othp = 100 - demp - repp
  ) %>%
  select(-dem, -oth, -rep) %>%
  rename(dem = demp, rep = repp, oth = othp) %>%
  mutate(county = tolower(county))

save(pres_wide, file = here("data", "tidy", "co_county_pres_wide.Rda"))

pres_wide %>%
  filter(year == 2016) %>%
  group_by(county) %>%
  mutate(winner = ifelse(dem > rep, "dem", "rep")) %>%
  .$winner %>%
  table()

pres_wide %>% 
  group_by(county, year) %>%
  mutate(winner = ifelse(dem > rep, "dem", "rep")) %>%
  select(county, year, winner) %>%
  pivot_wider(names_from = year, values_from = winner) %>%
  filter(`2016` != `2020`)
