load("data/tidy/df_clean_latest.Rda")

# Add switchers between general 2020 elections, and the other general elections
df_joined_switch <- df_joined %>%
  mutate(swtiched = case_when(
    gen2020 == gen2018 | gen2020 == gen2016 | gen2020 == gen2014 ~ "no switch",
    TRUE ~ "switcher"
  ))

# Summarize swtiches by county
table_1 <- df_joined_switch %>%
  group_by(county, swtiched) %>%
  summarise(n = n()) %>%
  mutate(prop = n/sum(n)) %>%
  filter(swtiched %in% "switcher") %>%
  arrange(desc(prop)) %>%
  select(-swtiched) %>%
  head(10) 

# county          n  prop
# <chr>       <int> <dbl>
# 1 lake         1265 0.262
# 2 el paso    108904 0.247
# 3 denver     111984 0.247
# 4 adams       66002 0.243
# 5 summit       5186 0.239
# 6 teller       4684 0.239
# 7 san miguel   1405 0.236
# 8 broomfield  12065 0.230
# 9 archuleta    2429 0.230
# 10 arapahoe    89898 0.224

# Summarize switches by type of county designation: does seem to have interesting
# results
table_2 <- df_joined_switch %>%
  group_by(swtiched, county_designation) %>%
  summarise(n = n()) %>%
  mutate(prop = n/sum(n)) %>%
  arrange(desc(prop)) %>%
  filter(swtiched %in% "switcher") %>%
  ungroup() %>%
  select(-1)

# swtiched  county_designation       n   prop
# <chr>     <chr>                <int>  <dbl>
# 1 switcher  urban               505414 0.617 
# 2 no switch urban              1715789 0.592 
# 3 no switch rural               800769 0.276 
# 4 switcher  rural               215941 0.264 
# 5 no switch suburban            243771 0.0841
# 6 switcher  suburban             66801 0.0816
# 7 no switch frontier            137352 0.0474
# 8 switcher  frontier             30768 0.0376

# Individual level voting behavior: in 2020

df_joined_switch %>%
  group_by(swtiched, party, county_designation) %>%
  summarise(n = n()) %>%
  mutate(prop = n/sum(n)) %>%
  arrange(desc(prop)) %>%
  filter(swtiched %in% "switcher") %>%
  ungroup() %>%
  select(-1) %>%
  xtable()







