source(here::here("R", "utilities.R"))
orig <- loadRData(here("data", "tidy", "multiclass_complete.Rda"))

df <- orig %>% select(-county, -in_person_vote_date)
assert_that(!any(is.na(df)))

# Simple percentages ===========================================================
prop(
  df %>%
    mutate(
      party = as.character(party),
      party = case_when(party == "dem" | party == "rep" ~ party, TRUE ~ "oth")
    ),
  c("gen2020", "party")
)
pretty_condprob(df, A_var = "gen2020", "In person", B_var = "party", "rep")
pretty_condprob(df, A_var = "gen2020", "In person", B_var = "party", "dem")

# Types of voting patterns =====================================================
summ <- df %>%
  mutate(
    party = as.character(party),
    party = case_when(
      party == "dem" | party == "rep" ~ party,
      TRUE ~ "oth"
    )
  ) %>%
  group_by(gen2020, pri2020, gen2018, pri2018, gen2016, pri2016, party) %>%
  summarise(n = n()) %>%
  group_by(gen2020, pri2020, gen2018, pri2018, gen2016, pri2016) %>%
  mutate(
    prop = n / sum(n),
    n = sum(n)
  ) %>%
  pivot_wider(names_from = "party", values_from = c("prop")) %>%
  arrange(desc(n)) %>%
  select(-oth, everything()) %>%
  mutate(
    dem = formatC(dem * 100, digits = 1, format = "f"),
    rep = formatC(rep * 100, digits = 1, format = "f"),
    oth = formatC(oth * 100, digits = 1, format = "f"),
    n = prettyNum(n, big.mark = ",")
  )

# Top 10 types of voting history patterns ======================================
nrow(summ) ## 565 types
temp <- xtable(head(summ, 10), align = "lllllllrrrr")
names(temp) <- c(
  cross2(c("Gen. ", "Pri. "), c(2020, 2018, 2016)) %>%
    map_chr(~ paste(.x, collapse = "")),
  "Obs.", "Dem (%)", "Rep (%)", "Others (%)"
)
print(
  temp, 
  file = here("tab", "top_10_voting_history.tex"),
  include.rownames = FALSE, booktabs = TRUE, floating = FALSE
)

# If limited to gen2020 in-persons =============================================
temp <- xtable(
  head(summ %>% filter(gen2020 == "In person"), 10),
  align = "lllllllrrrr"
)
names(temp) <- c(
  cross2(c("Gen. ", "Pri. "), c(2020, 2018, 2016)) %>%
    map_chr(~ paste(.x, collapse = "")),
  "Obs.", "Dem (%)", "Rep (%)", "Others (%)"
)
print(
  temp, 
  file = here("tab", "top_10_voting_history_in_person.tex"),
  include.rownames = FALSE, booktabs = TRUE, floating = FALSE
)

# Summarize switches by county =================================================
table_1 <- df_joined_switch %>%
  group_by(county, switched) %>%
  summarise(n = n()) %>%
  mutate(prop = n/sum(n)) %>%
  filter(switched %in% "switcher") %>%
  arrange(desc(prop)) %>%
  select(-switched) %>%
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

# Summarize switches by county designation =====================================
table_2 <- df_joined_switch %>%
  group_by(switched, county_designation) %>%
  summarise(n = n()) %>%
  mutate(prop = n/sum(n)) %>%
  arrange(desc(prop)) %>%
  filter(switched %in% "switcher") %>%
  ungroup() %>%
  select(-1)

# switched  county_designation       n   prop
# <chr>     <chr>                <int>  <dbl>
# 1 switcher  urban               505414 0.617 
# 2 no switch urban              1715789 0.592 
# 3 no switch rural               800769 0.276 
# 4 switcher  rural               215941 0.264 
# 5 no switch suburban            243771 0.0841
# 6 switcher  suburban             66801 0.0816
# 7 no switch frontier            137352 0.0474
# 8 switcher  frontier             30768 0.0376

# Individual level voting behavior: in 2020 ====================================
df_joined_switch %>%
  group_by(switched, party, county_designation) %>%
  summarise(n = n()) %>%
  mutate(prop = n/sum(n)) %>%
  arrange(desc(prop)) %>%
  filter(switched %in% "switcher") %>%
  ungroup() %>%
  select(-1) %>%
  xtable()

# Ternary plot =================================================================
library(ggtern)
library(fontcm)

vote_wide <-
  loadRData(here("data", "tidy", "voter_history_wide_full.RData")) %>%
  rename(county = county_name)
pres <- loadRData(here("data", "tidy", "co_county_pres_wide.Rda"))

## Simple summary ==============================================================
temp <- vote_wide %>%
  ungroup() %>%
  select(-voter_id, -party, -county) %>%
  mutate(
    across(everything(), ~ case_when(.x == 1 ~ 0, .x > 1 ~ 1, TRUE ~ NA_real_))
  )
names(temp) %>%
  map(~ prop(temp %>% filter(!is.na(!!as.name(.x))), .x, print = FALSE))

## Wrangle/join data ===========================================================
## History-based county's voting mode sums
## Augment presidential results
t_plot <- c(vote_2016 = 2016, vote_2020 = 2020) %>%
  imap_dfr(
    ~ vote_wide %>%
      group_by(county) %>%
      count(!!as.name(paste0("gen", .x))) %>%
      pivot_wider(
        names_from = !!as.name(paste0("gen", .x)), values_from = n
      ) %>%
      mutate(year = .x) %>%
      clean_names()
  ) %>%
  mutate(x2 = sum(x2, x3, x4, x6, na.rm = TRUE)) %>%
  rename(not_voted = x0, mail = x1, in_person = x2) %>%
  select(-contains("x")) %>%
  left_join(., pres) %>%
  mutate(
    winner = ifelse(rep > dem, "Rep", "Dem"),
    winner_2016 = case_when(
      rep > dem & year == 2016 ~ 1,
      rep < dem & year == 2016 ~ 0
    )
  ) %>%
  group_by(county) %>%
  mutate(
    winner_2016 = sum(winner_2016, na.rm = TRUE),
    winner_2016 = case_when(winner_2016 == 1 ~ "Rep", TRUE ~ "Dem")
  ) %>%
  mutate(year = factor(year))

## Create ternary plot =========================================================
p <- ternary_extra(
  ggtern(t_plot, aes(x = not_voted, y = mail, z = in_person)) +
    geom_point(aes(color = winner, shape = year))
)

pdf(here("fig", "ternary_all.pdf"), width = 7, height = 7)
print(pdf_default(p) + theme(legend.position = c(0.325, 0.8)))
dev.off()

## Facetted by 2016 result =====================================================
p <- ternary_extra(
  ggtern(
    t_plot %>% filter(winner_2016 == "Dem"),
    aes(x = not_voted, y = mail, z = in_person)
  ) + geom_point(aes(color = winner, shape = year))
)

pdf(here("fig", "ternary_2016_dem_counties.pdf"), width = 7, height = 7)
print(pdf_default(p) + theme(legend.position = c(0.325, 0.8)))
dev.off()

p <- ternary_extra(
  ggtern(
    t_plot %>% filter(winner_2016 == "Rep"),
    aes(x = not_voted, y = mail, z = in_person)
  ) + geom_point(aes(color = winner, shape = year))
)

pdf(here("fig", "ternary_2016_rep_counties.pdf"), width = 7, height = 7)
print(pdf_default(p) + theme(legend.position = c(0.325, 0.8)))
dev.off()

## Descriptives ================================================================
county_summ(t_plot, "winner")
county_summ(t_plot, "winner_2016")

