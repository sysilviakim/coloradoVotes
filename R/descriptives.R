source(here::here("R", "utilities.R"))
library(xtable)
options(scipen = 999)
orig <- loadRData(here("data", "tidy", "multiclass_complete.Rda"))

df <- orig %>% select(-county, -in_person_vote_date)
assert_that(!any(is.na(df)))

# Descriptives
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

# Types ========================================================================
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
