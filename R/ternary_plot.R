source(here::here("R", "utilities.R"))
library(ggtern)
library(fontcm)

# Import Data ==================================================================
vote_wide <-
  loadRData(here("data", "tidy", "voter_history_wide_full.RData")) %>%
  rename(county = county_name)
pres <- loadRData(here("data", "tidy", "co_county_pres_wide.Rda"))

# Simple summary ===============================================================
temp <- vote_wide %>%
  ungroup() %>%
  select(-voter_id, -party, -county) %>%
  mutate(
    across(everything(), ~ case_when(.x == 1 ~ 0, .x > 1 ~ 1, TRUE ~ NA_real_))
  )
names(temp) %>%
  map(~ prop(temp %>% filter(!is.na(!!as.name(.x))), .x, print = FALSE))

# Wrangle/join data ============================================================

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

# Making the plot! =============================================================
p <- ternary_extra(
  ggtern(t_plot, aes(x = not_voted, y = mail, z = in_person)) +
    geom_point(aes(color = winner, shape = year))
)

pdf(here("fig", "ternary_all.pdf"), width = 7, height = 7)
print(pdf_default(p) + theme(legend.position = c(0.325, 0.8)))
dev.off()

# Facetted by 2016 result ======================================================
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

# Descriptives =================================================================
county_summ(t_plot, "winner")
county_summ(t_plot, "winner_2016")

