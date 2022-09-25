source(here::here("R", "utilities.R"))
orig <- loadRData(here("data", "tidy", "multiclass_complete.Rda"))
df_switched <-
  loadRData(here("data", "tidy", "multiclass_county_collapsed.Rda"))

df <- orig %>% select(-county, -in_person_vote_date)

assert_that(!any(is.na(df)))

# Simple percentages ===========================================================
temp <- prop(
  df %>%
    mutate(
      party = as.character(party),
      party = case_when(party == "dem" | party == "rep" ~ party, TRUE ~ "oth")
    ),
  c("gen2020", "party")
) %>%
  xtable()

names(temp) <- c("Dem.", "Rep.", "Oth.")

print(
  temp,
  file = here("tab", "prop_table.tex"),
  include.rownames = FALSE, booktabs = TRUE, floating = FALSE
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
  head(summ %>% filter(gen2020 == "In person"), 5),
  align = "lllllllrrrr"
)
names(temp) <- c(
  cross2(c("Gen. ", "Pri. "), c(2020, 2018, 2016)) %>%
    map_chr(~ paste(.x, collapse = "")),
  "Obs.", "Dem (%)", "Rep (%)", "Others (%)"
)
print(
  temp,
  file = here("tab", "top_5_voting_history_in_person.tex"),
  include.rownames = FALSE, booktabs = TRUE, floating = FALSE
)

# Summarize switches by county =================================================
switch_by_county <- df_switched %>%
  group_by(county_full, switcher) %>%
  summarise(n = n(), county_designation = last(county_designation)) %>%
  mutate(sum = sum(n)) %>%
  mutate(prop = n / sum) %>%
  filter(switcher %in% "Yes") %>%
  rowwise() %>%
  mutate(
    county_full = simple_cap(county_full),
    county_designation = simple_cap(as.character(county_designation))
  ) %>%
  ungroup()
summary(switch_by_county$prop)

switch_avg <- as.numeric(prop(df_switched, "switcher", digit = 3)[[2]]) / 100
p <- ggplot(switch_by_county, aes(x = fct_reorder(county_full, desc(prop)))) +
  geom_col(
    aes(y = prop, color = NULL, fill = county_designation)
  ) +
  scale_y_continuous(labels = percent) +
  scale_fill_viridis_d(end = 0.9, direction = -1) +
  xlab("Counties") +
  ylab("Proportion of Switchers") +
  labs(fill = "Designation") +
  geom_hline(yintercept = switch_avg) +
  annotate(
    geom = "text", x = 50, y = 0.04, family = "CM Roman",
    label = paste0("Average: ", switch_avg * 100, "%")
  )
p

pdf(here("fig", "switch_by_county.pdf"), width = 8.5, height = 4)
print(
  pdf_default(p) +
    theme(
      axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
      legend.position = c(0.9, 0.8)
    )
)
dev.off()

p1 <- ggplot(switch_by_county, aes(prop)) +
  geom_density() +
  scale_x_continuous(labels = percent) + 
  xlab("Proportion of Switchers") + 
  ylab("Density")

pdf(here("fig", "switch_by_county_density.pdf"), width = 4, height = 2.5)
print(pdf_default(p1))
dev.off()

p2 <- ggplot(switch_by_county, aes(sample = prop)) +
  stat_qq() + 
  stat_qq_line() + 
  xlab("Theoretical Quantiles") +
  ylab("Switcher Proportion's Quantiles")

pdf(here("fig", "switch_by_county_qq.pdf"), width = 4, height = 2.5)
print(pdf_default(p2))
dev.off()

library(normtest)
ad.test(switch_by_county$prop)
cvm.test(switch_by_county$prop)

summary(
  lm(as.numeric(switcher) ~ party * county_designation, data = df_switched)
)

# Summarize switches by county designation =====================================
table_2 <- df_switched %>%
  group_by(county_designation, switcher) %>%
  summarise(n = n()) %>%
  mutate(sum = sum(n)) %>%
  mutate(prop = n / sum) %>%
  filter(switcher %in% "Yes") %>%
  select(-switcher, -sum) %>%
  xtable()

# county_designation     n   prop
# <fct>              <int>  <dbl>
#   1 urban              81501 0.0354
# 2 rural               9396 0.0343
# 3 frontier            1588 0.0251

names(table_2) <- c("County Designation", "N", "Proportion")

print(
  table_2,
  file = here("tab", "switches_county_designation_split.tex"),
  include.rownames = FALSE, booktabs = TRUE, floating = FALSE
)

# Individual level voting behavior: in 2020 ====================================
table_3 <- df_switched %>%
  group_by(switcher, party, county_designation) %>%
  summarise(n = n()) %>%
  mutate(sum = sum(n)) %>%
  mutate(prop = n / sum) %>%
  filter(switcher %in% "Yes") %>%
  ungroup() %>%
  select(-switcher, -sum) %>%
  xtable()

# party county_designation     n   prop
# <chr> <fct>              <int>  <dbl>
#   1 dem   urban              14577 0.899
# 2 dem   rural               1407 0.0868
# 3 dem   frontier             228 0.0141
# 4 oth   urban              30054 0.878
# 5 oth   rural               3686 0.108
# 6 oth   frontier             505 0.0147
# 7 rep   urban              36870 0.877
# 8 rep   rural               4303 0.102
# 9 rep   frontier             855 0.0203

names(table_3) <- c("Party", "County Designation", "N", "Proportion")

print(
  table_3,
  file = here("tab", "switches_partisan_split.tex"),
  include.rownames = FALSE, booktabs = TRUE, floating = FALSE
)
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
