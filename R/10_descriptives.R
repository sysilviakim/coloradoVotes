source(here::here("R", "utilities.R"))
df <- loadRData(here("data", "tidy", "multiclass_county_collapsed.Rda"))
df_switched <- loadRData(here("data", "tidy", "switcher.Rda")) %>%
  filter(!is.na(age_groups))
## assert_that(!any(is.na(df)))
load(here("data", "tidy", "co_county_covid_summary.Rda"))
county_covd <- county_covd %>%
  mutate(
    deaths_delta = deaths_delta * 100,
    cases_delta = cases_delta * 100
  )

df <- left_join(df, county_covd %>% rename(county_full = county))
df_switched <- left_join(df_switched, county_covd)

# Percentages / conditional probabilities ======================================
prop(df, "gen2020")
prop(df_switched, "switcher")
table(df$gen2020)
table(df_switched$switcher)

temp <- xtable(prop(df, c("gen2020", "party")))
names(temp) <- c("Democrat", "Republican", "Other")

print(
  temp,
  file = here("tab", "prop_table.tex"),
  booktabs = TRUE, floating = FALSE
)

temp <- df_switched %>% 
  group_by(county) %>%
  summarise(switcher = mean(as.numeric(switcher) - 1, na.rm = TRUE)) %>%
  arrange(desc(switcher)) %>%
  left_join(., county_covd)

cor.test(temp$switcher, temp$cases_delta) ## p-value = 0.6941
cor.test(temp$switcher, temp$deaths_delta) ## p-value = 0.9104

temp <- df %>%
  group_by(county) %>%
  mutate(
    in_person = case_when(
      gen2020 == "In person" ~ 1,
      TRUE ~ 0
    )
  ) %>%
  summarise(in_person = mean(in_person, na.rm = TRUE)) %>%
  left_join(., county_covd)

cor.test(temp$in_person, temp$cases_delta) ## p-value = 0.3689
cor.test(temp$in_person, temp$deaths_delta) ## p-value = 0.1189

pretty_condprob(df, A_var = "gen2020", "In person", B_var = "party", "rep")
## Cond. on party == rep, Pr(gen2020 == In person) is 6.9%
pretty_condprob(df, A_var = "gen2020", "In person", B_var = "party", "dem")
## Cond. on party == dem, Pr(gen2020 == In person) is 3.4%

pretty_condprob(df_switched, A_var = "switcher", "Yes", B_var = "party", "rep")
## Cond. on party == rep, Pr(switcher == Yes) is 5.2%
pretty_condprob(df_switched, A_var = "switcher", "Yes", B_var = "party", "dem")
## Cond. on party == dem, Pr(switcher == Yes) is 1.9%

pretty_condprob(df, A_var = "gen2020", "In person", B_var = "reg_bin", "EDR")
## Cond. on reg_bin == EDR, Pr(gen2020 == In person) is 92.9%
pretty_condprob(df_switched, A_var = "switcher", "Yes", B_var = "reg_bin", "EDR")
## Cond. on reg_bin == EDR, Pr(switcher == Yes) is 89.1%

pretty_condprob(df, A_var = "gen2020", "In person", B_var = "reg_bin", "15Days")
## Cond. on reg_bin == 15Days, Pr(gen2020 == In person) is 48.1%
pretty_condprob(df_switched, A_var = "switcher", "Yes", B_var = "reg_bin", "15Days")
## Cond. on reg_bin == 15Days, Pr(switcher == Yes) is 39.8%

pretty_condprob(df, A_var = "gen2020", "In person", B_var = "reg_bin", "30Days")
## Cond. on reg_bin == 30Days, Pr(gen2020 == In person) is 10.4%
pretty_condprob(df_switched, A_var = "switcher", "Yes", B_var = "reg_bin", "30Days")
## Cond. on reg_bin == 30Days, Pr(switcher == Yes) is 6.3%

pretty_condprob(df, A_var = "gen2020", "In person", B_var = "reg_bin", "30+")
## Cond. on reg_bin == 30+, Pr(gen2020 == In person) is 4.3%
pretty_condprob(df_switched, A_var = "switcher", "Yes", B_var = "reg_bin", "30+")
## Cond. on reg_bin == 30+, Pr(switcher == Yes) is 3.3%

pretty_condprob(df, A_var = "reg_bin", "EDR", B_var = "gen2020", "In person")
## Cond. on gen2020 == In person, Pr(reg_bin == EDR) is 8.8%
pretty_condprob(df_switched, A_var = "reg_bin", "EDR", B_var = "switcher", "Yes")
## Cond. on gen2020 == In person, Pr(reg_bin == EDR) is 8.8%

pretty_condprob(df, A_var = "reg_bin", "15Days", B_var = "gen2020", "In person")
## Cond. on gen2020 == In person, Pr(reg_bin == 15Days) is 10.8%
pretty_condprob(df_switched, A_var = "reg_bin", "15Days", B_var = "switcher", "Yes")
## Cond. on switcher == Yes, Pr(reg_bin == 15Days) is 3.4%

pretty_condprob(df, A_var = "reg_bin", "30Days", B_var = "gen2020", "In person")
## Cond. on gen2020 == In person, Pr(reg_bin == 30Days) is 2.4%
pretty_condprob(df_switched, A_var = "reg_bin", "30Days", B_var = "switcher", "Yes")
## Cond. on switcher == Yes, Pr(reg_bin == 30Days) is 0.8%

pretty_condprob(df, A_var = "reg_bin", "30+", B_var = "gen2020", "In person")
## Cond. on gen2020 == In person, Pr(reg_bin == 30+) is 77.9%
pretty_condprob(df_switched, A_var = "reg_bin", "30+", B_var = "switcher", "Yes")
## Cond. on switcher == Yes, Pr(reg_bin == 30+) is 93.9%

# Types of voting patterns =====================================================
summ <- df %>%
  group_by(gen2020, pri2020, gen2018, pri2018, gen2016, pri2016, party) %>%
  summarise(n = n()) %>%
  group_by(gen2020, pri2020, gen2018, pri2018, gen2016, pri2016) %>%
  mutate(
    prop = n / sum(n, na.rm = TRUE),
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

## Top 10 types of voting history patterns =====================================
nrow(summ) ## 578 types
temp <- xtable(head(summ, 10), align = "lllllllrrrr")
names(temp) <- c(
  cross2(c("Gen. ", "Pri. "), c(2020, 2018, 2016)) %>%
    map_chr(~ paste(.x, collapse = "")),
  "Obs.", "Dem. (%)", "Rep. (%)", "Others (%)"
)
print(
  temp,
  file = here("tab", "top_10_voting_history.tex"),
  include.rownames = FALSE, booktabs = TRUE, floating = FALSE
)

## If limited to gen2020 in-persons ============================================
temp <- xtable(
  head(summ %>% filter(gen2020 == "In person"), 10),
  align = "lllllllrrrr"
)
names(temp) <- c(
  cross2(c("Gen. ", "Pri. "), c(2020, 2018, 2016)) %>%
    map_chr(~ paste(.x, collapse = "")),
  "Obs.", "Dem. (%)", "Rep. (%)", "Others (%)"
)
print(
  temp,
  file = here("tab", "top_10_voting_history_in_person.tex"),
  include.rownames = FALSE, booktabs = TRUE, floating = FALSE
)

# Summarize in-person voting by ================================================
## County ======================================================================
p <- county_stacked_plot(df)
pdf(here("fig", "inperson_by_county.pdf"), width = 8.5, height = 4)
print(
  pdf_default(p) +
    theme(
      axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
      legend.position = c(0.9, 0.8)
    )
)
dev.off()

p1 <- ggplot(p$data, aes(prop)) +
  geom_density() +
  scale_x_continuous(labels = percent) +
  xlab("Proportion of In-person Votes") +
  ylab("Density")

pdf(here("fig", "inperson_by_county_density.pdf"), width = 4, height = 2.5)
print(pdf_default(p1))
dev.off()

p2 <- ggplot(p$data, aes(sample = prop)) +
  stat_qq() +
  stat_qq_line() +
  xlab("Theoretical Quantiles") +
  ylab("In-person Vote Proportion's Quantiles")

pdf(here("fig", "inperson_by_county_qq.pdf"), width = 4, height = 2.5)
print(pdf_default(p2))
dev.off()

library(nortest)
ad.test(p$data$prop)
cvm.test(p$data$prop)

## County designation ==========================================================
df %>%
  group_by(county_designation, gen2020) %>%
  summarise(n = n()) %>%
  mutate(prop = n / sum(n, na.rm = TRUE)) %>%
  group_by(county_designation) %>%
  mutate(n = sum(n, na.rm = TRUE)) %>%
  filter(gen2020 == "In person") %>%
  mutate(se = sqrt(prop * (1 - prop) / n))

# county_designation   switcher       n   prop       se
# <fct>                <fct>      <int>  <dbl>    <dbl>
# 1 urban              In person 3246409 0.0541 0.000126
# 2 rural              In person  379614 0.0504 0.000355
# 3 frontier           In person   87000 0.0377 0.000646

## COVID-19 prevalence =========================================================
p <- county_stacked_plot(df, fill = "cases_per_10k", continuous = TRUE)
pdf(
  here("fig", "inperson_by_county_covid_cases.pdf"),
  width = 8.5, height = 4.1
)
print(
  pdf_default(p) +
    theme(
      axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
      legend.position = c(0.8, 0.7425)
    )
)
dev.off()

p <- county_stacked_plot(df, fill = "deaths_per_10k", continuous = TRUE)
pdf(
  here("fig", "inperson_by_county_covid_deaths.pdf"),
  width = 8.5, height = 4.1
)
print(
  pdf_default(p) +
    theme(
      axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
      legend.position = c(0.8, 0.7425)
    )
)
dev.off()

## COVID-19 increase ===========================================================
p <- county_stacked_plot(df, fill = "cases_delta", continuous = TRUE)
pdf(
  here("fig", "inperson_by_county_cases_increase.pdf"),
  width = 8.5, height = 4.1
)
print(
  pdf_default(p) +
    theme(
      axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
      legend.position = c(0.8, 0.7425)
    )
)
dev.off()

p <- county_stacked_plot(df, fill = "deaths_delta", continuous = TRUE)
pdf(
  here("fig", "inperson_by_county_death_increase.pdf"),
  width = 8.5, height = 4.1
)
print(
  pdf_default(p) +
    theme(
      axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
      legend.position = c(0.8, 0.7425)
    )
)
dev.off()

## By party x age ==============================================================
## Will not include designation x party
p <- party_stacked_plot(df)
pdf(here("fig", "inperson_by_age_party.pdf"), width = 6, height = 4)
print(
  pdf_default(p) +
    theme(
      axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
      legend.position = "bottom"
    )
)
dev.off()

df %>%
  group_by(gen2020) %>%
  summarise(mean_age = mean(age, na.rm = TRUE))

## By party x registration date ================================================
df[["reg_month"]] <- case_when(
  year(df$registration_date) < 2017 ~ NA_Date_,
  TRUE ~ as.Date(floor_date(df$registration_date, unit = "month"))
)

p <- party_stacked_plot(df, x = "reg_month")
pdf(here("fig", "inperson_by_reg_month_party.pdf"), width = 6, height = 4)
print(pdf_default(p) + theme(legend.position = "bottom"))
dev.off()

# Summarize switches by ========================================================
## County ======================================================================
p <- county_stacked_plot(
  df_switched,
  y = "switcher", county = "county", yint = 0.03
)
pdf(here("fig", "switch_by_county.pdf"), width = 8.5, height = 4)
print(
  pdf_default(p) +
    theme(
      axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
      legend.position = c(0.9, 0.8)
    )
)
dev.off()

p1 <- ggplot(p$data, aes(prop)) +
  geom_density() +
  scale_x_continuous(labels = percent) +
  xlab("Proportion of Switchers") +
  ylab("Density")

pdf(here("fig", "switch_by_county_density.pdf"), width = 4, height = 2.5)
print(pdf_default(p1))
dev.off()

p2 <- ggplot(p$data, aes(sample = prop)) +
  stat_qq() +
  stat_qq_line() +
  xlab("Theoretical Quantiles") +
  ylab("Switcher Proportion's Quantiles")

pdf(here("fig", "switch_by_county_qq.pdf"), width = 4, height = 2.5)
print(pdf_default(p2))
dev.off()

library(nortest)
ad.test(p$data$prop)
cvm.test(p$data$prop)

## County designation ==========================================================
df_switched %>%
  group_by(county_designation, switcher) %>%
  summarise(n = n()) %>%
  mutate(prop = n / sum(n, na.rm = TRUE)) %>%
  group_by(county_designation) %>%
  mutate(n = sum(n, na.rm = TRUE)) %>%
  filter(switcher == "Yes") %>%
  mutate(se = sqrt(prop * (1 - prop) / n))

# county_designation switcher       n   prop       se
# <fct>              <fct>      <int>  <dbl>    <dbl>
# 1 urban              Yes      2301411 0.0354 0.000122
# 2 rural              Yes       273545 0.0343 0.000348
# 3 frontier           Yes        63333 0.0251 0.000621

## COVID-19 prevalence =========================================================
p <- county_stacked_plot(
  df_switched, continuous = TRUE,
  y = "switcher", county = "county", fill = "cases_per_10k", yint = 0.03
)
pdf(here("fig", "switch_by_county_covid_cases.pdf"), width = 8.5, height = 4.1)
print(
  pdf_default(p) +
    theme(
      axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
      legend.position = c(0.8, 0.72)
    )
)
dev.off()

p <- county_stacked_plot(
  df_switched, continuous = TRUE,
  y = "switcher", county = "county", fill = "deaths_per_10k", yint = 0.03
)
pdf(here("fig", "switch_by_county_covid_deaths.pdf"), width = 8.5, height = 4.1)
print(
  pdf_default(p) +
    theme(
      axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
      legend.position = c(0.8, 0.7425)
    )
)
dev.off()

## COVID-19 increase ===========================================================
p <- county_stacked_plot(
  df_switched, fill = "cases_delta", continuous = TRUE,
  county = "county", yint = 0.03
)
pdf(
  here("fig", "switch_by_county_cases_increase.pdf"),
  width = 8.5, height = 4.1
)
print(
  pdf_default(p) +
    theme(
      axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
      legend.position = c(0.8, 0.7425)
    )
)
dev.off()

p <- county_stacked_plot(
  df_switched, fill = "deaths_delta", continuous = TRUE,
  county = "county", yint = 0.03
)
pdf(
  here("fig", "switch_by_county_death_increase.pdf"),
  width = 8.5, height = 4.1
)
print(
  pdf_default(p) +
    theme(
      axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
      legend.position = c(0.8, 0.7425)
    )
)
dev.off()

## By party x age ==============================================================
## Will not include designation x party
p <- party_stacked_plot(df_switched)
pdf(here("fig", "switch_by_age_party.pdf"), width = 6, height = 4)
print(
  pdf_default(p) +
    theme(
      axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
      legend.position = "bottom"
    )
)
dev.off()

df_switched %>%
  group_by(switcher) %>%
  summarise(mean_age = mean(age, na.rm = TRUE))

## By party x registration date ================================================
df_switched[["reg_month"]] <- case_when(
  year(df_switched$registration_date) < 2017 ~ NA_Date_,
  TRUE ~ as.Date(floor_date(df_switched$registration_date, unit = "month"))
)

p <- party_stacked_plot(df_switched, x = "reg_month")
pdf(here("fig", "switch_by_reg_month_party.pdf"), width = 6, height = 4)
print(pdf_default(p) + theme(legend.position = "bottom"))
dev.off()

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
  ggtern(t_plot, aes(x = mail, y = in_person, z = not_voted)) +
    geom_point(aes(color = winner, shape = year))
)

pdf(here("fig", "ternary_all.pdf"), width = 7, height = 7)
print(pdf_default(p) + theme(legend.position = c(0.325, 0.8)))
dev.off()

## Facetted by 2016 result =====================================================
p <- ternary_extra(
  ggtern(
    t_plot %>% filter(winner_2016 == "Dem"),
    aes(x = mail, y = in_person, z = not_voted)
  ) + geom_point(aes(color = winner, shape = year))
)

pdf(here("fig", "ternary_2016_dem_counties.pdf"), width = 7, height = 7)
print(pdf_default(p) + theme(legend.position = c(0.325, 0.8)))
dev.off()

p <- ternary_extra(
  ggtern(
    t_plot %>% filter(winner_2016 == "Rep"),
    aes(x = mail, y = in_person, z = not_voted)
  ) + geom_point(aes(color = winner, shape = year))
)

pdf(here("fig", "ternary_2016_rep_counties.pdf"), width = 7, height = 7)
print(pdf_default(p) + theme(legend.position = c(0.325, 0.8)))
dev.off()

## Descriptives ================================================================
county_summ(t_plot, "winner")
county_summ(t_plot, "winner_2016")
