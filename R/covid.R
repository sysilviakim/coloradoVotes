source("R/00_utilities.R")
library(readxl)

# Data source ==================================================================

# COVID-19 Data Repository by the Center for Systems Science and Engineering
# (CSSE) at Johns Hopkins University
# https://github.com/CSSEGISandData/COVID-19
# "All data is read in from the daily case report"

url <- paste0(
  "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/",
  "csse_covid_19_data/csse_covid_19_time_series/"
)
td <- format(Sys.Date(), "%Y%m%d")


# US Countywide COVID-19 cases/deaths ==========================================
us_cases <- read_csv(paste0(url, "time_series_covid19_confirmed_US.csv"))
us_death <- read_csv(paste0(url, "time_series_covid19_deaths_US.csv"))

# assert_that(
#   us_cases %>% filter(Province_State == "Colorado") %>% nrow() == 64
# )
# False because there are additional categories of "Out of CO" and "Unassigned"

if (!dir.exists(file.path("data/tidy", "COVID19"))) {
  dir.create(file.path("data/raw/", "COVID19"))
  dir.create(file.path("data/tidy", "COVID19"))
}

save(us_cases, file = paste0("data/tidy/COVID19/", "us_cases_jhu_", td, ".Rda"))
save(us_death, file = paste0("data/tidy/COVID19/", "us_death_jhu_", td, ".Rda"))

# By-state long data ===========================================================
us_long <- list(us_cases, us_death) %>%
  set_names(c("cases", "death")) %>%
  map(
    ~ .x %>%
      select(
        -UID, -iso2, -iso3, -code3, -FIPS, -Admin2, -Country_Region,
        -Lat, -Long_, -Combined_Key
      ) %>%
      rename(state = Province_State) %>%
      pivot_longer(cols = setdiff(names(.), "state"), names_to = "date") %>%
      filter(state %in% c(state.name, "District of Columbia")) %>%
      group_by(state, date) %>%
      summarise(value = sum(value, na.rm = TRUE)) %>%
      ungroup()
  )

us_long <- left_join(
  us_long$cases %>% rename(cases = value),
  us_long$death %>% rename(death = value)
) %>%
  mutate(date = parse_date(date, format = "%m/%d/%y")) %>%
  arrange(state, date)

assert_that(!any(is.na(us_long$cases)))
assert_that(!any(is.na(us_long$death)))

# Import population data from Census ===========================================

if (!file.exists("data/raw/COVID19/us_state_pop_census.xlsx")) {
  download.file(
    paste0(
      "https://www2.census.gov/programs-surveys/popest/tables/",
      "2010-2019/state/totals/nst-est2019-01.xlsx"
    ),
    destfile = "data/raw/COVID19/us_state_pop_census.xlsx",
    mode = "wb"
  )
}

us_pop <- read_xlsx(
  "data/raw/COVID19/us_state_pop_census.xlsx",
  range = "A10:B60",
  col_names = c("state", "census_pop_202004")
) %>%
  mutate(
    state = gsub("\\.", "", state)
  )

# Join COVID cases with population estimates + cases per million ===============
us_long <- left_join(us_long, us_pop) %>%
  mutate(
    cases_mill = cases / (census_pop_202004 / 1e6),
    death_mill = death / (census_pop_202004 / 1e6),
    cases_thsd = cases / (census_pop_202004 / 1e3),
    death_thsd = death / (census_pop_202004 / 1e3),
    States = if_else(state == "Colorado", "Colorado", "Others"),
    state = factor(
      state,
      levels = c(setdiff(unique(state), "Colorado"), "Colorado")
    )
  )
assert_that(!any(is.na(us_long$census_pop_202004)))

# Time-series trend ============================================================
fig_list <- c("cases_thsd", "death_thsd") %>%
  map(
    ~ us_long %>%
      filter(
        date < as.Date("2020-11-04") &
          date > as.Date("2020-03-01")
      ) %>%
      ggplot() +
      geom_line(
        aes(x = date, y = !!as.name(.x), group = state, color = States)
      ) +
      scale_colour_manual(values = c("Colorado" = "red", "Others" = "gray")) +
      scale_x_date(breaks = "month", labels = function(x) format(x, "%b\n%Y")) +
      xlab("Date") +
      ylab(
        if_else(.x == "cases_thsd", "Cases Per Thousand", "Deaths Per Thousand")
      )
  ) %>%
  map(~ Kmisc::pdf_default(.x))

# Export into PDFs =============================================================
pdf("fig/covid_cases_jhu_cases_thsd.pdf", width = 7, height = 4)
fig_list[[1]]
dev.off()

pdf("fig/covid_cases_jhu_death_thsd.pdf", width = 7, height = 4)
fig_list[[2]]
dev.off()

# Just Colorado ================================================================

fig_list <- c("cases_thsd", "death_thsd") %>%
  map(
    ~ us_long %>%
      filter(
        date < as.Date("2020-11-04") &
          date > as.Date("2020-03-01") & 
          state == "Colorado"
      ) %>%
      ggplot() +
      geom_line(aes(x = date, y = !!as.name(.x))) +
      scale_x_date(breaks = "month", labels = function(x) format(x, "%b\n%Y")) +
      xlab("Date") +
      ylab(
        if_else(.x == "cases_thsd", "Cases Per Thousand", "Deaths Per Thousand")
      )
  ) %>%
  map(~ Kmisc::pdf_default(.x))

# Decided not to export separately
us_long %>%
  filter(day(date) == 1 & state == "Colorado")


# So in terms of "rankings", how bad was Colorado out of 51 jurisdictions? =====
options(digits = 7, scipen = 999)

us_long <- us_long %>%
  filter(date == as.Date("2020-11-03")) %>%
  arrange(desc(death_thsd)) %>%
  mutate(death_worst = row_number()) %>%
  arrange(desc(cases_thsd)) %>%
  mutate(cases_worst = row_number())

us_long %>%
  filter(state == "Colorado") %>%
  select(matches("worst|cases|death")) %>%
  mutate_all(~ formatC(.x, format = "f", digits = 2))

# Out of curiosity... which state had it worst on Election Day? ================

us_long %>%
  filter(cases_worst < 3 | death_worst < 3) %>%
  select(state, cases_thsd, death_thsd, cases, death) %>%
  print(width = 1e6)

#   A tibble: 4 x 5
#   state        cases_thsd death_thsd  cases death
#   <fct>             <dbl>      <dbl>  <dbl> <dbl>
# 1 North Dakota       70.2      0.825  47187   555
# 2 South Dakota       60.0      0.548  48854   446
# 3 New Jersey         27.6      1.86  242825 16371
# 4 New York           26.7      1.74  517822 33773
