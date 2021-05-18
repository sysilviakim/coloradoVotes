source(here::here("R", "utilities.R"))

# COVID Data ===================================================================
# New York Times COVID-19 Data Repository
# Updated periodically
# https://github.com/nytimes/covid-19-data/tree/master/rolling-averages

covid_data <- read_csv(
  paste0(
    "https://raw.githubusercontent.com/nytimes/", 
    "covid-19-data/master/us-counties.csv"
  )
)

# Population Data ==============================================================
# Data from census county population estimates

population <- read_csv(
  paste0(
    "https://www2.census.gov/programs-surveys/popest/datasets/",
    "2010-2020/counties/totals/co-est2020-alldata.csv"
  )
) %>%
  clean_names()

# Cleaning =====================================================================
# Filtering for Colorado counties only
covid_colorado <- covid_data %>%
  filter(state %in% "Colorado")
assert_that(year(min(covid_colorado$date)) == 2020)

co_population <- population %>%
  filter(stname %in% "Colorado") %>%
  mutate(ctyname = str_remove(ctyname, " County")) %>%
  select(ctyname, popestimate2020) %>%
  rename(county = ctyname)

# Checking counties ============================================================
assert_that(
  length(setdiff(unique(covid_colorado$county), c(NA, "Unknown"))) == 64
)
assert_that(length(setdiff(unique(co_population$county), "Colorado")) == 64)
# All counties are accounted for. In the population data, there is one
# row for the total population of Colorado in the county column.

# Joining the data =============================================================
co_covid <- inner_join(co_population, covid_colorado, by = "county")

co_covid <- co_covid %>%
  select(county, popestimate2020, date, cases, deaths) %>%
  rename(population = popestimate2020)

# Calculating proportions ======================================================
co_covid <- co_covid %>%
  mutate(
    cases_per_million = cases / (population / 1e6),
    deaths_per_million = deaths / (population / 1e6),
    cases_per_100k = cases / (population / 1e5),
    deaths_per_100k = deaths / (population / 1e5),
    cases_per_10k = cases / (population / 1e4),
    deaths_per_10k = deaths / (population / 1e4)
  )

# Saving =======================================================================
save(co_covid, file = here("data", "tidy", "co_county_covid.Rda"))
