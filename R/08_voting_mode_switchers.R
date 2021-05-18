source(here::here("R", "utilities.R"))

# Import history wide data =====================================================
if (nrows == 100) {
  load(here("data", "tidy", "sample", "voter_history_wide_sample.RData"))
} else {
  load(here("data", "tidy", "voter_history_wide_full.RData"))
}

