source(here::here("R", "utilities.R"))
library(nnet)
library(caret)
fname <- here("data", "tidy", "multinom.Rda")

# Import merged/wrangled data ==================================================
if (!file.exists(fname)) {
  if (nrows == 100) {
    load(here("data", "tidy", "sample", "merged_sample.RData"))
  } else {
    load(here("data", "tidy", "merged_full.RData"))
  }
  df <- df %>%
    mutate(
      ## could do with some optimization; skip for now
      across(c(contains("20")), ~ case_when(.x > 2 ~ 2, TRUE ~ .x)),
      across(
        c(contains("20")), ~ factor(
          .x,
          levels = c(1, 2, 0),
          labels = c("Mail", "In person", "Not voted")
        )
      ),
      yob = as.numeric(yob),
      party = factor(
        party,
        levels = c("dem", "rep", "uaf", "lbr", "grn", "acn", "apv")
      )
    )
  if (nrows == 100) {
    save(df, file = gsub(".Rda", "_sample.Rda", fname))
  } else {
    save(df, file = fname)
  }
} else {
  load(fname)
}

## Finally, collapse counties
temp <- table(df$county)
temp <- sort(temp)
temp

df <- df %>%
  rename(county_full = county) %>%
  mutate(
    county = case_when(
      county_full %in% names(temp)[which(temp < 20000)] ~ "others",
      TRUE ~ county_full
    )
  )

fname <- here("data", "tidy", "multinom_county_collapsed.Rda")
if (nrows == 100) {
  save(df, file = gsub(".Rda", "_sample.Rda", fname))
} else {
  save(df, file = fname)
}
