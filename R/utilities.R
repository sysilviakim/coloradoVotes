# Packages =====================================================================
library(plyr)
library(tidyverse)
library(lubridate)
library(janitor)
library(assertthat)
library(here)
library(fst)
library(styler)
library(Kmisc)
library(rlist)

# Other setups =================================================================
if (Sys.info()["sysname"] == "Windows") {
  nrows <- -1
} else {
  nrows <- 100
}
options(scipen = 999)

# Functions ====================================================================

# Find the element with longest nchar
nchar_longest <- function(v) {
  v[order(nchar(v), v, decreasing = TRUE)][1]
}

# Negate %in%
`%notin%` <- Negate(`%in%`)

# Function to find values that don't match
redundant <- function(df, x, y, vid = "voter_id") {
  out <- df %>%
    select(!!as.name(vid), !!as.name(x), !!as.name(y)) %>%
    filter(
      !is.na(!!as.name(x)) & is.na(!!as.name(y)) |
        !is.na(!!as.name(y)) & is.na(!!as.name(x)) |
        !!as.name(x) != !!as.name(y)
    ) %>%
    mutate(across(everything(), ~ gsub("[^[:alnum:]]", "", .x))) %>%
    rowwise() %>%
    filter(
      !is.na(!!as.name(x)) & is.na(!!as.name(y)) |
        !is.na(!!as.name(y)) & is.na(!!as.name(x)) |
        !grepl(!!as.name(x), !!as.name(y)) &
          !grepl(!!as.name(y), !!as.name(x))
    )
  return(
    out %>%
      select(!!as.name(vid)) %>%
      left_join(., df %>% select(!!as.name(vid), !!as.name(x), !!as.name(y)))
  )
}

# Function to look at the distribution of different duration columns
prop_count <- function(df, x) {
  df <- df %>%
    group_by(!!as.name(x)) %>%
    summarize(freq = n()) %>%
    mutate(prop = (freq / sum(freq)) * 100) %>%
    arrange(desc(prop))
  return(df)
}

# selecting the columns, coalescing them, and then adding it back to the main df
coalesce_redundant <- function(df, word, var) {
  new_var <- select(df, contains(word))
  # using do.call to coalesce the entire subsetted data frame.
  var <- do.call(coalesce, new_var)
  df <- data.frame(var, df)
  return(df)
}

# Clean up function to aggregate columns:
fix_discrepancy <- function(df, word, var) {
  df_filtered <- select(df, matches(word))
  main_col <- do.call(coalesce, df_filtered)
  main_col <- as.data.frame(main_col)
  df_new <- data.frame(main_col, df)
  names(df_new)[1] <- var
  return(df_new)
}

# A function to check missing values in each column, and filter them out:
filter_missing <- function(df) {
  missing <- names(df) %>%
    map_dfr(~ tibble(variable = .x, missing = sum(is.na(df[[.x]])))) %>%
    mutate(prop_missing = missing / nrow(df) * 100) %>%
    filter(prop_missing > 95)
  df_filtered <- df %>%
    select(-contains(missing$variable)) 
  message(paste0("Deleted variables: ", missing$variable, "\n"))
  return(df_filtered)
}

# A function I picked up from r-bloggers
# https://www.r-bloggers.com/2011/11/
# outersect-the-opposite-of-rs-intersect-function/
outersect <- function(x, y) {
  sort(c(setdiff(x, y), setdiff(y, x)))
}

# A function to use to check if the merge loop is working: 
check_loop <- function(i) {  
  for (i in file_list[i]) {  
    out <- i %>%
      set_names(.) %>%
      map_dfr(
        ~ read.table(.x, sep = ",", header = TRUE, nrows = 10),
        .id = "history_file"
      ) %>%
      clean_names()
    assert_that(nrow(out)/10 == length(file_list[i]))
  }
}

# Define file directory ========================================================
# Need relative paths so not here::here
file_paths <- list(
  master_voter =
    here("data", "raw", "EX-003 Master Voter List"),
  hist =
    here("data", "raw", "EX-002 Voting History Files"),
  ballots =
    here("data", "raw", "CE-068_Voters_With_Ballots_List_Public"),
  returned =
    here("data", "raw", "CE-068c_Voters_With_Returned_Ballot_List_Public"),
  cured =
    here("data", "raw", "CE-077_Rejected_Cure"),
  undelivered =
    here("data", "raw", "CE-037_UndeliverableBallots")
)
