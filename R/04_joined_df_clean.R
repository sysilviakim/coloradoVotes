source(here::here("R", "utilities.R"))

# Import data, depending on the user ===========================================
if (nrows == -1) {
  df_raw <- df <- read_fst(here("data/tidy/joined_full.fst"))
} else {
  df_raw <- df <- read_fst(here("data/tidy/joined_sample_10k.fst"))
}

# Cleaning the data ============================================================

## County ----------------------------------------------------------------------
assert_that(length(setdiff(unique(df$county_code), NA)) == 64)
assert_that(length(setdiff(unique(df$county), NA)) == 64)
assert_that(length(setdiff(unique(df$county_cur), NA)) < 64) ## not every county
assert_that(length(setdiff(unique(df$county_rtn), NA)) == 64)
assert_that(length(setdiff(unique(df$county_blt), NA)) == 64)

## Removing some redundant variables -------------------------------------------
df <- df %>%
  select(-c(county_code, res_city, residential_city))

## Variables simple enough to be fixed using the fix_discrepancy function ------
x <- c(
  "first_name", "last_name", "yob", "gender", "party",
  "preference", "phone", "ballot_style", "vote_method", "county",
  "election_name"
)

# Looping the process
for (i in x) {
  df <- fix_discrepancy(df, i, str_c(i, "_main"))
}

## Combining residential_zip_code, and residential_zip_code_plus ---------------
## to make one zip with the code, and geographic segment
df <- df %>%
  mutate(
    residential_zip_main = case_when(
      is.na(residential_zip_plus) ~ residential_zip_code,
      !is.na(residential_zip_plus) ~ paste(
        residential_zip_code, residential_zip_plus,
        sep = "-"
      )
    )
  )

# Finally combining it with the master_vr res_zip column.
df <- df %>%
  mutate(residential_zip_main = coalesce(residential_zip_main, res_zip))

## If possible trying to get a full name instead of an initial -----------------
## if not defaulting to the general priority set in the beginning.
df <- df %>%
  mutate(
    middle_name_main = case_when(
      nchar(middle_name_rtn) < nchar(middle_name_blt) ~ middle_name_blt,
      is.na(middle_name_rtn) & !is.na(middle_name_blt) ~ middle_name_blt,
      TRUE ~ coalesce(middle_name_rtn, middle_name_blt, middle_name)
    )
  ) %>%
  select(-c(middle_name, middle_name_blt, middle_name_rtn))

# Pulling out the relevant columns
df_new <- df %>%
  select(contains("_main"), voter_id, file)

# Removing columns with too many missing values (>95% missing)
df_new <- filter_missing(df_new)

# Removing suffixes
colnames(df_new) <- gsub("_main", "", colnames(df_new))

# Adding an election_type (general) + election_date (11/03/2020) column
# to help order, and sort when merged.

df_new$election_type <- "general"
df_new$election_date <- mdy(11032020)

# Making sure some important columns are in place before saving it =============
# Abbreviations mixed with full
unique(df_new$party)

# recoding the abbreviations:
df_cleaned <- df_new %>%
  mutate(
    party = recode(
      party,
      dem = "democrat",
      rep = "republican",
      uaf = "unaffiliated",
      lbr = "libertarian",
      acn = "american constitution",
      grn = "green",
      uni = "unaffiliated",
      apv = "approval"
    )
  )

# Sanity checks ================================================================
prop(df_cleaned, "party")
prop(df_cleaned, "gender")
# three distinct values (mail, in person paper, or in person electronic)
prop(df_cleaned, "vote_method")
assert_that(length(unique(df_cleaned$county)) == 64)

# Wrangle NA values: recoding gender ===========================================
df_cleaned <- df_cleaned %>%
  mutate(gender = na_if(gender, "unknown"))

# Saving cleaned data ==========================================================
save(df_cleaned, file = here("data/tidy/df_joined_tidy.RData"))
