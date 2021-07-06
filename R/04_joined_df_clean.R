source(here::here("R", "utilities.R"))

# Import data, depending on the user ===========================================
if (nrows == -1) {
  df_raw <- df <- read_fst(here("data/tidy/joined_full.fst"))
} else {
  df_raw <- df <- read_fst(here("data/tidy/joined_sample_10k.fst"))
}

# Cleaning the data ============================================================
# [CHECK EACH VARIABLE HERE]
temp <- filter_missing(df_raw)

## County ----------------------------------------------------------------------
assert_that(length(setdiff(unique(df$county_code), NA)) == 64)
assert_that(length(setdiff(unique(df$county), NA)) == 64)
assert_that(length(setdiff(unique(df$county_cur), NA)) < 64) ## not every county
assert_that(length(setdiff(unique(df$county_rtn), NA)) == 64)
assert_that(length(setdiff(unique(df$county_blt), NA)) == 64)

## Removing some redundant variables -------------------------------------------
# Each county code corresponds to a unique county. There is already a county
# Column, so it might be redundant to have both.

df <- df %>%
  select(-c(county_code))

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

# Some checks to make sure the main columns are filled properly ================
df %>%
  select(contains("main")) %>%
  map(~ sum(is.na(.))) # It is concerning that county has missing values.

# Table to make sure we can impute from an alternate column.
table(is.na(df$election_name_main), is.na(df$county_main)) # election_name has
# no missing values when county has missing values.

# Fix:
df <- df %>%
  mutate(
    county_main = case_when(
      is.na(county_main) ~ word(election_name_main, 2),
      TRUE ~ county_main
    )
  )

# Making sure it worked:
sum(is.na(df$county_main)) # Yes.


## Combining residential_zip_code, and residential_zip_code_plus ---------------
## to make one zip with the code, and geographic segment

# First checking the variable options for the aggregation:
df %>%
  select(contains("zip")) %>%
  names()

# Using res_zip, and residential_zip_code,
df <- df %>%
  mutate(
    residential_zip_main = case_when(
      !is.na(residential_zip_plus) & is.na(res_zip) &
        !is.na(residential_zip_code) ~ paste(
        residential_zip_code, residential_zip_plus,
        sep = "-"
      ),
      !is.na(residential_zip_plus) & is.na(residential_zip_code) &
        !is.na(res_zip) ~ paste(
        res_zip, residential_zip_plus,
        sep = "-"
      ),
      TRUE ~ coalesce(res_zip, residential_zip_code)
    )
  )

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

## In person vote date is an important variable, with a lot of missing values,
## attempting to fix it.
df <- df %>%
  mutate(in_person_vote_date_main = coalesce(
    in_person_vote_date,
    in_person_vote_date_rtn
  ))

sum(!is.na(df$in_person_vote_date_main))
# [1] 197945
# Even after the coalesce, there aren't many. However, keeping the variable,
# in case of future need.

# Pulling out the relevant columns
df_new <- df %>%
  select(contains("_main"), voter_id, file)

# Removing columns with too many missing values (>95% missing)
# df_new <- filter_missing(df_new)

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
      dem = "democratic",
      rep = "republican",
      uaf = "unaffiliated",
      lbr = "libertarian",
      acn = "american constitution",
      grn = "green",
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

# Saving cleaned data (16 columns) =============================================
save(df_cleaned, file = here("data/tidy/df_joined_tidy.RData"))
