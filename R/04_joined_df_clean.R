source("R/utilities.R")

#reading in the data 
load("data/tidy/master_vr.RData")
load("data/tidy/elect_list.RData")

#joining the data (taken from the 03_data_join.R file)
df <- master_vr %>%
  mutate_all(trimws) %>%
  full_join(., elect_list$ballots, suffix = c("", "_blt"), by = "voter_id") %>%
  full_join(., elect_list$returned, suffix = c("", "_rtn"), by = "voter_id") %>%
  full_join(., elect_list$cured, suffix = c("", "_cur"), by = "voter_id") %>%
  full_join(., elect_list$undelivered, suffix = c("", "_und"), by = "voter_id")

# Functions ====================================================================

# Clean up function to aggregate columns: 
fixDiscrepancy <- function(df, word, varName) {
  df_filtered <- select(df, matches(word))
  MainCol <- do.call(coalesce, df_filtered)
  MainCol <- as.data.frame(MainCol)
  df_new <- data.frame(MainCol, df)
  names(df_new)[1] <- varName
  return(df_new)
}

#A function to check missing values in each column, and filter them out: 
FilterMissing <- function(df) {
  map(df, ~sum(is.na(.))) %>% 
    as.data.frame() %>%
    gather(key = "variable", value = "missing") %>%
    mutate(prop_missing = (missing/nrow(df))*100) %>%
    filter(prop_missing > 95) -> missing
  df %>%
    select(-contains(missing$variable)) -> df_filtered
  return(df_filtered)
}

# This function is not my own, I picked it up from r-blogger
#(https://www.r-bloggers.com/2011/11/outersect-the-opposite-of-rs-intersect-function/) 
outersect <- function(x, y) {
  sort(c(setdiff(x, y),
         setdiff(y, x)))
}

#Cleaning the data =============================================================
#removing some redundant variables 
df %>%
  select(-c(county_code, res_city, residential_city)) -> df

#Fixing variables simple enough to be fixed using the fixDiscrepancy function: 
x <- c("first_name", "last_name", "yob", "gender", "party",
       "preference", "phone", "ballot_style", "vote_method", "county", 
       "election_name")
#Looping the process
for (i in x) {
  j <- str_c(i, "_main")
  df <- fixDiscrepancy(df, i, j)
}

#Combining residential_zip_code, and residential_zip_code_plus, to make one zip
#with the code, and geographic segment. 
df %>%
  mutate(residential_zip_main = case_when(is.na(residential_zip_plus) ~ 
                                            residential_zip_code,
                                          !is.na(residential_zip_plus) ~ 
                                            paste(residential_zip_code, 
                                                  residential_zip_plus, sep = "-")))  -> df

#Finally combining it with the master_vr res_zip column. 
df %>%
  mutate(residential_zip_main = coalesce(residential_zip_main, res_zip)) -> df


#If possible trying to get a full name instead of an initial, if not defaulting
#to the general priority set in the beginning. 
df %>%
  mutate(middle_name_main = case_when(
    nchar(middle_name_rtn) < nchar(middle_name_blt) ~ middle_name_blt,
    is.na(middle_name_rtn) & !is.na(middle_name_blt) ~ middle_name_blt,
    TRUE ~ coalesce(middle_name_rtn, middle_name_blt, middle_name))) %>% 
  select(-c(middle_name, middle_name_blt, middle_name_rtn)) -> df

#Pulling out the relevant columns
df %>% 
  select(contains("_main"), voter_id, file) -> df_new 
#Removing columns with too many missing values (>95% missing)
FilterMissing(df_new) -> df_new
#Removing suffixes
colnames(df_new) <- gsub("_main","",colnames(df_new))
#Final fix to ensure voter_id is in interger format for the merge
df_new %>%
  mutate(voter_id = as.integer(voter_id)) -> df_new
#adding an election_type (general) column, and election_date (11/03/2020) column
#to help order, and sort when merged. 
df_new$election_type <- "general"
df_new$election_date <- mdy(11032020)

#Making sure some important columns are in place before saving it ==============
unique(df_new$party) #there are abreviations 

#recoding the abbreviations: 
df_new %>% 
  mutate(party = recode(party, dem = "democrat",
                        rep = "republican",
                        uaf = "unaffiliated",
                        lbr = "libertarian",
                        acn = "american constitution",
                        grn = "green",
                        uni = "unaffiliated",
                        apv = "approval")) -> df_cleaned

unique(df_cleaned$gender) #coding unknown as NA as well. 
#recoding
df_cleaned %>%
  mutate(gender = na_if(gender, "unknown")) -> df_cleaned

unique(df_cleaned$county) #64 counties as required 

unique(df_cleaned$vote_method) #three distinct values (mail, in person paper, 
#or in person electronic)

#saving the set ================================================================
# Previously saved as: save(df_cleaned, file = "data/tidy/df_joined_cleaned.RData")
save(df_cleaned, file = "data/tidy/df_joined_tidy.RData")










