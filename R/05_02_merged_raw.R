source(here::here("R", "utilities.R"))
load(here("data/tidy/df_joined_tidy.RData"))

# First find all directories + list files in each directory with pattern =======
file_list <- list.dirs(
  here("data", "raw", "EX-002 Voting History Files"),
  recursive = FALSE
) %>%
  set_names(
    .,
    gsub(paste0(here("data/raw/EX-002 Voting History Files"), "|/"), "", .)
  ) %>%
  imap(
    ~ list.files(.x, pattern = ".txt$", full.names = TRUE, recursive = T) %>%
      grep(., pattern = "*Details*", invert = TRUE, value = TRUE) %>%
      grep(
        .,
        pattern = paste0(
          "data/raw/EX-002 Voting History Files/", "20180719/",
          "EX-002_2018_Primary_Supplemental_Vote_History/",
          "EX-002_2018_Primary_Supplemental_Vote_History.txt"
        ), invert = TRUE, value = TRUE
      ) %>%
      grep(., pattern = "*Parameters*", invert = TRUE, value = TRUE)
  )

save(file_list, file = here("data", "tidy", "file_list_history.Rda"))

# Check components of `file_list` list =========================================
names(file_list)
file_list %>% map_dbl(length)
file_list %>% map_chr(class)

files_df <- file_list %>%
  imap_dfr(~ tibble(date = .y, file_list = .x))

# Binding all of the file_list together to create one df to then clean =========
# First using check_loop to make sure all the files will load properly into the
# list. 
i <- c(1:10)
map(i, check_loop)
# No false values were returned from assert that. It seems like the loop works. 

out <- vector("list", length(file_list))

for (i in 1:length(file_list)) {
  out[[i]] <- file_list[[i]] %>%
    set_names(.) %>%
    map_dfr(
      ~ read.table(.x, sep = ",", header = TRUE),
      .id = "history_file"
    ) %>%
    clean_names()
}

save(out, file = here("data", "tidy", "full_history_list.fst"))

out <- list.rbind(out)

# The primary file was the only one in a different format, so loading that in
# separately
primary_history <- read.table(
  paste0(
    "data/raw/EX-002 Voting History Files/", "20180719/",
    "EX-002_2018_Primary_Supplemental_Vote_History/",
    "EX-002_2018_Primary_Supplemental_Vote_History.txt"
  ),
  header = TRUE,
  sep = "|",
  nrows = 10000
) %>%
  clean_names()

names(out)
names(primary_history)

# Matching the names
primary_history <- primary_history %>%
  rename(
    voting_method = vote_method,
    party = voter_party,
    county_name = county
  ) %>%
  select(-c(
    voter_preference, voted_party,
    received_party_ballot
  )) 

primary_history$history_file <- paste0(
  "data/raw/EX-002 Voting History Files/", "20180719/",
  "EX-002_2018_Primary_Supplemental_Vote_History/",
  "EX-002_2018_Primary_Supplemental_Vote_History.txt"
)

# Binding
out <- rbind(out, primary_history)

save(out, file = here("data", "tidy", "full_history_long.fst"))