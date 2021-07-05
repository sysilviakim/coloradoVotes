source(here::here("R", "utilities.R"))
load(here("data/tidy/df_joined_tidy.RData"))

# First find all directories + list files in each directory with pattern =======
## Intentionally not using here package
file_list <- list.dirs(
  "data/raw/EX-002 Voting History Files",
  recursive = FALSE
) %>%
  set_names(
    ., gsub(paste0("data/raw/EX-002 Voting History Files", "|/"), "", .)
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
# First using check_loop to make sure all files will load properly into list
map(seq(10), check_loop)
# No false values were returned from assert that. It seems like the loop works.

# Import history file in a loop ================================================
out <- vector("list", length(file_list))
for (i in 1:length(file_list)) {
  out[[i]] <- file_list[[i]] %>%
    set_names(.) %>%
    map_dfr(
      ~ read.table(.x, sep = ",", header = TRUE),
      .id = "history_file"
    ) %>%
    clean_names()
  message(paste0("File import complete for ", names(file_list)[i], "."))
}
out <- list.rbind(out)

# The primary file was the only one with a different set of variables, so
# loading that in separately to format before merging
primary_history <- read.table(
  here(
    "data/raw/EX-002 Voting History Files", "20180719",
    "EX-002_2018_Primary_Supplemental_Vote_History",
    "EX-002_2018_Primary_Supplemental_Vote_History.txt"
  ),
  header = TRUE,
  sep = "|"
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
  select(-voter_preference, -voted_party, -received_party_ballot)

primary_history$history_file <- here(
  "data/raw/EX-002 Voting History Files", "20180719",
  "EX-002_2018_Primary_Supplemental_Vote_History",
  "EX-002_2018_Primary_Supplemental_Vote_History.txt"
)

# Binding
out <- rbind(out, primary_history)

# Initial check to make sure gen2020 voter number is reasonable: 
out %>%
  mutate(election_date = mdy(election_date)) %>%
  mutate(election = str_c(election_type, year(election_date))) %>%
  filter(election %in% "General2020") %>% 
  nrow()

## [1] 3280141; it is a little over 10k off, but seems pretty close. 

set.seed(123)
out_sample <- out %>% 
  sample_n(10000)
save(out_sample, file = here("data", "tidy", "full_history_long_sample.RData"))

write_fst(out, here("data", "tidy", "full_history_long.fst"))
