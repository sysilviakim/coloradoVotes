source("R/00_utilities.R")

# Data unzipping ===============================================================
file_paths <- list(
  master_voter = file.path("data", "raw", "EX-003 Master Voter List"),
  returned =
    file.path("data", "raw", "CE-068c_Voters_With_Returned_Ballot_List_Public"),
  cured = file.path("data", "raw", "CE-077_Rejected_Cure"),
  undelivered = file.path("data", "raw", "CE-037_UndeliverableBallots")
)

file_paths %>%
  map(
    ~ {
      out <- tibble(
        path = .x,
        files = list.files(.x, pattern = ".zip", full.names = TRUE)
      )
      if (grepl("Cure|Undeliverable", .x)) {
        out %>%
          filter(files %in% file_recent(out, pattern = "_([0-9]{1,8})_"))
      } else {
        out %>%
          filter(files %in% file_recent(out, pattern = "_([0-9]+).zip$"))
      }
    }
  ) %>%
  bind_rows() %>%
  split(., seq(nrow(.))) %>%
  map(
    ~ system(
      "cmd.exe",
      input = paste0(
        "7z ", "x \"./", .x$files, "\" -aoa -o", "\"./", .x$path, "\""
      )
    )
  )

# Data import/clean ============================================================
elect_list <- within(file_paths, rm("master_voter")) %>%
  map(
    ~ read.delim(
      list.files(.x, pattern = ".txt", full.names = TRUE),
      sep = "|", na.strings = "", colClasses = "character",
      stringsAsFactors = TRUE, nrows = nrows
    ) %>%
      clean_names() %>%
      mutate_at(
        intersect(
          names(.),
          c(
            "last_name", "first_name", "middle_name", "voter_name",
            "mail_addr1", "mail_addr2", "mail_addr3"
          )
        ),
        ~ stringi::stri_trans_general(., "latin-ascii")
      ) %>%
      mutate_if(is.character, ~ tolower(gsub("\"", "", .)))
  )

master_vr <- list.files(
  file.path("data", "raw", "EX-003 Master Voter List"),
  pattern = "Registered.*txt", full.names = TRUE
) %>%
  map(
    ~ readr::read_csv(
      .x,
      na = "", col_types = cols(.default = "c"),
      n_max = ifelse(nrows == -1, Inf, nrows),
      locale = readr::locale(encoding = "UTF-8")
      # If read.delim, quote = ""; otherwise, quote EOF error and misses lines
      # That is to say, there are unclosed double quotes
      # e.g., "State House 48","N"08468","Yes","Congressional 1",
      # This is file 5, voter ID 601488340 (in fact... has more columns here)
      # Half a record deleted? Residue from manual intervention?
      # Not using read.delim because of extra comma not in header; see
      # https://stackoverflow.com/questions/8854046/
      # Why? Has data with a comma inside, while using commas as delimiters
      # e.g., "Kim", "Silvia", "Kim, Silvia", ...
      # Encoding issue as well: CEDEÃ`O TORRES or 18/2 RUTESHEIMER STRAÃYE
      # Annnnnd there are line breaks in the middle of quotes
      # e.g., Part 1 line 11824--11825 (voter ID 639278)
      # "Rio Grande
      # Swatch" ---> good thing is, readr::read_csv correctly recognizeds this
      # as a single data row
      # Poorly formatted file overall (facepalm)
      # Approx. 500000 * 8 + (178493 - 1) for Nov 5, 2020 snapshot
    )
  ) %>%
  bind_rows(.id = "file") %>%
  clean_names()

# Was this data imported without error? ========================================
assert_that(unique(master_vr$residential_state) == "CO")
assert_that(length(unique(master_vr$county)) == 64)
assert_that(all(unique(master_vr$status_code) %in% c("A", "I")))
assert_that(all(unique(master_vr$gender) %in% c("Female", "Male", "Unknown")))
assert_that(all(unique(master_vr$permanent_mail_in_voter) %in% c("Yes", "No")))
assert_that(
  all(unique(master_vr$congressional) %in% paste0("Congressional ", seq(7)))
)

x <- master_vr %>%
  mutate(
    id_required = if_else(id_required == "N\"08468", "N", id_required)
  ) %>%
  mutate_at(
    c(
      "last_name", "first_name", "middle_name", "voter_name",
      "mail_addr1", "mail_addr2", "mail_addr3"
    ),
    ~ stringi::stri_trans_general(., "latin-ascii")
  ) %>%
  mutate_if(is.character, ~ tolower(gsub("\"", "", .)))

assert_that(all(unique(master_vr$id_required) %in% c("Y", "N")))

# https://www.sos.state.co.us/pubs/elections/VoterRegNumbers/2020/October/VoterCountsByStatus.pdf
# 3,767,236 active and 421,488 inactive on Nov 1, 2020
# 3,757,834 active and 456,548 inactive on Dec 1, 2020
# 3,758,963 active and 419,474 inactive per Nov 5, 2020 snapshot

# Crosswalk between files ======================================================
names(elect_list$returned)
names(elect_list$cured)
names(elect_list$undelivered)
names(master_vr)

intersect(names(master_vr), names(elect_list$returned))
#  [1] "voter_id"    "county"      "last_name"   "first_name"  "middle_name"
#  [6] "name_suffix" "gender"      "precinct"    "split"       "party"      
# [11] "preference"

nrow(inner_join(master_vr, elect_list$returned, by = "voter_id"))
# 3286791
nrow(inner_join(master_vr, elect_list$returned))
# 3265620

nrow(inner_join(master_vr, elect_list$cured, by = "voter_id"))
# 29727
nrow(inner_join(master_vr, elect_list$cured))
# 29484

nrow(inner_join(master_vr, elect_list$undelivered, by = "voter_id"))
# 48328
nrow(inner_join(master_vr, elect_list$undelivered))
# 0

# Save tidy format =============================================================
if (nrows == -1) {
  save(master_vr, file = "data/tidy/master_vr.RData")
  save(elect_list, file = "data/tidy/elect_list.RData")
}