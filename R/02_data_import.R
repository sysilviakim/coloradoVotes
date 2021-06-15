source(here::here("R", "utilities.R"))

# Data import/clean: election dataframes =======================================
elect_list <- within(file_paths, rm("master_voter")) %>%
  map(
    ~ read.delim(
      list.files(.x, pattern = ".txt", full.names = TRUE),
      sep = "|", na.strings = "", colClasses = "character",
      stringsAsFactors = TRUE, nrows = nrows, quote = ""
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
    # In the cured file, two instances of error
    # Voter ID 602568350: extra columns added (46 ---> 49) in the middle
    # Voter ID 602675030: extra line break  in street address
    # This would have been actually *better* if there were quotes, but...
    # It would be easier to fix the input data itself, but...
  )

# Fix anomaly in cured; may have to be adjusted by snapshot
# First error
temp <- elect_list$cured
idx <- which(temp$voter_id == "602568350")
temp[idx, which(colnames(temp) == "yob"):(ncol(temp) - 3)] <-
  temp[idx, which(colnames(temp) == "non_standard_res_address"):ncol(temp)]
temp[idx, which(colnames(temp) == "modified_date"):ncol(temp)] <-
  temp[idx + 1, seq(3)]
temp <- temp[-(idx + 1), ]

# Second error
idx <- which(temp$voter_id == "602675030")
temp[idx, which(colnames(temp) == "res_house_number"):ncol(temp)] <-
  temp[idx, 2:which(colnames(temp) == "sent_ballot_state")]
temp[idx, "non_standard_res_address"] <- paste(
  temp[idx, "non_standard_res_address"], temp[idx + 1, 1]
)
temp <- temp[-(idx + 1), ]
elect_list$cured <- temp

# Data import/clean: voter registration data ===================================
files <- list.files(path = here("data", "raw", "EX-003 Master Voter List",
                                "2020_General"), pattern = "*.txt",
                    full.names = TRUE)

# Using the same proccess used for vf: 
master_vr <- vector("list", length(files))

# Loop to merge
for (i in 1:length(files)) {
  master_vr[[i]] <- files[[i]] %>%
    set_names(.) %>%
    map_dfr(
      ~ read.table(.x, sep = ",", header = TRUE),
      .id = "gen2020_file"
    ) %>%
    clean_names()
}

# Bind 
master_vr <- list.rbind(master_vr) %>%
  mutate(across(.cols = everything(), tolower),
         across(.cols = everything(), as.character)) 

# master_vr <- list.files(
#   file.path("data", "raw", "EX-003 Master Voter List"),
#   pattern = "Registered.*txt", full.names = TRUE
# ) %>%
#   map(
#     ~ readr::read_csv(
#       .x,
#       na = "", col_types = cols(.default = "c"),
#       n_max = ifelse(nrows == -1, Inf, nrows),
#       locale = readr::locale(encoding = "UTF-8")
#       # If read.delim, quote = ""; otherwise, quote EOF error and misses lines
#       # That is to say, there are unclosed double quotes
#       # e.g., "State House 48","N"08468","Yes","Congressional 1",
#       # This is file 5, voter ID 601488340 (in fact... has more columns here)
#       # Half a record deleted? Residue from manual intervention?
#       # Not using read.delim because of extra comma not in header; see
#       # https://stackoverflow.com/questions/8854046/
#       # Why? Has data with a comma inside, while using commas as delimiters
#       # e.g., "Kim", "Silvia", "Kim, Silvia", ...
#       # Encoding issue as well: CEDEÃ`O TORRES or 18/2 RUTESHEIMER STRAÃYE
#       # Annnnnd there are line breaks in the middle of quotes
#       # e.g., Part 1 line 11824--11825 (voter ID 639278)
#       # "Rio Grande
#       # Swatch" ---> good thing is, readr::read_csv correctly recognizeds this
#       # as a single data row
#       # Poorly formatted file overall (facepalm)
#       # Approx. 500000 * 8 + (178493 - 1) for Nov 5, 2020 snapshot
#     )
#   ) %>%
#   bind_rows(.id = "file") %>%
#   clean_names()

# Was this data imported without error? ========================================
assert_that(unique(master_vr$residential_state) == "co")
assert_that(length(unique(master_vr$county)) == 64)
assert_that(all(unique(master_vr$status_code) %in% c("a", "i")))
assert_that(all(unique(master_vr$gender) %in% c("female", "male", "unknown")))
assert_that(all(unique(master_vr$permanent_mail_in_voter) %in% c("yes", "no")))
assert_that(
  all(unique(master_vr$congressional) %in% paste0("congressional ", seq(7)))
)
# Reran all the assert_thats after loading the new master_vr, they are all true. 

master_vr <- master_vr %>%
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

assert_that(all(unique(master_vr$id_required) %in% c("y", "n")))

# https://www.sos.state.co.us/pubs/elections/VoterRegNumbers/
# 2020/October/VoterCountsByStatus.pdf

# 3,767,236 active and 421,488 inactive on Nov 1, 2020
# 3,757,834 active and 456,548 inactive on Dec 1, 2020
# 3,758,963 active and 419,474 inactive per Nov 5, 2020 snapshot

# Save tidy format =============================================================
if (nrows == -1) {
  # save(master_vr, file = here("data/tidy/master_vr.RData"))
  write_fst(master_vr, here("data/tidy/master_vr.fst"))
  # Lists cannot be fst-exported
  save(elect_list, file = here("data/tidy/elect_list.RData"))
}
