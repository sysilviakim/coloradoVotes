source(here::here("R", "utilities.R"))

# Define file directory ========================================================
# Need relative paths so not here::here
file_paths <- list(
  master_voter =
    file.path("data", "raw", "EX-003 Master Voter List"),
  hist =
    file.path("data", "raw", "EX-002 Voting History Files"),
  ballots =
    file.path("data", "raw", "CE-068_Voters_With_Ballots_List_Public"),
  returned =
    file.path("data", "raw", "CE-068c_Voters_With_Returned_Ballot_List_Public"),
  cured =
    file.path("data", "raw", "CE-077_Rejected_Cure"),
  undelivered =
    file.path("data", "raw", "CE-037_UndeliverableBallots")
)

# Valid file paths? Verify =====================================================
file_paths %>%
  map(~ dir.exists(.x)) %>%
  unlist() %>%
  all() %>%
  assert_that()

# Unzip all .zip (Windows ver.) ================================================
file_list <- file_paths %>% 
  map(list.dirs) %>% 
  unlist() %>%
  map(
    ~ tibble(
      path = .x,
      files = list.files(.x, pattern = ".zip|.gz", full.names = TRUE)
    )
    # if (grepl("Cure|Undeliverable", .x)) {
    #   out %>%
    #     filter(files %in% file_recent(out, pattern = "_([0-9]{1,8})_"))
    # } else {
    #   out %>%
    #     filter(files %in% file_recent(out, pattern = "_([0-9]+).zip$"))
    # }
  ) %>%
  bind_rows()

if (Sys.info()["sysname"] == "Windows") {
  file_list %>%
    split(., seq(nrow(.))) %>%
    map(
      ~ system(
        "cmd.exe",
        input = paste0(
          "7z ", "x \"", .x$files, "\" -aoa -o", "\"./", .x$path, "\""
        )
      )
    )
}
