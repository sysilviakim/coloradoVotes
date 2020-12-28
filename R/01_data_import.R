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

# Data import ==================================================================
elect <- within(file_paths, rm("master_voter")) %>% 
  map(
    ~ read.delim(
      list.files(.x, pattern = ".txt", full.names = TRUE), 
      sep = "|", na.strings = "", colClasses = "character",
      stringsAsFactors = TRUE, nrows = nrows
    ) %>%
      clean_names()
  )

master_list <- list.files(
  file.path("data", "raw", "EX-003 Master Voter List"),
  pattern = "Registered.*txt", full.names = TRUE
) %>%
  map(
    ~ read.delim(
      .x, sep = "|", na.strings = "", colClasses = "character",
      stringsAsFactors = TRUE, nrows = nrows
    )
  ) %>%
  bind_rows(.id = "file") %>%
  clean_names()

