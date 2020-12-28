source("R/00_utilities.R")

# Data unzipping ===============================================================
list.files(
  file.path("data", "raw", "EX-003 Master Voter List"),
  pattern = ".zip", full.names = TRUE
) %>%
  map(
    ~ system(
      "cmd.exe", 
      input = paste0(
        "7z ", "x \"./", .x, "\" -aoa -o", "\"./",
        file.path("data", "raw", "EX-003 Master Voter List"), "\""
      )
    )
  )

# Data import ==================================================================
elect <- list(
  returned = file.path(
    "data", "raw", "CE-068c_Voters_With_Returned_Ballot_List_Public",
    "CE-068c_Voters_With_Returned_Ballot_List_Public_03Nov_600015773_null.txt"
  ),
  cured = file.path(
    "data", "raw", "CE-077_Rejected_Cure",
    "CE-077_Rejected_Cure_03Nov_600015773_20201111_115002.txt"
  ),
  undelivered = file.path(
    "data", "raw", "CE-037_UndeliverableBallots",
    "CE-037_UndeliverableBallots_03Nov_600015773_20201111_232502.txt"
  )
) %>% 
  map(
    ~ read.table(
      .x, header = TRUE, sep = "|", na.strings = "", colClasses = "character",
      fill = TRUE, stringsAsFactors = TRUE, comment.char = "",
      nrows = nrows
    ) %>%
      clean_names()
  )

master_list <- list.files(
  file.path("data", "raw", "EX-003 Master Voter List"),
  pattern = "Registered.*txt", full.names = TRUE
) %>%
  map(
    ~ read.table(
      .x, header = TRUE, sep = "|", na.strings = "", colClasses = "character",
      fill = TRUE, stringsAsFactors = TRUE, comment.char = "",
      nrows = nrows
    )
  ) %>%
  bind_rows(.id = "file") %>%
  clean_names()

