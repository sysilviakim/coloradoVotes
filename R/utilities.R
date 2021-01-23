# Packages =====================================================================
library(plyr)
library(tidyverse)
library(lubridate)
library(janitor)
library(assertthat)
library(here)
library(styler)
library(Kmisc) 

# Other setups =================================================================
if (Sys.info()["sysname"] == "Windows") {
  nrows <- -1
} else {
  nrows <- 100
}

# Functions ====================================================================
nchar_longest <- function(v) {
  v[order(nchar(v), v, decreasing = TRUE)][1]
}

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

# renv::snapshot()