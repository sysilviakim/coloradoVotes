# Packages =====================================================================
library(tidyverse)
library(lubridate)
library(janitor)
# remotes::install_github(
#   "sysilviakim/Kmisc", INSTALL_opts = c("--no-multiarch")
# )
library(Kmisc) 

if (Sys.info()["sysname"] == "Windows") {
  nrows <- -1
} else {
  nrows <- 100
}

# Functions ====================================================================
