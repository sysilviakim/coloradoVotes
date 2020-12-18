# Packages =====================================================================
library(tidyverse)
library(lubridate)
library(janitor)

if (Sys.info()["sysname"] == "Windows") {
  nrows <- -1
} else {
  nrows <- 100
}