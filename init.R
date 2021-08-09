renv::init()
install.packages("devtools")
install.packages("remotes")
install.packages("colorspace")
library(remotes)
install_github(
  "sysilviakim/Kmisc", INSTALL_opts = c("--no-multiarch"), dependencies = TRUE
)
Kmisc::proj_skeleton()

# Library installation
install.packages("plyr")
install.packages("tidyverse")
install.packages("lubridate")

install.packages("here")
install.packages("assertthat")
install.packages("styler")
install.packages("janitor")
install.packages("fst")

install.packages("rlist")
install.packages("tidymodels")
install.packages("ranger")
install.packages("gbm")
install.packages("caret")
install.packages("e1071")
install.packages("MLmetrics")

renv::snapshot()