zz <- file("error.Rout", open = "wt")
sink(zz, type = "message")

library(fst, lib.loc = "/home/ab6668a/R/x86_64-pc-linux-gnu-library/4.0/")
library(here, lib.loc = "/home/ab6668a/R/x86_64-pc-linux-gnu-library/4.0/")

part_1 <- read_fst(here("/home/ab6668a/voter_history_long_v1.fst"))

part_2 <- read_fst(here("/home/ab6668a/voter_history_long_v2.fst"))

full_long <- rbind(part_1, part_2)

write_fst(full_long, "voter_history_long_full_hpc.fst")

closeAllConnections()