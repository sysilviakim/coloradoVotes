source(here::here("R", "utilities.R"))

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
  map_dfr(
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
  )

save(file_list, file = here("data", "tidy", "file_list.Rda"))

# Unzip all .zip (Windows ver.) ================================================
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
