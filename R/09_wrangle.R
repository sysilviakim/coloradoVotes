source(here::here("R", "utilities.R"))
library(nnet)
library(caret)
fname <- here("data", "tidy", "multiclass.Rda")

# Import merged/wrangled data ==================================================
if (!file.exists(fname)) {
  if (nrows == 100) {
    load(here("data", "tidy", "sample", "merged_sample.RData"))
  } else {
    load(here("data", "tidy", "merged_full.RData"))
  }

  ## Voting mode labels and party labels ---------------------------------------
  df <- df %>%
    mutate(
      ## could do with some optimization; skip for now
      across(c(contains("20")), ~ case_when(.x > 2 ~ 2, TRUE ~ .x)),
      across(
        c(contains("20")), ~ factor(
          .x,
          levels = c(1, 2, 0),
          labels = c("Mail", "In person", "Not voted")
        )
      ),
      yob = as.numeric(yob),
      party_all = factor(
        party,
        levels = c("dem", "rep", "uaf", "lbr", "grn", "acn", "apv")
      ),
      party = case_when(
        party %in% "dem" | party %in% "democratic" ~ "dem",
        party %in% "rep" | party %in% "republican" ~ "rep",
        TRUE ~ "oth"
      ),
      party = factor(party, levels = c("dem", "rep", "oth"))
    )

  ## Registration date ---------------------------------------------------------
  df$registration_date <- mdy(df$registration_date)

  ## Age -----------------------------------------------------------------------
  df <- df %>%
    ## Given that the 2020 election is the focus
    mutate(age = 2020 - as.integer(yob)) %>%
    mutate(
      age_groups = cut(
        age,
        breaks = c(0, 25, 41, 57, 76, 120),
        labels = c(
          "Gen Z (18-24)", "Milennial (25-40)",
          "Gen X (41-56)", "Boomer (57-75)", "Silent (75+)"
        )
      )
    )

  ## Registration bins ---------------------------------------------------------
  df <- df %>%
    mutate(
      reg_gen2020 = as.numeric(mdy("11/03/2020") - mdy(registration_date)),
      reg_gen2016 = as.numeric(mdy("11/08/2016") - mdy(registration_date)),
      reg_gen2018 = as.numeric(mdy("11/06/2018") - mdy(registration_date)),
      reg_gen2014 = as.numeric(mdy("11/04/2014") - mdy(registration_date)),
      reg_pri2020 = as.numeric(mdy("06/30/2020") - mdy(registration_date)),
      reg_pri2016 = as.numeric(mdy("06/28/2016") - mdy(registration_date)),
      reg_pri2018 = as.numeric(mdy("06/26/2018") - mdy(registration_date)),
      reg_pri2014 = as.numeric(mdy("06/24/2014") - mdy(registration_date))
    ) %>%
    mutate(
      reg_bin = coalesce(
        reg_gen2020, reg_pri2020, reg_gen2018, reg_pri2018,
        reg_gen2016, reg_pri2016, reg_gen2014, reg_pri2014
      )
    ) %>%
    mutate(
      reg_bin = case_when(
        reg_bin == 0 ~ "EDR",
        reg_bin > 15 & reg_bin <= 30 ~ "30Days",
        reg_bin > 0 & reg_bin <= 15 ~ "15Days",
        TRUE ~ "30+"
      )
    )

  ## Urban-rural classification ------------------------------------------------
  ## https://web.archive.org/web/20211102151447/https://www.colorado.gov/pacific/sites/default/files/DC_STI_HIVPrev_Colorado-County-Designations.pdf
  ## Not dated nor archived properly now, so will not use
  ## Colorado Rural Health Center's 2018 County Designations
  ## http://coruralhealth.wpengine.netdna-cdn.com/wp-content/uploads/2013/10/2018-map.pdf
  urban <- tolower(c(
    "Adams", "Arapahoe", "Boulder", "Broomfield",
    "Clear Creek", "Denver", "Douglas", "El Paso", "Elbert",
    "Gilpin", "Jefferson", "Larimer", "Mesa",
    "Park", "Pueblo", "Teller", "Weld"
  ))

  frontier <- tolower(c(
    "Moffat", "Jackson", "Sedgwick", "Rio Blanco", "Washington", "Yuma",
    "Lincoln", "Kit Carson", "Cheyenne", "Kiowa", "Gunnison",
    "San Miguel", "Dolores", "San Juan", "Hinsdale", "Mineral",
    "Saguache", "Custer", "Bent",
    "Huerfano", "Costilla", "Las Animas", "Baca"
  ))

  df <- df %>%
    mutate(
      county_designation = case_when(
        county %in% urban ~ "urban",
        county %in% frontier ~ "frontier",
        TRUE ~ "rural"
      ),
      county_designation = factor(
        county_designation,
        levels = c("urban", "rural", "frontier")
      )
    )

  ## Imputing political leanings if missing ------------------------------------ 
  ## with 2018 primary vote history
  primary_hist_2018 <- read.table(
    here(
      "data/raw/EX-002 Voting History Files", "20180719",
      "EX-002_2018_Primary_Supplemental_Vote_History",
      "EX-002_2018_Primary_Supplemental_Vote_History.txt"
    ),
    header = TRUE, sep = "|"
  ) %>%
    clean_names() %>%
    select(voted_party, received_party_ballot, voter_id) %>%
    mutate(
      voter_id = as.character(voter_id),
      across(everything(), tolower)
    ) %>%
    mutate(
      voted_party = na_if(voted_party, ""),
      received_party_ballot = na_if(received_party_ballot, "")
    )

  df_temp <- left_join(df, primary_hist_2018, by = "voter_id")

  ## Create a column with leanings:
  ## Leanings are imputed from:
  ## - Party voted for (priority)
  ## - Party of the ballot they received (if party voted for is not available)
  df_test <- df_temp %>%
    mutate(
      party_test = case_when(
        party %in% "oth" & !is.na(voted_party) ~ voted_party,
        party %in% "oth" & is.na(voted_party) & !is.na(received_party_ballot) ~
          received_party_ballot,
        TRUE ~ party
      )
    )

  ggplot(df_test, aes(party)) +
    geom_bar()

  # Comparing now:
  # Prop table without imputing:
  table(df$party) / nrow(df)
  # dem       oth       rep
  # 0.3632265 0.2915832 0.3451904

  # Prop table after imputing:
  table(df_test$party_test) / nrow(df_test)
  # dem       oth       rep
  # 0.4218437 0.1913828 0.3867735
  # Seems to work!

  ## Add challenged/rejected column --------------------------------------------
  ## Based on the name and lack of a column that tells if they were cured or
  ## not, I am assuming this data just refers to ballots that were rejected;
  ## since the only information regarding rejections provided is a
  ## rejection reason, and there is no info on where they were cured or not.
  
  rejected_files <- list.files(
    path = here("data/raw/CE-077_Rejected_Cure/Archive"),
    pattern = "*.txt",
    full.names = TRUE
  )
  
  out <- vector("list", length(rejected_files))
  
  ## Import and clean at the same time
  for (i in 2:length(rejected_files)) {
    out[[i]] <- rejected_files[[i]] %>%
      map_dfr(
        ~ read.table(.x, sep = "|", header = TRUE, quote = "", fill = TRUE)
      ) %>%
      clean_names() %>%
      select(voter_id, reject_reason) %>%
      # Adding a column of 1s to later add 0s for voters who weren't rejected
      mutate(
        rejected = 1,
        reject_reason = tolower(reject_reason)
      )
  }
  
  ## Join again with this new information:
  df_joined <- left_join(df_temp, out %>% bind_rows(), by = "voter_id") %>%
    ## Replace NA with 0 (not challenged)
    mutate(rejected = replace_na(rejected, "0"))
  
  ## Switching -----------------------------------------------------------------
  df <- df %>%
    filter(
      ## Some pattern, either mail or in person, before 2020 cycle
      gen2018 != "Not voted" | pri2018 != "Not voted" | 
        gen2016 != "Not voted" | pri2016 != "Not voted" | 
        gen2014 != "Not voted" | pri2014 != "Not voted"
    ) %>%
    filter(
      ## Did vote in gen 2020
      gen2020 != "Not voted"
    ) %>%
    mutate(
      switcher = case_when(
        gen2020 == "In person" & (
          gen2018 == "Mail" | pri2018 == "Mail" | 
            gen2016 == "Mail" | pri2016 == "Mail" | 
            gen2014 == "Mail" | pri2014 == "Mail"
        ) ~ "Yes",
        TRUE ~ "No"
      )
    ) %>%
    filter(!(switcher == "No" & gen2020 == "In person")) %>%
    mutate(switcher = factor(switcher))
  
  ## While `df_joined` is the final product, it should match nrow with df
  assert_that(nrow(df) == nrow(df_joined))
  df <- df_joined
  
  if (nrows == 100) {
    save(df, file = gsub(".Rda", "_sample.Rda", fname))
  } else {
    save(df, file = fname)
  }
} else {
  load(fname)
}

# Create county-collapsed version ==============================================
temp <- table(df$county)
temp <- sort(temp)
temp

df <- df %>%
  rename(county_full = county) %>%
  mutate(
    county = case_when(
      county_full %in% names(temp)[which(temp < 20000)] ~ "others",
      TRUE ~ county_full
    )
  )

fname <- here("data", "tidy", "multiclass_county_collapsed.Rda")
if (nrows == 100) {
  save(df, file = gsub(".Rda", "_sample.Rda", fname))
} else {
  save(df, file = fname)
}
