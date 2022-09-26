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
      )
    )

  df$yob <- as.numeric(df$yob)
  df$party_all <- factor(
    df$party,
    levels = c("dem", "rep", "uaf", "lbr", "grn", "acn", "apv")
  )

  df$party <- case_when(
    df$party == "dem" | df$party == "democratic" ~ "dem",
    df$party == "rep" | df$party == "republican" ~ "rep",
    TRUE ~ "oth"
  )

  df$party <- factor(df$party, levels = c("dem", "rep", "oth"))

  ## Registration date ---------------------------------------------------------
  df$registration_date <- mdy(df$registration_date)

  ## Age -----------------------------------------------------------------------
  ## Given that the 2020 election is the focus
  df$age <- 2020 - as.integer(df$yob)
  df$age_groups <- cut(
    df$age,
    breaks = c(0, 25, 41, 57, 76, 120),
    labels = c(
      "Gen Z (18-24)", "Milennial (25-40)",
      "Gen X (41-56)", "Boomer (57-75)", "Silent (75+)"
    )
  )

  ## Registration bins ---------------------------------------------------------
  df$reg_gen2020 <- as.numeric(mdy("11/03/2020") - df$registration_date)
  df$reg_gen2016 <- as.numeric(mdy("11/08/2016") - df$registration_date)
  df$reg_gen2018 <- as.numeric(mdy("11/06/2018") - df$registration_date)
  df$reg_gen2014 <- as.numeric(mdy("11/04/2014") - df$registration_date)
  df$reg_pri2020 <- as.numeric(mdy("06/30/2020") - df$registration_date)
  df$reg_pri2016 <- as.numeric(mdy("06/28/2016") - df$registration_date)
  df$reg_pri2018 <- as.numeric(mdy("06/26/2018") - df$registration_date)
  df$reg_pri2014 <- as.numeric(mdy("06/24/2014") - df$registration_date)

  df$reg_bin <- coalesce(
    df$reg_gen2020, df$reg_pri2020, df$reg_gen2018, df$reg_pri2018,
    df$reg_gen2016, df$reg_pri2016, df$reg_gen2014, df$reg_pri2014
  )

  df$reg_bin <- case_when(
    df$reg_bin == 0 ~ "EDR",
    df$reg_bin > 15 & df$reg_bin <= 30 ~ "30Days",
    df$reg_bin > 0 & df$reg_bin <= 15 ~ "15Days",
    TRUE ~ "30+"
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

  df$county_designation <- case_when(
    df$county %in% urban ~ "urban",
    df$county %in% frontier ~ "frontier",
    TRUE ~ "rural"
  )

  df$county_designation <- factor(
    df$county_designation,
    levels = c("urban", "rural", "frontier")
  )

  ## Imputing political leanings if missing ------------------------------------
  ## with 2018 primary vote history
  primary_hist_2018 <- read.table(
    here(
      "data/raw/EX-002 Voting History Files", "20180719",
      "EX-002_2018_Primary_Supplemental_Vote_History",
      "EX-002_2018_Primary_Supplemental_Vote_History.txt"
    ),
    header = TRUE, sep = "|",
    col.names = read_excel(
      here(
        "data/raw/EX-002 Voting History Files", "20180719",
        "EX-002_2018_Primary_Supplemental_Vote_History",
        "EX-002_2018_Primary_Supplemental_Vote_History_Header_Row.xlsx"
      )
    ) %>%
      clean_names() %>%
      names()
  ) %>%
    select(voter_id, voted_party, received_party_ballot) %>%
    mutate(
      voter_id = as.character(voter_id),
      across(everything(), tolower)
    ) %>%
    mutate(
      voted_party = na_if(voted_party, ""),
      received_party_ballot = na_if(received_party_ballot, "")
    )

  df_temp <- left_join(df, primary_hist_2018, by = "voter_id")
  assert_that(nrow(df) == nrow(df_temp))
  prop(primary_hist_2018, "voted_party")
  #  dem  rep <NA>
  # 14.4  8.6 77.0
  prop(primary_hist_2018, "received_party_ballot")
  # dem  not tracked         rep
  # 55.4         0.0        44.6

  ## Create a column with leanings, imputed from
  ## - Party voted for (priority)
  ## - Party of the ballot they received (if party voted for is not available)
  df_temp$party <- as.character(df_temp$party)
  df_test <- df_temp
  df_test$party_test <- case_when(
    df_test$party == "oth" & !is.na(df_test$voted_party) ~ df_test$voted_party,
    df_test$party == "oth" & is.na(df_test$voted_party) &
      !is.na(df_test$received_party_ballot) &
      df_test$received_party_ballot != "not tracked" ~
      df_test$received_party_ballot,
    TRUE ~ df_test$party
  )
  df_test$party_test <-
    factor(df_test$party_test, levels = c("dem", "rep", "oth"))

  # Comparing. Without imputing (slightly diff from recorded number):
  prop(df, "party")
  #  dem  rep  oth
  # 30.8 28.4 40.8

  # Prop table after imputing:
  prop(df_test, "party_test")
  #  dem  rep  oth
  # 35.6 31.5 32.8
  # Seems to work!

  ## Add challenged/rejected column --------------------------------------------
  ## Based on the name and lack of a column that tells if they were cured or
  ## not, I am assuming this data just refers to ballots that were rejected;
  ## since the only information regarding rejections provided is a
  ## rejection reason, and there is no info on where they were cured or not.

  rejected_files <- list.files(
    path = here("data/raw/CE-077_Rejected_Cure/Archive"),
    pattern = "Rejected(.*)txt",
    full.names = TRUE
  )

  out <- vector("list", length(rejected_files))

  ## Import and clean at the same time
  for (i in seq(length(rejected_files))) {
    out[[i]] <- rejected_files[[i]] %>%
      map_dfr(
        ~ read.table(
          .x,
          sep = "|", header = TRUE, quote = "", fill = TRUE,
          colClasses = "character"
        )
      ) %>%
      clean_names() %>%
      select(voter_id, reject_reason) %>%
      # Adding a column of 1s to later add 0s for voters who weren't rejected
      mutate(
        rejected = 1,
        reject_reason = tolower(reject_reason)
      )
  }

  rejected_df <- out %>%
    bind_rows() %>%
    dedup() %>%
    group_by(voter_id) %>%
    arrange(reject_reason) %>%
    slice(n())
  assert_that(!any(duplicated(rejected_df$voter_id)))

  ## Join again with this new information:
  df_joined <- left_join(df_temp, rejected_df, by = "voter_id")
  
  ## Replace NA with 0 (not challenged)
  df_joined$rejected <- replace_na(df_joined$rejected, 0)
  assert_that(nrow(df_temp) == nrow(df_joined))

  df <- df_joined

  if (nrows == 100) {
    save(df, file = gsub(".Rda", "_sample.Rda", fname))
  } else {
    save(df, file = fname)
  }
} else {
  load(fname)
}

# Switcher data ================================================================
fname <- here("data", "tidy", "switcher.Rda")
if (!file.exists(fname)) {
  df_switch <- df %>%
    filter(
      ## Some pattern, either mail or in person, before 2020 cycle
      gen2018 != "Not voted" | pri2018 != "Not voted" |
        gen2016 != "Not voted" | pri2016 != "Not voted" |
        gen2014 != "Not voted" | pri2014 != "Not voted"
    ) %>%
    filter(
      ## Did vote in gen 2020
      gen2020 != "Not voted"
    ) 
  
  df_switch$switcher <- case_when(
    df_switch$gen2020 == "In person" & 
      (
        df_switch$gen2018 == "Mail" | df_switch$pri2018 == "Mail" |
        df_switch$gen2016 == "Mail" | df_switch$pri2016 == "Mail" |
        df_switch$gen2014 == "Mail" | df_switch$pri2014 == "Mail"
      ) ~ "Yes",
      TRUE ~ "No"
  )
  df_switch$switcher <- factor(df_switch$switcher)
  
  df_switch <- df_switch %>%
    filter(!(switcher == "No" & gen2020 == "In person"))
  if (nrows == 100) {
    save(df_switch, file = gsub(".Rda", "_sample.Rda", fname))
  } else {
    save(df_switch, file = fname)
  }
} else {
  load(fname)
}

assert_that(nrow(df_switch) < nrow(df))

# Create county-collapsed version ==============================================
temp <- table(df$county)
temp <- sort(temp)
temp
#   san juan    hinsdale     mineral       kiowa     jackson 
#        693         744         872        1004        1063 
#   cheyenne     dolores    sedgwick     crowley        baca 
#       1269        1656        1691        2022        2557 
#   costilla        bent    phillips     lincoln  washington 
#       2600        2608        2816        3005        3313 
#   saguache      custer  rio blanco  kit carson       ouray 
#       4009        4133        4263        4407        4464 
#       lake      gilpin     conejos    huerfano        yuma 
#       4815        4898        4991        5088        5556 
# san miguel     prowers  rio grande clear creek      moffat 
#       5946        6337        7321        7521        8173 
#    alamosa  las animas   archuleta       otero       grand 
#       9081        9419       10574       10922       11407 
#      logan    gunnison      pitkin        park     chaffee 
#      11789       12931       13607       14334       15483 
#     morgan   montezuma       routt      teller      elbert 
#      15733       17870       19119       19613       21055 
#      delta      summit    montrose     fremont       eagle 
#      21600       21713       27713       29203       33552 
#   garfield    la plata  broomfield      pueblo        mesa 
#      35423       42059       52012      101543      102681 
#       weld     boulder     larimer     douglas       adams 
#     190876      234086      254546      258131      271178 
#   arapahoe   jefferson     el paso      denver 
#     400709      419001      440556      453669 

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
