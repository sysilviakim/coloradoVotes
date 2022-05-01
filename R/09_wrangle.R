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
        party == "dem" | party == "democratic" ~ "dem",
        party == "rep" | party == "republican" ~ "rep",
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
  df$reg_gen2020 <- as.numeric(mdy("11/03/2020") - df$registration_date)
  df$reg_gen2016 <- as.numeric(mdy("11/08/2016") - df$registration_date)
  df$reg_gen2018 <- as.numeric(mdy("11/06/2018") - df$registration_date)
  df$reg_gen2014 <- as.numeric(mdy("11/04/2014") - df$registration_date)
  df$reg_pri2020 <- as.numeric(mdy("06/30/2020") - df$registration_date)
  df$reg_pri2016 <- as.numeric(mdy("06/28/2016") - df$registration_date)
  df$reg_pri2018 <- as.numeric(mdy("06/26/2018") - df$registration_date)
  df$reg_pri2014 <- as.numeric(mdy("06/24/2014") - df$registration_date)
  
  df <- df %>%
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
  df_test <- df_temp %>%
    mutate(
      party_test = case_when(
        party == "oth" & !is.na(voted_party) ~ voted_party,
        party == "oth" & is.na(voted_party) & 
          !is.na(received_party_ballot) & 
          received_party_ballot != "not tracked" ~
          received_party_ballot,
        TRUE ~ party
      ),
      party_test = factor(party_test, levels = c("dem", "rep", "oth"))
    )

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
          .x, sep = "|", header = TRUE, quote = "", fill = TRUE,
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
  df_joined <- left_join(df_temp, rejected_df, by = "voter_id") %>%
    ## Replace NA with 0 (not challenged)
    mutate(rejected = replace_na(rejected, 0))
  assert_that(nrow(df_temp) == nrow(df_joined))
  
  ## Switching -----------------------------------------------------------------
  df_joined <- df_joined %>%
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
# san juan    hinsdale     mineral     jackson       kiowa    cheyenne     dolores    sedgwick     crowley    costilla 
#      471         561         622         781         805        1029        1240        1251        1476        1771 
#     bent        baca    phillips     lincoln  washington    saguache        lake      custer  rio blanco  kit carson 
#     1888        1975        2206        2263        2630        2805        3018        3071        3182        3354 
#    ouray      gilpin    huerfano     conejos  san miguel        yuma     prowers  rio grande clear creek      moffat 
#     3386        3538        3700        3796        4114        4421        4629        5300        5558        5749 
#  alamosa  las animas   archuleta       otero       grand       logan    gunnison      pitkin        park      morgan 
#     6444        6449        7261        8129        8326        9014        9201        9805       10337       11333 
#  chaffee   montezuma      teller       routt      summit      elbert       delta    montrose     fremont       eagle 
#    11700       12572       13532       14088       14604       16206       16369       20745       21086       23833 
# garfield    la plata  broomfield      pueblo        mesa        weld     boulder     larimer       adams     douglas 
#    25522       28903       37275       73393       74275      136376      169861      183106      186786      190208 
# arapahoe     el paso      denver   jefferson 
#   283724      293444      306108      317684 

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
