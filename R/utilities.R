# Packages =====================================================================
library(plyr)
library(tidyverse)
library(lubridate)
library(readxl)
library(janitor)
library(assertthat)
library(here)
library(fst)
library(styler)
library(Kmisc)
library(rlist)
library(tidymodels)
library(xtable)
library(caret)
library(ranger)
library(MLmetrics)
library(gbm)
library(fontcm)

# Other setups =================================================================
if (Sys.info()["sysname"] == "Windows") {
  # nrows <- 100
  nrows <- -1
} else {
  nrows <- 100
}
options(scipen = 999)

# Functions ====================================================================

# Find the element with longest nchar
nchar_longest <- function(v) {
  v[order(nchar(v), v, decreasing = TRUE)][1]
}

# Negate %in%
`%notin%` <- Negate(`%in%`)

# Function to find values that don't match
redundant <- function(df, x, y, vid = "voter_id") {
  out <- df %>%
    select(!!as.name(vid), !!as.name(x), !!as.name(y)) %>%
    filter(
      !is.na(!!as.name(x)) & is.na(!!as.name(y)) |
        !is.na(!!as.name(y)) & is.na(!!as.name(x)) |
        !!as.name(x) != !!as.name(y)
    ) %>%
    mutate(across(everything(), ~ gsub("[^[:alnum:]]", "", .x))) %>%
    rowwise() %>%
    filter(
      !is.na(!!as.name(x)) & is.na(!!as.name(y)) |
        !is.na(!!as.name(y)) & is.na(!!as.name(x)) |
        !grepl(!!as.name(x), !!as.name(y)) &
          !grepl(!!as.name(y), !!as.name(x))
    )
  return(
    out %>%
      select(!!as.name(vid)) %>%
      left_join(., df %>% select(!!as.name(vid), !!as.name(x), !!as.name(y)))
  )
}

# Function to look at the distribution of different duration columns
prop_count <- function(df, x) {
  df <- df %>%
    group_by(!!as.name(x)) %>%
    summarize(freq = n()) %>%
    mutate(prop = (freq / sum(freq)) * 100) %>%
    arrange(desc(prop))
  return(df)
}

# selecting the columns, coalescing them, and then adding it back to the main df
coalesce_redundant <- function(df, word, var) {
  new_var <- select(df, contains(word))
  # using do.call to coalesce the entire subsetted data frame.
  var <- do.call(coalesce, new_var)
  df <- data.frame(var, df)
  return(df)
}

# Clean up function to aggregate columns:
fix_discrepancy <- function(df, word, var) {
  df_filtered <- select(df, matches(word))
  main_col <- do.call(coalesce, df_filtered)
  main_col <- as.data.frame(main_col)
  df_new <- data.frame(main_col, df)
  names(df_new)[1] <- var
  return(df_new)
}

# A function to check missing values in each column, and filter them out:
filter_missing <- function(df) {
  missing <- names(df) %>%
    map_dfr(~ tibble(variable = .x, missing = sum(is.na(df[[.x]])))) %>%
    mutate(prop_missing = missing / nrow(df) * 100) %>%
    filter(prop_missing > 95)
  df_filtered <- df %>%
    select(-contains(missing$variable))
  message(paste0("Deleted variables: ", missing$variable, "\n"))
  return(df_filtered)
}

# A function I picked up from r-bloggers
# https://www.r-bloggers.com/2011/11/
# outersect-the-opposite-of-rs-intersect-function/
outersect <- function(x, y) {
  sort(c(setdiff(x, y), setdiff(y, x)))
}

# A function to use to check if the merge loop is working:
check_loop <- function(i) {
  for (i in file_list[i]) {
    out <- i %>%
      set_names(.) %>%
      map_dfr(
        ~ read.table(.x, sep = ",", header = TRUE, nrows = 10),
        .id = "history_file"
      ) %>%
      clean_names()
    assert_that(nrow(out) / 10 == length(file_list[i]))
  }
}

# Conditional probability (adopted from hiddenDonorsELJ)
pretty_condprob <- function(df, A_var, A_val, B_var, B_val,
                            output = "string", digits = 1, percent = TRUE) {
  B <- df %>%
    filter(!!as.name(B_var) == B_val)
  A <- df %>%
    filter((!!as.name(A_var) == A_val) & (!!as.name(B_var) == B_val))
  if (output != "string") {
    return(nrow(A) / nrow(B))
  } else {
    message(
      paste0(
        "Cond. on ", B_var, " == ", B_val, ", Pr(",
        A_var, " == ", A_val, ") is ",
        ifelse(
          percent == TRUE,
          paste0(round(nrow(A) / nrow(B) * 100, digits = digits), "%"),
          round(nrow(A) / nrow(B), digits = digits)
        )
      )
    )
  }
}

multiclass_train_prep <- function(df, class = 3, y = "gen2020") {
  tc <- trainControl(
    method = "cv", number = 10,
    summaryFunction = ifelse(class > 2, multiClassSummary, prSummary),
    allowParallel = TRUE, verboseIter = FALSE, seeds = rep_seeds(),
    classProbs = TRUE, savePredictions = TRUE
  )

  df <- df %>% mutate(across(where(is.factor), as.character))
  df[df == "In person"] <- "In_person"
  df[df == "Not voted"] <- "Not_voted"

  ## Train-test split
  set.seed(123)
  x <- createDataPartition(df[[y]], p = 0.8, list = FALSE)
  training <- df[x, ] %>% mutate(!!as.name(y) := factor(!!as.name(y)))
  test <- df[-x, ] %>% mutate(!!as.name(y) := factor(!!as.name(y)))
  return(list(train = training, test = test, tc = tc))
}

pred_df <- function(list, model, y = "gen2020") {
  list$test$pred <- predict.train(model, newdata = list$test)
  table(list$test$pred)
  confusionMatrix(data = list$test$pred, reference = list$test[[y]])
  temp <- list$test %>%
    select(obs = !!as.name(y), pred = pred) %>%
    bind_cols(., predict.train(model, newdata = list$test, type = "prob"))
  temp <- as.data.frame(temp) ## If tibble, levels(temp[, "pred"]) does not work
  return(temp)
}

## caret::underSample only allows for 1:1:1 for minority classes
## ROSE::ovun.sample cannot handle more than 2 classes
## Manually downsampling mail voters to 25%

downSample_custom <- function(list, p = .5, majority = "Mail", y = "gen2020") {
  set.seed(123)
  list$traind <- list$train %>%
    filter(!!as.name(y) == majority) %>%
    sample_frac(size = p) %>%
    bind_rows(., list$train %>% filter(!!as.name(y) != majority))
  return(list)
}

ternary_extra <- function(p, label = c("", "")) {
  p <- p +
    labs(color = "Outcome", shape = "Year") +
    xlab("Mail") +
    ylab("In-Person") +
    zlab("Not Voted") +
    theme_bw() +
    theme_arrownormal() +
    geom_line(aes(group = county), size = 0.2, alpha = 0.1) +
    scale_color_manual(values = c("Dem" = "#2166ac", "Rep" = "#b2182b")) +
    annotate(
      geom = "text",
      x = c(0.45, 0.1),
      y = c(0.45, 0.45),
      z = c(0.1, 0.45),
      label = label,
      ## label = c("No Turnout", "In Person"),
      family = "CM Roman"
    )
  return(p)
}

county_summ <- function(df, group = "winner") {
  df %>%
    filter(year == 2020) %>%
    mutate(
      two_modes = in_person / (mail + in_person) * 100,
      sum = not_voted + mail + in_person,
      not_voted = not_voted / sum * 100,
      mail = mail / sum * 100,
      in_person = in_person / sum * 100
    ) %>%
    group_by(!!as.name(group)) %>%
    summarise(
      two_modes = formatC(mean(two_modes), format = "f", digits = 1),
      in_person = formatC(mean(in_person), format = "f", digits = 1),
      mail = formatC(mean(mail), format = "f", digits = 1),
      not_voted = formatC(mean(not_voted), format = "f", digits = 1),
    )
}

varimp_labels <- c(
  "party_oth", "party_rep", "congressional_congressional_2",
  "congressional_congressional_3", "congressional_congressional_4",
  "congressional_congressional_5", "congressional_congressional_6",
  "congressional_congressional_7", "permanent_mail_in_voter_yes",
  "status_inactive", "countyalamosa", "countyarapahoe", "countyarchuleta",
  "countybaca", "countybent", "countyboulder", "countybroomfield",
  "countychaffee", "countycheyenne", "countyclear_creek", "countyconejos",
  "countycostilla", "countycrowley", "countycuster", "countydelta",
  "countydenver", "countydolores", "countydouglas", "countyeagle",
  "countyel_paso", "countyelbert", "countyfremont", "countygarfield",
  "countygilpin", "countygrand", "countygunnison", "countyhinsdale",
  "countyhuerfano", "countyjackson", "countyjefferson", "countykiowa",
  "countykit_carson", "countyla_plata", "countylake", "countylarimer",
  "countylas_animas", "countylincoln", "countylogan", "countymesa",
  "countymineral", "countymoffat", "countymontezuma", "countymontrose",
  "countymorgan", "countyotero", "countyouray", "countypark", "countyphillips",
  "countypitkin", "countyprowers", "countypueblo", "countyrio_blanco",
  "countyrio_grande", "countyroutt", "countysaguache", "countysan_juan",
  "countysan_miguel", "countysedgwick", "countysummit", "countyteller",
  "countywashington", "countyweld", "countyyuma", "registration_date",
  "gen2018_in_person", "gen2018_not_voted", "gen2016_in_person",
  "gen2016_not_voted", "gen2014_in_person", "gen2014_not_voted",
  "gender_male", "gender_missing", "pri2020_in_person", "pri2020_not_voted",
  "pri2018_in_person", "pri2018_not_voted", "pri2016_in_person",
  "pri2016_not_voted", "pri2014_in_person", "pri2014_not_voted",
  "age_groups_gen_x_41_56", "age_groups_gen_z_18_24",
  "age_groups_milennial_25_40", "age_groups_missing", "age_groups_silent_75",
  "county_designation_rural", "county_designation_frontier", "cases_per_10k",
  "deaths_per_10k", "dem_2016", "rep_2016", "oth_2016", "winner_2016rep",
  "distance", "cases_delta", "deaths_delta"
) %>%
  enframe() %>%
  select(-name) %>%
  rename(name = value) %>%
  rowwise() %>%
  mutate(
    label = case_when(
      name == "party_oth" ~ "Party: Other",
      name == "party_rep" ~ "Party: Republican",
      name == "permanent_mail_in_voter_yes" ~ "Permanent Mail-in Voter",
      name == "registration_date" ~ "Registration Date",
      name == "status_inactive" ~ "Inactive",
      name == "age_groups_missing" ~ "Age: Missing",
      name == "age_groups_silent_75" ~ "Silent Generation",
      name == "gender_male" ~ "Male",
      name == "gender_missing" ~ "Gender: Missing",
      name == "cases_per_10k" ~ "County-level Cases per 10,000 on Oct 2020",
      name == "deaths_per_10k" ~ "County-level Deaths per 10,000 on Oct 2020",
      name == "dem_2016" ~ "County-level Dem. Presidential Vote Share in 2016",
      name == "rep_2016" ~ "County-level Rep. Presidential Vote Share in 2016",
      name == "oth_2016" ~ "County-level Oth. Presidential Vote Share in 2016",
      name == "winner_2016rep" ~ "County-level Presidential Winner",
      name == "distance" ~ "Distance from Nearest Vote Center",
      name == "deaths_delta" ~ "County-level Deaths, Percent Increase in Two Weeks",
      name == "cases_delta" ~ "County-level Cases, Percent Increase in Two Weeks",
      TRUE ~ name
    ),
    label = gsub("_", " ", label),
    label = case_when(
      grepl("county", label) ~
        paste0("County: ", simple_cap(gsub("county", "", label))),
      grepl("congressional", label) ~
        paste0("Cong. Dist. ", simple_cap(gsub("congressional ", "", label))),
      grepl("designation", label) ~
        paste0(simple_cap(gsub("county designation ", "", label)), " County"),
      grepl("age group", label) ~ simple_cap(
        gsub("age groups ", "", trimws(str_remove_all(label, "[0-9]")))
      ),
      TRUE ~ label
    ),
    label = case_when(
      grepl("not voted", label) & grepl("pri", label) ~ paste0(
        "Not Voted in Primary ", trimws(str_remove_all(label, "[a-z]"))
      ),
      grepl("not voted", label) & grepl("gen", label) ~ paste0(
        "Not Voted in General ", trimws(str_remove_all(label, "[a-z]"))
      ),
      grepl("in person", label) & grepl("pri", label) ~ paste0(
        "Voted in-person in Primary ", trimws(str_remove_all(label, "[a-z]"))
      ),
      grepl("in person", label) & grepl("gen", label) ~ paste0(
        "Voted in-person in General ", trimws(str_remove_all(label, "[a-z]"))
      ),
      TRUE ~ label
    )
  ) %>%
  ungroup()

# county-level summary for descriptive data
desc_county <- function(df, county, y, fill) {
  v <- ifelse(
    y == "gen2020",
    "In person",
    "Yes"
  )
  out <- df %>%
    group_by(!!as.name(county), !!as.name(y)) %>%
    summarise(n = n(), !!as.name(fill) := last(!!as.name(fill))) %>%
    mutate(prop = n / sum(n, na.rm = TRUE)) %>%
    filter(!!as.name(y) == v) %>%
    rowwise() %>%
    mutate(county = simple_cap(!!as.name(county)))

  if (!grepl("10k", fill)) {
    out <- out %>%
      mutate(!!as.name(fill) := simple_cap(as.character(!!as.name(fill))))
  }

  return(ungroup(out))
}

county_stacked_plot <- function(df,
                                y = "gen2020",
                                fill = "county_designation",
                                xint = 50,
                                yint = 0.045,
                                county = "county_full", 
                                continuous = FALSE) {
  ylab <- ifelse(
    y == "gen2020",
    "Proportion of In-person Votes",
    "Proportion of Switchers"
  )
  labfill <- ifelse(
    fill == "county_designation",
    "Designation",
    ifelse(
      grepl("cases", fill),
      "COVID-19 Cases Per 10,000",
      "COVID-19 Deaths Per 10,000"
    )
  )

  df_summ <- desc_county(df = df, county = county, y = y, fill = fill)

  avg_num <- as.numeric(prop(df, y, digit = 3)[[2]]) / 100
  
  if (continuous) {
    df_summ[[fill]] <- as.numeric(df_summ[[fill]])
    p <- ggplot(df_summ, aes(x = fct_reorder(county, desc(prop)))) +
      geom_col(aes(y = prop, color = NULL, fill = !!as.name(fill))) +
      scale_fill_viridis_c(end = 0.9, direction = -1)
  } else {
    p <- ggplot(df_summ, aes(x = fct_reorder(county, desc(prop)))) +
      geom_col(aes(y = prop, color = NULL, fill = !!as.name(fill))) +
      scale_fill_viridis_d(end = 0.9, direction = -1)
  }
  p <- p +
    scale_y_continuous(labels = percent) +
    xlab("Counties") +
    ylab(ylab) + 
    geom_hline(yintercept = avg_num) +
    annotate(
      geom = "text", x = xint, y = yint, family = "CM Roman",
      label = paste0(
        "Average: ", formatC(avg_num * 100, digits = 2, format = "f"), "%"
      )
    )
  
  if (grepl("delta", fill)) {
    labfill <- ifelse(
      grepl("cases", fill),
      "COVID-19 Cases % Increase", ## , Sep 24 to Oct 9, 2020",
      "COVID-19 Deaths % Increase" ## , Sep 24 to Oct 9, 2020"
    )
  }
  
  p <- p +
    labs(fill = labfill)
  return(p)
}

party_stacked_plot <- function(df, 
                               y = "gen2020",
                               x = "age_groups") {
  xlab <- ifelse(
    x == "age_groups", 
    "Age Groups",
    "Registration Months for Post-2016 Registrants"
  )
  lvls <- ifelse(
    y == "gen2020",
    c("In person", "Mail", "Not voted"),
    c("Yes", "No")
  )
  labls <- ifelse(
    y == "gen2020",
    c("In person", "Mail", "Not voted"),
    c("Switched to In-person", "Not Switched")
  )
  p <- df %>%
    group_by(party, !!as.name(x), !!as.name(y)) %>%
    filter(!is.na(!!as.name(x))) %>%
    summarise(n = n()) %>%
    group_by(!!as.name(y)) %>%
    mutate(sum = sum(n, na.rm = TRUE)) %>%
    mutate(prop = n / sum) %>%
    mutate(
      party = case_when(
        party == "dem" ~ "Dem.",
        party == "rep" ~ "Rep.",
        TRUE ~ "Others"
      ),
      !!as.name(y) := factor(!!as.name(y), levels = lvls, labels = labls)
    ) %>%
    ggplot() +
    geom_bar(
      stat = "identity",
      aes(x = !!as.name(x), y = prop, fill = party)
    ) +
    xlab(xlab) +
    ylab("") +
    labs(fill = "Party") +
    scale_y_continuous(labels = percent) +
    facet_wrap(as.formula(paste("~", y))) +
    scale_fill_manual(
      values = c("Dem." = "#2166ac", "Rep." = "#b2182b", "Others" = "darkgray")
      ## ['#b2182b','#ef8a62','#fddbc7','#f7f7f7','#d1e5f0','#67a9cf','#2166ac']
    )
  return(p)
}

# Define file directory ========================================================
# Need relative paths so not here::here
file_paths <- list(
  master_voter =
    here("data", "raw", "EX-003 Master Voter List"),
  hist =
    here("data", "raw", "EX-002 Voting History Files"),
  ballots =
    here("data", "raw", "CE-068_Voters_With_Ballots_List_Public"),
  returned =
    here("data", "raw", "CE-068c_Voters_With_Returned_Ballot_List_Public"),
  cured =
    here("data", "raw", "CE-077_Rejected_Cure"),
  undelivered =
    here("data", "raw", "CE-037_UndeliverableBallots")
)
