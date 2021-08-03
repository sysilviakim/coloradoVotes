source(here::here("R", "utilities.R"))
load("data/tidy/sample/merged_sample.RData")

# Wrangle ----------------------------------------------------------------------
# Criteria for binning registration dates: 
# - Election Day Registration: If registration day coincides with 
# gen2014/16/18/20, pri2014/16/18/20
# - Within 15 days of the election date
# - Within 30 Days of the election date 
# - Anything more than 30, coded as other 
# Ideally when the columns are coalesced there is only one non-NA value. But, 
# on the off chance they re registered, I ordered them chronologically to 
# pull their latest registration time.  
# Also binned age--don't have any specific logic behind them, went off age groups
# that generally seem to be used. 
df <- df %>%
  mutate(registration_date = mdy(registration_date)) %>%
  mutate(party = case_when(
    party %in% "dem" | party %in% "democratic" ~ "dem",
    party %in% "rep" | party %in% "republican" ~ "rep",
    TRUE ~ "oth"
  )) %>%
  mutate(age = 2021 - as.integer(yob)) %>%
  mutate(age_grp = case_when(
    age >= 18 & age <= 29 ~ "18 - 29",
    age >= 30 & age <= 44 ~ "30-44",
    age >= 45 & age <= 59 ~ "45-59",
    age >= 60 ~ "60+"
  )) %>%
  mutate(reg_gen2020 = as.numeric(mdy("11/03/2020") - registration_date),
         reg_gen2016 = as.numeric(mdy("11/08/2016") - registration_date),
         reg_gen2018 = as.numeric(mdy("11/06/2018") - registration_date),
         reg_gen2014 = as.numeric(mdy("11/04/2014") - registration_date),
         reg_pri2020 = as.numeric(mdy("06/30/2020") - registration_date),
         reg_pri2016 = as.numeric(mdy("06/28/2016") - registration_date),
         reg_pri2018 = as.numeric(mdy("06/26/2018") - registration_date),
         reg_pri2014 = as.numeric(mdy("06/24/2014") - registration_date)) %>%
  mutate(across(c(reg_gen2020, reg_gen2018, reg_gen2016, reg_gen2014,
                reg_pri2020, reg_pri2018, reg_pri2016, reg_pri2014),
         ~ case_when(
           . == 0 ~ "EDR",
           . > 15 & . <= 30 ~ "30Days",
           . > 0 & . <= 15 ~ "15Days"
         ))) %>%
  mutate(reg_bin = coalesce(reg_gen2020, reg_pri2020, reg_gen2018, reg_pri2018, 
                            reg_gen2016, reg_pri2016, reg_gen2014, reg_pri2014),
         reg_bin = replace_na(reg_bin, "30+"))

# Imputing political leanings --------------------------------------------------  
primary_history <- read.table(
  here(
    "data/raw/EX-002 Voting History Files", "20180719",
    "EX-002_2018_Primary_Supplemental_Vote_History",
    "EX-002_2018_Primary_Supplemental_Vote_History.txt"
  ),
  header = TRUE,
  sep = "|"
) %>%
  clean_names() %>%
  select(voted_party, received_party_ballot, voter_id) %>%
  mutate(voter_id = as.character(voter_id),
         across(everything(), tolower)) %>%
  mutate(voted_party = na_if(voted_party, ""),
         received_party_ballot = na_if(received_party_ballot, ""))

df_joined <- left_join(df, primary_history, by = "voter_id")

# Create a column with leanings: 
# Leanings are imputed from: 
# - Party voted for (priority)
# - Party of the ballot they recieved (if party voted for is not available)
df_test <- df_joined %>%
  mutate(party_test = case_when(
    party %in% "oth" & !is.na(voted_party) ~ voted_party,
    party %in% "oth" & is.na(voted_party) & !is.na(received_party_ballot) ~ 
      received_party_ballot,
    TRUE ~ party
  )) 

ggplot(df_test, aes(party)) +
  geom_bar()

# Comparing now: 
# Prop table without imputing: 
table(df$party)/nrow(df) 
# dem       oth       rep 
# 0.3632265 0.2915832 0.3451904

# Prop table after imputing: 
table(df_test$party_test)/nrow(df_test)
# dem       oth       rep 
# 0.4218437 0.1913828 0.3867735 
# Seems to work! 

# Graphics ---------------------------------------------------------------------
# Checking if republicans vote higher is certain counties: 
ggplot(df_test, aes(x = county, fill = as.character(gen2020))) + 
  geom_bar(position = "fill") +
  labs(fill = "Voting Method") +
  theme(axis.text.x = element_text(angle = 90)) 

# Proportion plot of party by county: 
ggplot(df_test, aes(x = county, fill = party_test)) + 
  geom_bar(position = "fill") +
  labs(fill = "Party", title = "Party Affilliation by County") +
  theme(axis.text.x = element_text(angle = 90))

# Proportion plot of registration date by vote method: 
ggplot(df_test, aes(x = reg_bin, fill = as.character(gen2020))) + 
  geom_bar(position = "fill") +
  labs(fill = "Vote Method", title = "Vote Method versus Registration Date") 

# Proportion plot of registration date by partisanship: 
ggplot(df_test, aes(x = reg_bin, fill = party_test)) + 
  geom_bar(position = "fill") +
  labs(fill = "Party", title = "Registration Day versus Partisanship")

# Registration date versus age: 
# Plot of age group distribution first, for reference: 
ggplot(df_test, aes(age_grp)) +
  geom_bar() # Wow, CO is old! 
# Proportion plot: 
ggplot(df_test, aes(x = reg_bin, fill = age_grp)) + 
  geom_bar(position = "fill") +
  labs(fill = "Age Group", title = "Registration Day versus Age")
# Somewhat interesting observation seems to be people in the 45-59 
# age goup prefer EDR to earlier registrations. 

# Proportion plot of age by county: 
ggplot(df_test, aes(x = county, fill = age_grp)) + 
  geom_bar(position = "fill") +
  labs(fill = "Age Group", title = "Age Group by County") +
  theme(axis.text.x = element_text(angle = 90))
# Following the age bar graph, some counties are exclusively composed of 60+ 
# people. Not sure if I went wrong somewhere, of if this is actually the case. 

# Age versus Partisanship: 
ggplot(df_test, aes(x = age_grp, fill = party_test)) +
  geom_bar(position = "fill") +
  labs(fill = "Party", title = "Age Group versus Party")
# Kind of expected, the older the age group the lesser the rep proportion, and 
# greater the dem proportion. 






  