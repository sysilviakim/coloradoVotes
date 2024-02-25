source(here::here("R", "utilities.R"))
library(tidygeocoder)
library(sf)

# Import merged data before cleaning ===========================================
## Takes output from the vr history merge file
load(here("data", "tidy", "merged_full.RData"))

## Full address line
df$full_address <- paste0(
  df$residential_address, ", ",
  df$residential_state, " ", df$residential_zip_code
)

## Split the dataframe by 1,000 rows into a list, but just the addresses
df_list <- df %>%
  ungroup() %>%
  select(contains("residential")) %>%
  select(-residential_zip) %>%
  dedup() %>%
  split(ceiling(seq(nrow(.)) / 1000))
save(df_list, file = here("data", "tidy", "geocoding_temp.Rda"))

## Iterate through the list and geocode the addresses
for (i in seq(length(df_list))) {
  df_list[[i]] <- df_list[[i]] %>%
    geocode(
      street = residential_address,
      city = residential_city,
      state = residential_state,
      postalcode = residential_zip_code,
      method = "census",
      lat = latitude, long = longitude, full_results = TRUE
    )
  cat("Processed", i, "of", length(df_list), "chunks\n")
  save(df_list, file = here("data", "tidy", "geocoding_final.Rda"))
}
save(df_list, file = here("data", "tidy", "geocoding_final.Rda"))

# VSPC drop box locations, Jun 2020 ============================================
dropbox <- here(
  "data", "raw", "Election Information",
  "June 2020 State Primary VSPCs and Drop Boxes.xlsx"
) %>%
  readxl::read_xlsx() %>%
  clean_names()

## Geocode dropbox locations
## https://jessecambon.github.io/tidygeocoder/articles/geocoder_services.html
## Default method: Nominatim osm
dropbox <- dropbox %>%
  geocode(
    location_address,
    method = "osm",
    lat = latitude, long = longitude, full_results = TRUE
  )

## Bounding box ---> now generate a single latitude and longitude
## Each boundingbox variable is a vector of four values
## Latitude is a mean of the first two, and longitude a mean of the last two
dropbox <- dropbox %>%
  rowwise() %>%
  mutate(
    latitude = mean(as.numeric(c(boundingbox[1], boundingbox[2]))),
    longitude = mean(as.numeric(c(boundingbox[3], boundingbox[4])))
  ) %>%
  ungroup() %>%
  select(county, location_name, location_address, latitude, longitude) %>%
  dedup()

## Save the data
save(df, dropbox, file = here("data", "tidy", "dropbox_osm.Rda"))

## If NaN value, try census
census_prep <- dropbox %>% filter(is.nan(latitude))
census_prep <- census_prep %>%
  select(-latitude, -longitude) %>%
  geocode(
    location_address,
    method = "census",
    lat = latitude, long = longitude, full_results = TRUE
  )

## Save the data
save(census_prep, file = here("data", "tidy", "dropbox_census.Rda"))

## Which ones are still (stubbornly) unavailable?
## census_prep %>% filter(is.na(latitude)) %>% View()
census_prep <- census_prep %>%
  mutate(
    latitude = ifelse(
      is.na(latitude) & grepl("8999 Independence Way", location_address), 37.4451123, latitude
    ),
    longitude = ifelse(
      is.na(longitude) & grepl("8999 Independence Way", location_address), -105.8914881, longitude
    ),
    latitude = ifelse(
      is.na(latitude) & grepl("6683 County Road", location_address), 37.0889197, latitude
    ),
    longitude = ifelse(
      is.na(longitude) & grepl("6683 County Road", location_address), -106.0305859, longitude
    ),
    latitude = ifelse(
      is.na(latitude) & grepl("400 Gasper St", location_address), 37.1969717, latitude
    ),
    longitude = ifelse(
      is.na(longitude) & grepl("400 Gasper St", location_address), -105.4343597, longitude
    ),
    latitude = ifelse(
      is.na(latitude) & grepl("125 Stephanie Place", location_address), 39.3763789, latitude
    ),
    longitude = ifelse(
      is.na(longitude) & grepl("125 Stephanie Place", location_address), -104.8746427, longitude
    ),
    latitude = ifelse(
      is.na(latitude) & grepl("3280 E. Redstone Park", location_address), 39.5478816, latitude
    ),
    longitude = ifelse(
      is.na(longitude) & grepl("3280 E. Redstone Park", location_address), -105.0319, longitude
    ),
    latitude = ifelse(
      is.na(latitude) & grepl("8155 Piney River", location_address), 39.5068856, latitude
    ),
    longitude = ifelse(
      is.na(longitude) & grepl("8155 Piney River", location_address), -105.0480509, longitude
    ),
    latitude = ifelse(
      is.na(latitude) & grepl("20 Eagle County", location_address), 39.3925408, latitude
    ),
    longitude = ifelse(
      is.na(longitude) & grepl("20 Eagle County", location_address), -107.1009917, longitude
    ),
    latitude = ifelse(
      is.na(latitude) & grepl("1675 W Garden of the Gods", location_address), 38.8952398, latitude
    ),
    longitude = ifelse(
      is.na(longitude) & grepl("1675 W Garden of the Gods", location_address), -104.8705515, longitude
    ),
    latitude = ifelse(
      is.na(latitude) & grepl("3275 Akers Dr", location_address), 38.8813144, latitude
    ),
    longitude = ifelse(
      is.na(longitude) & grepl("3275 Akers Dr", location_address), -104.6978077, longitude
    ),
    latitude = ifelse(
      is.na(latitude) & grepl("345 County Rd", location_address), 38.3751892, latitude
    ),
    longitude = ifelse(
      is.na(longitude) & grepl("345 County Rd", location_address), -105.700289, longitude
    ),
    latitude = ifelse(
      is.na(latitude) & grepl("195 W 14th", location_address), 39.5415013, latitude
    ),
    longitude = ifelse(
      is.na(longitude) & grepl("195 W 14th", location_address), -107.7954087, longitude
    ),
    latitude = ifelse(
      is.na(latitude) & grepl("1 Main St", location_address), 39.9312167, latitude
    ),
    longitude = ifelse(
      is.na(longitude) & grepl("1 Main St", location_address), -105.8016577, longitude
    ),
    latitude = ifelse(
      is.na(latitude) & grepl("Zero St", location_address), 40.0867233, latitude
    ),
    longitude = ifelse(
      is.na(longitude) & grepl("Zero St", location_address), -105.952508, longitude
    ),
    latitude = ifelse(
      is.na(latitude) & grepl("54175 RCR", location_address), 40.7070689, latitude
    ),
    longitude = ifelse(
      is.na(longitude) & grepl("54175 RCR", location_address), -106.9294748, longitude
    ),
    latitude = ifelse(
      is.na(latitude) & grepl("83 Nancy's Place", location_address), 39.5724776, latitude
    ),
    longitude = ifelse(
      is.na(longitude) & grepl("83 Nancy's Place", location_address), -106.0950307, longitude
    ),
    latitude = ifelse(
      is.na(latitude) & grepl("4209 County Road", location_address), 40.1697956, latitude
    ),
    longitude = ifelse(
      is.na(longitude) & grepl("4209 County Road", location_address), -104.9861126, longitude
    )
  )
assert_that(all(!is.na(census_prep$latitude)))

## Save the data
dropbox <- census_prep
save(dropbox, file = here("data", "tidy", "dropbox_final.Rda"))

# Calculating the minimum distance to the closest drop box =====================
load(here("data", "tidy", "geocoding_final.Rda"))
load(here("data", "tidy", "dropbox_final.Rda"))
df <- df_list %>%
  bind_rows() %>%
  unique() %>%
  filter(!is.na(latitude))

## Columns latitude and longitude in `df`
## Columns latitude and longitude in `dropbox`
voters_sf <- st_as_sf(df, coords = c("longitude", "latitude"), crs = 4326)
dropbox_sf <- st_as_sf(dropbox, coords = c("longitude", "latitude"), crs = 4326)

## Calculate the distance between each voter and the closest dropbox
## distances <- st_distance(head(voters_sf), dropbox_sf)
## Actually, the nearest point would be better
nearest <- st_nearest_feature(voters_sf, dropbox_sf)
save(nearest, file = here("data", "tidy", "nearest_dropbox_index.Rda"))

## Need to calculate the distance between each voter and the closest box
for (i in seq(nrow(df))) {
  df$distance[i] <- st_distance(voters_sf[i, ], dropbox_sf[nearest[i], ])
  cat(i, "\n")
  if (i %% 5000 == 0 | i == nrow(df)) {
    save(df, file = here("data", "tidy", "voter_res_to_box_distance.Rda"))
  }
}
assert_that(all(!is.na(df$distance)))

## Actually, rename it to `distance_to_box`
distance_to_box <- df
save(
  distance_to_box,
  file = here("data", "tidy", "voter_res_to_box_distance.Rda")
)
