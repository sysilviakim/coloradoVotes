source(here::here("R", "utilities.R"))

fname <- here("data", "tidy", "multiclass.Rda")
if (nrows == 100) {
  load(gsub(".Rda", "_sample.Rda", fname))
} else {
  load(fname)
}

# Function ---------------------------------------------------------------------
# Function that does an initial geocoding batch job, then filters out addresses
# with NA (due to multiple matches), individually geocodes them, and then
# joins the two:
geocode_full <- function(df) {
  # Round one geocoding
  geo_1 <- `df` %>%
    geocode(address = address, method = "census", verbose = TRUE)

  # Filter for those with multiple matches (NA vals)
  if ("voter_id" %in% colnames(geo_1)) {
    addresses_temp <- geo_1 %>%
      filter(is.na(lat)) %>%
      select(address, voter_id)
  } else {
    addresses_temp <- geo_1 %>%
      filter(is.na(lat)) %>%
      select(address)
  }

  # Run individual geocoding
  geo_2 <- addresses_temp %>%
    geocode(address = address, method = "census", mode = "single", verbose = TRUE)

  # Join with the main one:
  geo_new <<- geo_1 %>%
    filter(!is.na(lat)) %>%
    rbind(geo_2) %>%
    # I need to find a way to deal with the misformatted addresses, there are
    # three issues here:
    #  - Apartment number in the address
    #  - Lot number in the address
    #  - Some weird character like "#"
    # Removing them for now though:
    filter(!is.na(lat))
}

# Format the address column ----------------------------------------------------
addresses <- df %>%
  ungroup() %>%
  mutate(address = str_c(residential_address, residential_city, "CO",
    sep = ", "
  )) %>%
  select(voter_id, address) %>%
  slice(1:50)

# Use the geocode function:
geocode_full(addresses)

# Join back into the main file
df_geo <- inner_join(df, geo_new, by = "voter_id")

# Get locations of polling places ----------------------------------------------
# Download kml file of polling places from: 
## https://www.google.com/maps/d/u/1/viewer?mid=1Q8RG8rSHeBhPzUbsqQV5nctnfpqZwybH&ll=39.026382895920456%2C-105.550939&z=7
# Convert to csv file at: http://www.convertcsv.com/kml-to-csv.htm
# Load csv file:
polling_places <- read_csv(here("data/extras/polling_places.csv")) %>%
  rename(place_name = name) %>%
  select(address) %>%
  # or testing sake, using a sample
  slice(1:50)

# Geocode this file using the geocode_full function:
geocode_full(polling_places)

# Column for distance to closest dropbox ---------------------------------------
# Replicated a stackoverflow answer for this part!
# Order into a newdf as needed first.
# First, the voters:
voter_addresses <- data.frame(
  voter_id = as.numeric(df_geo$voter_id),
  lon_address = df_geo$long,
  lat_address = df_geo$lat
)
# Second, the polling locations:
polling_address <- data.frame(
  place_number = 1:nrow(geo_new),
  lon_place = geo_new$long,
  lat_place = geo_new$lat
)

# Create nested dfs:
voter_nest <- nest(voter_addresses, -voter_id, .key = "voter_coords")
polling_nest <- nest(polling_address, -place_number, .key = "polling_coords")

# Combine for combinations:
data_master <- crossing(voter_nest, polling_nest)

# Calculate shortest distance:
shortest_dist <- data_master %>%
  mutate(dist = map2_dbl(voter_coords, polling_coords, distm)) %>%
  group_by(voter_id) %>%
  filter(dist == min(dist)) %>%
  mutate(
    dist_km = dist / 1000,
    voter_id = as.character(voter_id)
  ) %>%
  select(voter_id, dist_km)

# Join back with our main df:
df_final <- inner_join(df_geo, shortest_dist, by = "voter_id")
