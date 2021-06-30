# Determine distance of each case from the USGS facilit yin Fredericksburg,
# Virginia, that is a member of INTERMAGNET: 
# https://www.usgs.gov/natural-hazards/geomagnetism/science/fredericksburg-frd?qt-science_center_objects=0#qt-science_center_objects
#
# Use's geosphere backage to calculate distances between two coordinates

setwd('~/nuforc_stats/notebooks/')

library(dplyr)
library(geosphere)
library(ggplot2)
library(knitr)
library(lubridate)
library(stringr)

LONG <- 77.3729  # 77.3729°W
LAT <- 38.2047  # 38.2047°N

# lat: 39.276, long: -77.21, damascus md
# 77.21°W 39.276°N

# load, only those states possibly within 100 miles radius
df <- readRDS('../data/nuforc/processed/nuforc_reports.rds') %>%
  filter(state %in% c('VA', 'WV', 'PA', 'MD', 'DE', 'DC', 'NJ')) %>%
  mutate(city_longitude = abs(city_longitude))

# Get distance of each remaining observation from observatory
locs <- df %>%
  select(city, state, city_longitude, city_latitude) %>%
  unique()

dist <- list()
for (i in 1:nrow(locs)) {
  print(i)
  test_lon <- locs$city_longitude[i]
  test_lat <- locs$city_latitude[i]
  d <- distm( c(LONG, LAT), c(test_lon, test_lat), fun=distHaversine)
  dist[[i]] <- d
}

locs$km_to_obs <- round(unlist(dist) / 1000, 2)

# Now restrict df to only those sightings within 160 km of the obs
dff <- df %>%
  right_join(locs %>% select(city, state, km_to_obs), by=c('state', 'city')) %>%
  filter(km_to_obs < 160)

nrow(dff)

hist(dff$km_to_obs)

write.csv(dff, '../data/magnetic/processed/va_obs.csv', row.names=FALSE)
