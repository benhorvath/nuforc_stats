# Elevation data from USGS Elevation Point Query Service, using the WGS 84 projection

setwd('~/nuforc_stats/')

library(dplyr)
library(elevatr)
library(ggplot2)
library(lubridate)
library(stringr)

# load
df <- readRDS("./data/nuforc/processed/nuforc_reports.rds")
x <- df %>% select(city_longitude, city_latitude) %>%
  rename(x='city_longitude', y='city_latitude') %>%
  unique()

proj <- 'EPSG:4326'  # WGS 84 projection

elev <- as.data.frame(suppressMessages(get_elev_point(locations=x, prj=proj, units='feet', src='epqs')))

elev <- elev %>%
  rename(longitude='x', latitude='y') %>%
  select(-elev_units)

write.csv(elev, './data/elevation.csv', row.names=FALSE)