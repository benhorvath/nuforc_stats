# API documentation: https://www.visualcrossing.com/resources/documentation/weather-data/weather-data-documentation/

setwd('~/nuforc_stats/')

library(dplyr)
library(ggplot2)
library(lubridate)
library(stringr)

# load
# Appears to be some snow data that is missing in late 2014 -- just mark as zero for now
df <- read.csv('./data/weather/raw/weather_dump.tsv', stringsAsFactors=FALSE, sep='\t') %>%
  select(-queryCost, -precipprob, -preciptype, -stations, -source) %>%
  tidyr::replace_na(list(snow = 0,
                         snowdepth = 0))

sapply(df, function(x) sum(is.na(x)))

# potentially cap visibility at 20 (miles)?


# Handle NAs -- mean or median of reports with the data


write.table(df, './data/weather/processed/weather.tsv', row.names=FALSE, sep='\t')