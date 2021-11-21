setwd('~/nuforc_stats/notebooks/')

library(dplyr)
library(ggplot2)
library(ggResidpanel)
library(knitr)
library(lme4)
library(lubridate)
library(stringr)
library(PerformanceAnalytics)

# load
df <- readRDS('../data/nuforc/processed/nuforc_reports.rds') %>%
  mutate(loc = tolower(paste(city, state, sep=', '))) %>%
  filter(state != 'AK', state != 'HI',
         year >= 2010,
         year <= 2020,
         str_detect(summary, 'MADAR') == FALSE)

# Counties
counties <- read.csv('../data/census/raw/uscities.csv', stringsAsFactors=FALSE) %>%
  select(city_ascii, state_id, county_fips, county_name, timezone) %>%
  unique()

dff <- df %>%
  inner_join(counties, by=c('city'='city_ascii', 'state'='state_id'))

# This drops from 56,639 to 50,223 -- 11.3% less
df1 <- df %>%
  inner_join(counties, by=c('city'='city_ascii', 'state'='state_id')) %>%
  group_by(year, state, county_fips, county_name) %>%
  summarise(n=n())

# County area
county_area <- read.csv('../data/census/raw/county_area.csv', stringsAsFactors=FALSE, skip=2) %>%
  janitor::clean_names() %>%
  select(areaname, county_fips, land_area_sqm_2010)

# Loses only 1
df2 <- df1 %>%
  inner_join(county_area, by='county_fips')

# County population
county_pop <- read.csv('../data/census/processed/county_pop.csv', stringsAsFactors=FALSE)

df3 <- df2 %>%
  inner_join(county_pop, by=c('year', 'county_fips')) %>%
  select(year, state.y, state_abr, county_fips, county_name.x, n, land_area_sqm_2010,
         pop, density) %>%
  rename(state='state.y', county_name='county_name.x')

# Urbanization
urban <- read.csv('../data/census/processed/county_perc_urban.csv', stringsAsFactors=FALSE)



