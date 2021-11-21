# Use the Census's FIPS standard of geographic codes  (2010) to map U.S. cities to
# counties, then use the tidycensus package to associate Census measure of 
# population density to county.
# 
# Retrieves from 2010-2020
#
# FIPS geographic place mapping: https://data.world/nrippner/ansi-geographic-codes
# tidycensus package: https://walker-data.com/tidycensus/

require(janitor)
library(dplyr)
library(ggplot2)
library(stringr)
library(tidycensus)

setwd('~/nuforc_stats/notebooks/')

# Census API key
census_api_key('0e92016a65168167e83945c92ce6c254e6964524')

st_mapping <- c(Alabama='AL', Alaska='AK', Arizona='AZ', Arkansas='AR', 
                California='CA', Colorado='CO', Connecticut='CT', 
                Delaware='DE', Florida='FL', Georgia='GA', Hawaii='HI',
                Idaho='ID', Illinois='IL', Indiana='IN', Iowa='IA',
                Kansas='KS', Kentucky='KY', Louisiana='LA', Maine='ME',
                Maryland='MD', Massachusetts='MA', Michigan='MI', Minnesota='MN',
                Mississippi='MS', Missouri='MO', Montana='MT', Nebraska='NE',
                Nevada='NV', `New Hampshire`='NH', `New Jersey`='NJ', `New Mexico`='NM',
                `New York`='NY', `North Carolina`='NC', `North Dakota`='ND', Ohio='OH',
                Oklahoma='OK', Oregon='OR', Pennsylvania='PA', `Rhode Island`='RI', 
                `South Carolina`='SC', `South Dakota`='SD', Tennessee='TN', Texas='TX',
                Utah='UT', Vermont='VT', Virginia='VA', Washington='WA',
                `West Virginia`='WV', Wisconsin='WI', Wyoming='WY', `District of Columbia`='DC') 

appellations <- 'CDP|city|town|municipality|borough|NA|village|government|County|county|township|corporation|comunidad|urban'

# Can also get back to 2010 with:
# census <- get_estimates(geography='place', product='population', time_series=TRUE)
# see PERIOD_CODE for labels: https://www.census.gov/data/developers/data-sets/popest-popproj/popest/popest-vars/2019.html

# Retrieve city population 
census <- get_estimates(geography='place', product='population', time_series=TRUE) %>%
  filter(variable == 'DENSITY') %>%
  janitor::clean_names() %>%
  mutate(state = str_trim(str_extract(name, '[^,]*$')),
         state_abr = recode(state, !!!st_mapping),
         year=date+2009,
         density=value,
         city=name,
         city = str_trim(str_extract(city, '^([^,])+')),
         city = str_trim(str_remove(city, appellations))) %>%
  select(state, state_abr, city, year, geoid, density) %>%
  filter(year <= 2020) %>%
  na.omit

write.csv(census, '../data/census/processed/pop_2010_2020.csv', row.names=FALSE)
