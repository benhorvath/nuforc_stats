# Use the Census's FIPS standard of geographic codes  (2010) to map U.S. cities to
# counties, then use the tidycensus package to associate Census measure of 
# population density to county.
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

# # Process FIPS
# places <- read.csv('../data/census/raw/ansi_places.csv', sep='|', stringsAsFactors=FALSE) %>%
#   janitor::clean_names() %>%
#   mutate(placename = str_trim(str_remove(placename, ' \\w+$')),
#          county = str_trim(str_remove(county, 'Area|Borough|city|City|County|Municipality|Municipio|Parish'))) %>%
#   dplyr::select(-type, -funcstat) %>%
#   rename(state_abr='state', city='placename') %>%
#   mutate(loc = tolower(paste(city, state_abr, sep=', '))) %>%
#   dplyr::select(placefp, state_abr, county, city, loc)

appellations <- 'CDP|city|town|municipality|borough|NA|village|government|County|county|township|corporation|comunidad|urban'

# Retrieve city population 
census <- get_estimates(geography='place', product='population', year=2019) %>%
  tidyr::pivot_wider(names_from='variable', values_from='value') %>%
  janitor::clean_names() %>%
  mutate(city = str_trim(str_extract(name, '^([^,])+')),
         city = str_trim(str_remove(city, appellations)),
         state =  str_trim(str_extract(name, '([^,])*$')),
         state_abr = recode(state, !!!st_mapping)) %>%
  dplyr::select(geoid, city, state, state_abr, pop, density) %>%
  filter(pop > 0)

head(census)

write.csv(census, '../data/census/processed/pop.csv', row.names=FALSE)
