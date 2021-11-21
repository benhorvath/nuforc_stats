# Retrieves county-level estimates of urbanization including urban/rual population,
# density, and total area from  2010 census.
# Source: https://www.census.gov/programs-surveys/popest/technical-documentation/research/evaluation-estimates/2020-evaluation-estimates/2010s-totals-housing-units.html

setwd('~/nuforc_stats/notebooks/')

library(dplyr)
library(ggplot2)
library(lubridate)
require(janitor)

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

df <- read.csv('../data/census/raw/county_percent_urban.csv', skip=2,
               stringsAsFactors=FALSE) %>%
  janitor::clean_names() %>%
  mutate(state_abr = recode(statename, !!!st_mapping))

write.csv(df, '../data/census/processed/county_perc_urban.csv', row.names=FALSE)

