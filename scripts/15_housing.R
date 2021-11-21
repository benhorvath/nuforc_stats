# Retrieves county-level housing estimates from the census web site
# Source: https://www.census.gov/programs-surveys/popest/technical-documentation/research/evaluation-estimates/2020-evaluation-estimates/2010s-totals-housing-units.html

setwd('~/nuforc_stats/notebooks/')

library(dplyr)
library(ggplot2)
library(lubridate)


states <- list()

for (i in 1:56) {
  print(i)
  
  tryCatch({
    url <- sprintf('https://www2.census.gov/programs-surveys/popest/datasets/2010-2020/housing/HU-EST2020_%s.csv', i)
    x <- read.csv(url)
    states[[i]] <- x
    Sys.sleep(2)
  }, error=function(e){})
  
}

df <- do.call(rbind, states)

df <- df %>%
  janitor::clean_names() %>%
  select(stname, ctyname, starts_with('huestimate') ) %>%
  select(-huestimate042020) %>%
  tidyr::pivot_longer(!c(stname, ctyname), values_to='houses', names_to='year') %>%
  mutate(year = str_remove(year, 'huestimatesbase|huestimate04|huestimate'),
         ctyname = str_remove(ctyname, ' County'))

write.csv(df, '../data/census/processed/housing.csv', row.names=FALSE)

