
setwd('~/nuforc_stats/')

library(dplyr)
library(ggplot2)
library(lubridate)
library(stringr)


# Continental U.S. only
STATES <- c('AK', 'AL', 'AZ', 'AR', 'CA', 'CO', 'CT', 'DE', 'DC', 'FL', 'GA', 'ID',
            'IL', 'IN', 'IA', 'KS', 'KY', 'LA', 'ME', 'MD', 'MA', 'MI', 'MN',
            'MS', 'MO', 'MT', 'NE', 'NV', 'NH', 'NJ', 'NM', 'NY', 'NC', 'ND',
            'OH', 'OK', 'OR', 'PA', 'RI', 'SC', 'SD', 'TN', 'TX', 'UT', 'VT',
            'VA', 'WA', 'WV', 'WI', 'WY')

reports <- read.csv('./data/nuforc/raw/nuforc_reports.csv', stringsAsFactors=FALSE) %>%
  mutate(id = str_extract(report_link, '(S\\d+)'),
         date_time = as_datetime(date_time, format='%Y-%m-%dT%H:%M:%S'),
         hour = hour(date_time),
         date = as_date(date_time),
         year = year(date_time),
         posted = as_datetime(posted, format='%Y-%m-%dT%H:%M:%S'),
         city_latitude = as.numeric(city_latitude),
         city_longitude = as.numeric(city_longitude),
         shape = na_if(shape, '')) %>%
  filter(state %in% STATES,
         date_time >= ymd('2000-01-01'),
         date_time < ymd('2021-01-01'),
         !is.na(city_latitude)) %>%
  arrange(desc(date_time))

write.table(reports, './data/nuforc/processed/nuforc_reports.tsv', row.names=FALSE, quote=FALSE, sep='\t')
saveRDS(reports, './data/nuforc/processed/nuforc_reports.rds')

# Exploratory:
# shape-state
# duration-state
# hour-state
# year-state
# duration-shape
# hour-shape
# year-shape
# hour-duration
# year-duration

###########################
# Get unique location-dates
###########################

geodate <- reports %>%
  mutate(loc = paste(city, state, sep=', ')) %>%
  select(loc, date) %>%
  unique()

write.table(geodate, './data/nuforc/processed/geodate.tsv', row.names=FALSE,
            quote=FALSE, sep='\t', col.names=FALSE)




###########################################
# Regex to standardize durations to seconds
###########################################
  
word2digit <- c('one'='1', 'two'='2', 'three'='3', 'four'='4', 'five'='5',
                'six'='6', 'seven'='7', 'eight'='8', 'nine'='9', 'ten'='10')

CONTINUOUS <- 'ongoing|all night|on going|ongoing|contin|current|still|now|all day|nightly|present|every night|occurring|a while|weeks|all the time|in progress|everyday'

  
get_unit <- function(x) {
  x <- str_replace_all(x, '&lt;', '')
  if (str_detect(x, '\\d+-\\d+')){
      if(str_detect(x, 'sec|sec.|secs|secs.|second|\\d+s')) {
        unit <- 'range-second'
      } else if(str_detect(x, 'ninutes|min|min.|mins|mins.|minute|\\d+m')) {
        unit <- 'range-minute'
      }
      else if(str_detect(x, 'hr|hour|\\d+h')) {
        unit <- 'range-hour'
      }
  }
  else if(str_detect(x, 'sec|sec.|secs|secs.|second|\\d+s')) {
    unit <- 'second'
  }
  else if(str_detect(x, 'ninutes|min|min.|mins|mins.|minute|\\d+m')) {
    unit <- 'minute'
  }
  else if(str_detect(x, 'hr|hour|\\d+h')) {
    unit <- 'hour'
  }
  else if(str_detect(x, '^\\d+:\\d{2}:\\d{2}$')) {
    unit <- 'duration-hour'
  }
  else if(str_detect(x, '^\\d+:\\d{2}$')) {
    unit <- 'duration-minute'
  }
  else if(str_detect(x, '[a-zA-Z]') == FALSE) {
    unit <- 'none'
  }
  else if(is.na(x)) {
    unit <- NA
  }
  else if(x == '') {
    unit <- NA
  }
  else if( str_detect(x, CONTINUOUS) == TRUE ) {
    unit <- 'continuous'
  }
  else {
    unit <- 'OTHER'
  }
  return(unit)
}
get_unit_v <- Vectorize(get_unit)

avg_range <- function(x) {
  n1 <- as.numeric(str_extract(x, '\\d+(?=-)'))
  n2 <- as.numeric(str_extract(x, '(?<=-)\\d+'))
  return(mean(c(n1, n2)))
}
avg_range_v <- Vectorize(avg_range)

parse_duration <- function(x){
  hms <- as.numeric(str_extract_all(x, '\\d+')[[1]])
  h <- hms[1]
  m <- hms[2] / 60
  return( hms[1] + (hms[2] / 60) + (hms[3] / 60 /60) )
}
parse_duration_v <- Vectorize(parse_duration)

start <- data.frame(duration=reports$duration) %>%
  na.omit() %>%
  mutate(duration = tolower(duration),
         duration = str_remove_all(duration, '&lt;'),
         duration = str_replace_all(duration, 'around|approximately|approx.|approx|about', ''),
         duration = str_replace_all(duration, word2digit),
         unit = get_unit_v(duration),
         digit = case_when(unit %in% c('minute', 'second', 'hour') ~ as.numeric(str_extract(duration, '\\d+')),
                           str_starts(unit, 'range-') == TRUE ~ avg_range_v(duration),
                           unit == 'duration-minute' ~ as.numeric(str_extract(duration, '^\\d+')),
                           unit == 'duration-hour' ~ as.numeric(parse_duration_v(duration)),
                           unit == 'continuous' ~ 999,
                           TRUE ~ as.numeric(NA)
         )
         
  ) %>%
  mutate(duration_sec = case_when(unit == 'continuous' ~ 999999,
                                  unit == 'duration-hour' ~ digit*60*60,
                                  unit == 'duration-minute' ~ digit*60,
                                  unit == 'hour' ~ digit*60*60,
                                  unit == 'minute' ~ digit*60,
                                  unit %in% c('none', 'OTHER') ~ as.numeric(NA),
                                  unit == 'range-hour' ~ digit*60*60,
                                  unit == 'range-minute' ~ digit*60,
                                  unit == 'range-second' ~ digit,
                                  unit == 'second' ~ digit,
                                  TRUE ~ as.numeric(NA)))

# Examine 'none', 'OTHER'
# none: 4805
# Other: 2588


dff <- reports %>%
  tidyr::replace_na(list(duration = '')) %>%
  mutate(duration = tolower(duration),
         duration = str_remove_all(duration, '&lt;'),
         duration = str_replace_all(duration, 'around|approximately|approx.|approx|about', ''),
         duration = str_replace_all(duration, word2digit),
         unit = get_unit_v(duration),
         digit = case_when(unit %in% c('minute', 'second', 'hour') ~ as.numeric(str_extract(duration, '\\d+')),
                           str_starts(unit, 'range-') == TRUE ~ avg_range_v(duration),
                           unit == 'duration-minute' ~ as.numeric(str_extract(duration, '^\\d+')),
                           unit == 'duration-hour' ~ as.numeric(parse_duration_v(duration)),
                           unit == 'continuous' ~ 999,
                           TRUE ~ as.numeric(NA)
         )
  ) %>%
  mutate(duration_sec = case_when(unit == 'continuous' ~ 999999,
                                  unit == 'duration-hour' ~ digit*60*60,
                                  unit == 'duration-minute' ~ digit*60,
                                  unit == 'hour' ~ digit*60*60,
                                  unit == 'minute' ~ digit*60,
                                  unit %in% c('none', 'OTHER') ~ as.numeric(NA),
                                  unit == 'range-hour' ~ digit*60*60,
                                  unit == 'range-minute' ~ digit*60,
                                  unit == 'range-second' ~ digit,
                                  unit == 'second' ~ digit,
                                  TRUE ~ as.numeric(NA)))

# TODO: Some of the 00:xx are coming out as zero seconds, should be xx seconds!

########################
# Clean up and save data
########################

dff <- dff %>%
  select(-unit, -digit)

write.table(dff, './data/nuforc/processed/nuforc_reports.tsv', row.names=FALSE, quote=FALSE, sep='\t')
saveRDS(dff, './data/nuforc/processed/nuforc_reports.rds')
