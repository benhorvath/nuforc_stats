# Load and process 2019 data from the magnetic observatory in Fredericksburg,
# Virginia, a member of INTERMAGNET:
# https://www.usgs.gov/natural-hazards/geomagnetism/science/fredericksburg-frd?qt-science_center_objects=0#qt-science_center_objects
#
# XYZ are in nanoteslas: https://www.intermagnet.org/data-donnee/formats/imfv122-eng.php
# Divibe by 1000 to get microtesla: https://en.wikipedia.org/wiki/Orders_of_magnitude_(magnetic_field)

setwd('~/nuforc_stats/notebooks/')

library(dplyr)
library(lubridate)

raw_dir <- '../data/magnetic/raw'
files <- list.files(path=raw_dir, pattern="*.min", full.names=TRUE)

dfs <- lapply(files, function(f) read.table(f, sep='', skip=25, header=FALSE) )

df <- do.call(rbind, dfs) %>%
  magrittr::set_colnames(c('dt', 't', 'doy', 'frdx', 'frdy', 'frdz', 'frdg')) %>%
  mutate(dt = ymd_hms(paste(dt, t, ' '))) %>%
  select(-t)

dff <- df %>%
  tidyr::pivot_longer(!dt:doy, names_to='axis', values_to='measure') %>%
  arrange(dt, axis)

ggplot(dff, aes(x=dt, y=log(measure))) +
  geom_line() +
  facet_wrap(~ axis, scales='free') +
  theme_light()

write.csv(dff, '../data/magnetic/processed/va_readings.csv', row.names=FALSE)

# observations <- observations %>%
#   mutate(date_time_est = ymd_hms(date_time, tz = "America/New_York"),
#          date_time_utm = with_tz(date_time_est, tzone='UTC'))
# 
# observations %>% select(date_time, date_time_est, date_time_utm) %>% head
