# Crop population density raster to continental U.S.
#
# Population density tif from WorldPop: https://www.worldpop.org/geodata/summary?id=39710
# Bounding box for continental U.S.: https://www.sciencebase.gov/catalog/file/get/57753ebee4b07dd077c70868?f=__disk__ce%2Fa3%2Fd1%2Fcea3d1f177d07b812253f9164e3275d5923280b8&transform=1&allowOpen=true

library(dplyr)
library(ggplot2)
library(raster)

setwd('~/nuforc_stats/notebooks/')

pop <- raster(x='../data/pop/raw/usa_pd_2000_1km.tif')

# Crop data to continental U.S. only
pop_usa <- crop(pop, extent(-127.922322113, -65.023446015, 22.592853684, 50.630121493))
# plot(log(pop_usa))

writeRaster(pop_usa , '../data/pop/processed/pop_density2000.tif')
