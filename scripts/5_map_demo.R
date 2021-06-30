library(dplyr)
library(ggplot2)
library(sf)
library(maptools)
library(raster)
library(spatstat)
library(USAboundaries)

setwd('~/nuforc_stats/notebooks/')

df <- readRDS('../data/nuforc/processed/nuforc_reports.rds') %>%
  mutate(loc = tolower(paste(city, state, sep=', ')))

####
# MA
####

# Bring in Massachusets shape file
s <- us_states(states='Massachusetts')

s_one <- st_union(s)  # can also use st_geometry to maintain inner boundaries!
# s_geo <- st_geometry(s)
s_flat <- st_transform(s_one, crs='+proj=utm +zone=18 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0')
# NOTE: To get the right coordinate system of Starbucks, use: st_crs(Starbucks) and then user the user input
w <- as.owin(as_Spatial(s_flat))
# w.km <- rescale(w, 1000)

# Load MA observations
obs <- df %>%
  filter(state == 'MA') %>%
  select(city_longitude, city_latitude, id) %>%
  rename(lon='city_longitude',
         lat='city_latitude') %>%
  mutate(lon=jitter(lon, 150),
         lat=jitter(lat, 150))

# Convert the dataframe to a spatial object. Note that the
# crs=4326 parameter assigns a WGS84 coordinate system to the 
# spatial object
obs.sf <- st_as_sf(obs, coords = c("lon", "lat"), crs=4326)
obs.sf.utm <- st_transform(obs.sf, '+proj=utm +zone=18 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0') # project from geographic to UTM
obs.sp  <- as(obs.sf.utm, "Spatial")      # Create Spatial* object
obs.ppp <- as(obs.sp, "ppp")              # Create ppp object

marks(obs.ppp) <- NULL

Window(obs.ppp) <- w

plot(obs.ppp)



#############
# Entire U.S.
#############

# Entire U.S.
# Continental U.S. only
STATES <- c('Alabama', 'Maine', 'Pennsylvania', 'Maryland', 'Rhode Island',
            'Arizona', 'Massachusetts', 'South Carolina', 'Arkansas',
            'Michigan', 'South Dakota', 'California', 'Minnesota', 'Tennessee',
            'Colorado', 'Mississippi', 'Texas', 'Connecticut', 'Missouri',
            'Utah', 'Delaware', 'Montana', 'Vermont', 'District of Columbia',
            'Nebraska', 'Virginia', 'Florida', 'Nevada', 'Washington', 'Georgia',
            'New Hampshire', 'West Virginia', 'New Jersey', 'Wisconsin', 'Idaho',
            'New Mexico', 'Wyoming', 'Illinois', 'New York', 'American Samoa',
            'Indiana', 'North Carolina', 'Iowa', 'North Dakota', 'Kansas',
            'Ohio', 'Kentucky', 'Oklahoma', 'Louisiana', 'Oregon')

s <- us_boundaries(type='state', states=STATES)

s_one <- st_geometry(s)  # can also use st_geometry to maintain inner boundaries!
# s_geo <- st_geometry(s)
s_flat <- st_transform(s_one, crs='+proj=utm +zone=15 +datum=WGS84')
# NOTE: To get the right coordinate system of Starbucks, use: st_crs(Starbucks) and then user the user input
w <- as.owin(as_Spatial(s_flat))
# w.km <- rescale(w, 1000)

# Load MA observations
obs <- df %>%
  dplyr::select(city_longitude, city_latitude, id) %>%
  rename(lon='city_longitude',
         lat='city_latitude') %>%
  mutate(lon=jitter(lon, 10),
         lat=jitter(lat, 10))

# Convert the dataframe to a spatial object. Note that the
# crs=4326 parameter assigns a WGS84 coordinate system to the 
# spatial object
obs.sf <- st_as_sf(obs, coords = c("lon", "lat"), crs=4326)
obs.sf.utm <- st_transform(obs.sf, '+proj=utm +zone=15 +datum=WGS84') # project from geographic to UTM
obs.sp  <- as(obs.sf.utm, "Spatial")      # Create Spatial* object
obs.ppp <- as(obs.sp, "ppp")              # Create ppp object

marks(obs.ppp) <- NULL
Window(obs.ppp) <- w

plot(obs.ppp, main='UAP sightings (2000-2020)')



##########################################
# Quadrat density on a tessellated surface
##########################################


pop <- raster(x='../data/pop/processed/pop_density2000.tif')
pop <- projectRaster(pop, crs=crs(obs.sp))  # reproject pop density raster to match observations

pop_im <- as.im(log(pop+1))

brk  <- c( -Inf, 2, 4, 6, 8, 10, 12, Inf)  # Define the breaks  c( -Inf, 4, 6, 8 , Inf)
Zcut <- cut(pop_im, breaks=brk, labels=1:7)  # Classify the raster
E    <- tess(image=Zcut)  # Create a tesselated surface
plot(E, main="", las=1)

Q   <- quadratcount(obs.ppp, tess = E)  # Tally counts
Q.d <- intensity(Q)  # Compute density
Q.d
# ^ umber of points per square meter within each quadrat unit.

plot(intensity(Q, image=TRUE)*10^4, las=1, main=NULL)
# plot(obs.ppp, pch=20, cex=0.6, col=rgb(1,1,1,.5), add=TRUE)


K1 <- density(obs.ppp) # Using the default bandwidth
plot(K1, main=NULL, las=1)
contour(K1, add=TRUE)

K2 <- density(obs.ppp, sigma=100*1000) # Using a 50km bandwidth
plot(K2, main=NULL, las=1)
contour(K2, add=TRUE)


#######################################
# Kernel density adjusted for covariate
#######################################

# Idk if this is very interesting!?

# Compute rho using the ratio method
rho <- rhohat(obs.ppp, pop_im,  method="ratio")
# Generate rho vs covariate plot
plot(rho, las=1, main=NULL, legendargs=list(cex=0.8, xpd=TRUE, inset=c(1.01, 0) ))
#  describe/explore the shape of the relationship between point density and covariate.
# intensity increases with population density

pred <- predict(rho)
cl   <- interp.colours(c("lightyellow", "orange" ,"red"), 100) # Create color scheme
plot(pred, col=cl, las=1, main=NULL, gamma = 0.25)

###
#
###

# Model 

