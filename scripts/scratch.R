# Density is in person per square kilometer
cities  <- read.csv('~/Downloads/simplemaps_uscities_basicv1/uscities.csv', stringsAsFactors=FALSE)

#### Model:

# AREA DATA IS HERE: https://www2.census.gov/geo/docs/maps-data/data/gazetteer/2019_Gazetteer/
# data from 2012--2020
# Download entire thing "places": https://www.census.gov/geographies/reference-files/time-series/geo/gazetteer-files.html
# Citation:  "2019 U.S. Gazetteer Files". United States Census Bureau. Retrieved June 30, 2020.

####
# Gazateer
###

places <- read.csv('../data/census/raw/2020_Gaz_place_national.tsv', sep='\t',
                   stringsAsFactors=FALSE) %>%
  janitor::clean_names() %>%
  rename(state='usps',
         city='name')

appellations <- 'CDP|city|town|municipality|borough|NA|village|government|County|county|township|corporation|comunidad|urban'

places <- places %>%
  mutate(city = str_trim(str_remove(city, appellations)))

dff <- df %>%
  mutate(city = case_when(tolower(city) == 'brooklyn' ~ 'New York City',
                                     tolower(city) == 'queens' ~ 'New York City',
                                     tolower(city) == 'staten island' ~ 'New York City',
                                     TRUE ~ city)) %>%
  mutate(madar = stringr::str_detect(text, 'MADAR')) %>%
  filter(madar == FALSE) %>%
  group_by(state, city) %>%
  summarise(n=n()) %>%
  inner_join(places, by=c('state', 'city')) %>%
  inner_join(cities, by=c('state'='state_id', 'city'))

# plots
ggplot(dff, aes(x=population, y=n)) +
  geom_point() +
  geom_smooth()

ggplot(dff, aes(x=log(population), y=n)) +
  geom_point() +
  geom_smooth()

ggplot(dff, aes(x=log(population), y=log(n))) +
  geom_point() +
  geom_smooth()

ggplot(dff, aes(x=aland_sqmi, y=n)) +
  geom_point() +
  geom_smooth()

ggplot(dff, aes(x=log(aland_sqmi), y=log(n))) +
  geom_point() +
  geom_smooth()

m <- lm(log(n) ~ log(population) + log(aland_sqmi), dff)

m2 <- lm(log(n) ~ log(population) * log(aland_sqmi), dff)
# Interpreation is fucked up, see: https://www.theanalysisfactor.com/interpreting-interactions-in-regression/
# B1 is how much we expect n to change for a one-unit change in population when area = 0 <-- Interpret for log-log
# B2 is how much we expect n to change for a one-unit change in area when population = 0 <-- Interpret for log-log
# B3 is the rate at which the slope of X1 changes as X2 changes

# Compare these two, it's clear that m2 is better:
# scatter.smooth(log(dff$population)*log(dff$aland_sqmi), resid(m))
# scatter.smooth(log(dff$population)*log(dff$aland_sqmi), resid(m2))

wts <- 1/fitted( lm(abs(residuals(m2))~fitted(m2)) )^2
m3 <- lm(log(n) ~ log(population) * log(aland_sqmi), dff, weights=wts)

# TODO: Potentially region is important too

# just go with m2



dff$pred <- exp(predict(m2))
dff$r <- dff$n - dff$pred
dff$times <- dff$n / dff$pred

dff <- dff %>%
  arrange(desc(times))

# dff %>% select(state, city, n, population, aland_sqmi, pred, r, times) %>% View

# Graphical demonstration
# land  area: 0, 5, 10, 25, 40
X1 <- data.frame(population=seq(1000, 200000, 5000),
                 aland_sqmi=rep(0.5, 40))
X1$pred <- exp(predict(m2, X1))

X2 <- data.frame(population=seq(1000, 200000, 5000),
                 aland_sqmi=rep(5, 40))
X2$pred <- exp(predict(m2, X2))

X3 <- data.frame(population=seq(1000, 200000, 5000),
                 aland_sqmi=rep(10, 40))
X3$pred <- exp(predict(m2, X3))

X4 <- data.frame(population=seq(1000, 200000, 5000),
                 aland_sqmi=rep(25, 40))
X4$pred <- exp(predict(m2, X4))

X5 <- data.frame(population=seq(1000, 200000, 5000),
                 aland_sqmi=rep(40, 40))
X5$pred <- exp(predict(m2, X5))

X <- do.call(rbind, list(X1, X2, X3, X4, X5)) %>%
  mutate(aland_sqmi = as.factor(aland_sqmi))


ggplot(X, aes(x=population, y=pred, colour=aland_sqmi)) + geom_line() + theme_light()
