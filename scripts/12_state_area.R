library(htmltab)
library(dplyr)

setwd("/Users/benjamin.horvaththedailybeast.com/nuforc_stats/notebooks")

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

url <- 'https://en.wikipedia.org/wiki/List_of_U.S._states_and_territories_by_area'
tbl <- htmltab(doc = url)

colnames(tbl) <- c('state', 'total_area_rank', 'total_area_sqmi', 'total_area_sqkm',
                   'land_area_rank', 'land_area_sqmi', 'land_area_sqkm',
                   'prop_land', 'water_area_rank', 'water_area_sqmi', 
                   'water_area_sqkm', 'prop_water')

tbl2 <- tbl %>%
  mutate(state = str_squish(state)) %>%
  filter(state %in% names(st_mapping)) %>%
  mutate_all(str_remove_all, ',|%') %>%
  mutate(prop_land = as.numeric(prop_land) / 100,
         prop_water = as.numeric(prop_water) / 100) %>%
  mutate(state_abr = recode(state, !!!st_mapping))

write.csv(tbl2, '../data/census/processed/state_area.csv', row.names=FALSE)

####

# bullshit:

dff <- df %>%
  mutate(madar = stringr::str_detect(text, 'MADAR')) %>%
  filter(madar == FALSE) %>%
  group_by(year, state) %>%
  summarise(n=n())

ggplot(dff, aes(x=year, y=n)) +
  facet_wrap(~state, scales='free') +
  geom_line() +
  geom_smooth(se=FALSE, method='lm', formula=y~poly(x, 2))

dfff <- pop %>%
  right_join(dff, by=c('state_abr'='state', 'year')) %>%
  left_join(area, by='state_abr')

# Center
X <- dfff %>%
  select(state_abr, year, n, population, land_area_sqmi, total_area_sqmi, prop_water)
           
dfff$land_area_sqmi <- as.numeric(dfff$land_area_sqmi )
            
m <- lmer(log(n) ~ log(population)*log(land_area_sqmi) + (1 | state_abr) + (1 | year), X)


### ABSOLUTELY log then scale

XX <- X %>%
  mutate(density = population / land_area_sqmi)

state_caps <- read.csv('../data/state_capitals.csv', stringsAsFactors=FALSE) %>%
  mutate(state_abr = recode(name, !!!st_mapping)) %>%
  select(-name, -capital)

XX <- XX %>%
  inner_join(state_caps, by='state_abr') %>%
  mutate(total_area_sqmi = as.numeric(total_area_sqmi)) %>%
  mutate(n = scale(log(n+1), scale=FALSE),
         density = scale(log(density), scale=FALSE),
         population = scale(log(population+1), scale=FALSE),
         land_area_sqmi = scale(log(land_area_sqmi+1), scale=FALSE),
         latitude = scale(latitude),
         longitude = scale(longitude),
         total_area_sqmi = scale(log(total_area_sqmi+1), scale=FALSE),
         prop_water = scale(prop_water, scale=FALSE))

m2 <- lmer(n ~ density*land_area_sqmi + latitude + longitude + (1 | state_abr) + (1 | year), XX)

m_null <- lmer(n ~ 1 + (1 | state_abr) + (1 | year), XX)

XX$pred <- predict(m2, XX)
# XX$r <- resid(m2)

XX_long <- XX %>%
  select(state_abr, year, n, pred) %>%
  tidyr::pivot_longer(!c(state_abr, year), names_to='sightings', values_to='x')
  
  pivot_longer(!religion, names_to = "income", values_to = "count")

ggplot(XX_long, aes(x=year, y=x, colour=sightings)) +
  facet_wrap(~state_abr, scales='free') +
  geom_line()


### Predict 2021

v <- XX %>%
  filter(year == 2020) %>%
  select(-n) %>%
  mutate(year = 2021)

v$pred <- predict(m2, newdata=v, allow.new.levels = TRUE)

v$pred <- exp(v$pred + 3.448828)
