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

all_tbls <- list()

for (i in 0:20) {

  j <- i + 2000
  print(j)
  
  url <- paste("https://fred.stlouisfed.org/release/tables?rid=118&eid=259194&od=", j, "-01-01", sep='')
  tbl <- htmltab(doc = url)
  
  colnames(tbl) <- c('state', 'population', 'x', 'y')
  
  tbl2 <- tbl %>%
    select(state, population) %>%
    mutate(year=j,
           population=1000*as.numeric(gsub(',', '', population)),
           state_abr = recode(state, !!!st_mapping))
  
  all_tbls[[i+1]] = tbl2
  
}

df <- do.call(rbind, all_tbls)

write.csv(df, '../data/census/processed/state_pops.csv', row.names=FALSE)
