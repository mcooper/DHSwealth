library(tidyverse)
library(countrycode)

setwd('~/mortalityblob/dhs/')

options(stringsAsFactors=F)

dhs <- read.csv('wealthvars_clean.csv')
mics <- read.csv('mics_clean.csv')
geo <- read.csv('wealthvars_geo.csv')

dhs <- dhs[!substr(dhs$country, 1, 2) == 'OS', ]

dhs$survey <- dhs$country
dhs$country <- countrycode(substr(dhs$country, 1, 2), 'dhs', 'country.name')
dhs$program <- "DHS"
dhs$hhid <- dhs$householdno
dhs$urban_rural <- ifelse(dhs$urban, 'Urban', 'Rural')
dhs$has_geo <- dhs$code %in% geo$code

mics$program <- "MICS"
mics$has_geo <- FALSE

mics$has_electricity <- mics$has_electricity == 'Yes'
mics$has_radio <- mics$has_radio == 'Yes'
mics$has_television <- mics$has_television == 'Yes'
mics$has_car <- mics$has_car == 'Yes'
mics$has_refrigerator <- mics$has_refrigerator == 'Yes'
mics$has_bicycle <- mics$has_bicycle == 'Yes'
mics$has_motorcycle <- mics$has_motorcycle == 'Yes'

mics$country[grepl('Ivoire 2016', mics$survey)] <- 'Côte d’Ivoire'
mics$country[grepl('DATA MYANMAR', mics$survey)] <- 'Myanmar'
mics$country[grepl('LSIS 2011', mics$survey)] <- 'Laos'
mics$country[grepl('Gambia 2010', mics$survey)] <- 'The Gambia'
mics$country[grepl('St.Lucia 2012', mics$survey)] <- 'St Lucia'
mics$country[grepl('Zimbabwe', mics$survey)] <- 'Zimbabwe'

mics$country <- countrycode(mics$country, 'country.name', 'country.name')

dhs <- dhs[ , names(dhs) %in% names(mics)]

both <- bind_rows(mics, dhs)

both <- both %>%
  filter(!is.na(urban_rural))

getmode <- function(v) {
   uniqv <- unique(v)
   uniqv[which.max(tabulate(match(v, uniqv)))]
}

#get survey single year
yeardat <- both %>%
  group_by(survey) %>%
  summarize(survey_single_year = getmode(survey_year))

yeardat$survey_single_year[yeardat$survey == 'CO2'] <- 1990

both <- merge(both, yeardat)

both$survey <- paste0(both$country, ' ', both$survey_single_year, ' ', both$program)

write.csv(both, 'mics_dhs_wealth.csv', row.names=F)
