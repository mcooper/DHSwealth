library(tidyverse)
library(countrycode)

setwd('~/mortalityblob/dhs/')

dhs <- read.csv('wealthvars_clean.csv')
mics <- read.csv('mics_clean.csv')

dhs <- dhs[!substr(dhs$country, 1, 2) == 'OS', ]

dhs$survey <- dhs$country
dhs$country <- countrycode(substr(dhs$country, 1, 2), 'dhs', 'country.name')
dhs$program <- "DHS"
dhs$hhid <- dhs$householdno
dhs$hhweight <- dhs$site_weight
dhs$urban_rural <- ifelse(dhs$urban, 'Urban', 'Rural')
dhs$has_geo <- !is.na(dhs$latitude)

mics$program <- "MICS"
mics$has_geo <- FALSE

mics$has_electricity <- mics$has_electricity == 'Yes'
mics$has_radio <- mics$has_radio == 'Yes'
mics$has_television <- mics$has_television == 'Yes'
mics$has_car <- mics$has_car == 'Yes'
mics$has_refrigerator <- mics$has_refrigerator == 'Yes'
mics$has_bicycle <- mics$has_bicycle == 'Yes'
mics$has_motorcycle <- mics$has_motorcycle == 'Yes'

dhs <- dhs[ , names(dhs) %in% names(mics)]

both <- bind_rows(mics, dhs)

both <- both %>%
  filter(!is.na(urban_rural))

write.csv(both, 'mics_dhs_wealth.csv', row.names=F)
