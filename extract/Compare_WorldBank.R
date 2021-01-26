library(tidyverse)
library(countrycode)
library(zoo)

data <- read.csv('~/mortalityblob/dhs/wealth_export.csv')

data$iso3 <- countrycode(data$country, 'country.name', 'iso3c')
data$year <- data$survey_year

wb <- read.csv('~/mortalityblob/dhs/API_SI.POV.DDAY_DS2_en_csv_v2_1928965.csv', skip=4) %>%
  select(-`Country.Name`, -`Indicator.Name`, -`Indicator.Code`) %>%
  gather(year, poverty_headcount, -Country.Code) %>%
  mutate(year = as.numeric(substr(year, 2, 5)),
         iso3 = countrycode(Country.Code, 'wb', 'iso3c')) %>%
  select(-Country.Code) %>%
  group_by(iso3) %>%
  filter(!all(is.na(poverty_headcount))) %>%
  mutate(poverty_headcount = na.approx(poverty_headcount, na.rm=F))

comb <- merge(data, wb)

len(tab(comb$survey))
len(tab(comb$survey[!is.na(comb$poverty_headcount)]))

comb <- comb %>%
  group_by(survey) %>%
  mutate(in_poverty = wfh2 < quantile(wfh2, probs=mean(poverty_headcount, na.rm=T)/100))

comb2 <- comb %>%
  select(iso3, year, urban_rural, hhweight, country, survey, program, wfh, wfh2, 
         poverty_headcount, in_poverty)

write.csv(comb2, '~/mortalityblob/dhs/wealth_export_chi.csv', row.names=F)
