library(tidyverse)
library(pcaMethods)
library(countrycode)

data <- read.csv('~/mortalityblob/dhs/wealthvars_clean.csv')

pca_vars <- c("water_source_drinking", "has_electricity", "has_radio", "has_television",
              "has_refrigerator", "has_bicycle", "has_motorcycle", "has_car", "has_telephone",
              "toilet_type", "material_wall", "material_floor", "material_roof",
              "number_sleeping_rooms")

options(na.action='na.pass')

sel <- data[ , pca_vars]

sel$number_sleeping_rooms <- sel$number_sleeping_rooms/max(sel$number_sleeping_rooms, na.rm=T)

#Get Wealth Factor from All Vars
mm <- model.matrix( ~ water_source_drinking + has_electricity + has_radio + has_television + has_refrigerator + has_bicycle + has_motorcycle + has_car + has_telephone + toilet_type + material_wall + material_floor + material_roof + number_sleeping_rooms, data=sel)

pca <- pca(mm, method = 'svdImpute', center=TRUE, nPcs = 1, verbose=TRUE)

data$wealth_factor_harmonized_all <- scores(pca)[ , 1]

#Get Wealth Factor from some efree
mm <- model.matrix( ~ water_source_drinking + has_radio + has_television + has_bicycle + has_motorcycle + has_car + has_telephone + toilet_type + material_wall + material_floor + material_roof + number_sleeping_rooms, data=sel)

pca <- pca(mm, method = 'svdImpute', center=TRUE, nPcs = 1, verbose=TRUE)

data$wealth_factor_harmonized_efree1 <- scores(pca)[ , 1]

#Get Wealth Factor from all efree
mm <- model.matrix( ~ water_source_drinking + has_bicycle + has_motorcycle + has_car + toilet_type + material_wall + material_floor + material_roof + number_sleeping_rooms, data=sel)

pca <- pca(mm, method = 'svdImpute', center=TRUE, nPcs = 1, verbose=TRUE)

data$wealth_factor_harmonized_efree2 <- scores(pca)[ , 1]

#Write results
writedat <- data %>%
  mutate(urban_rural = ifelse(urban, 'Urban', 'Rural')) %>%
  group_by(latitude, longitude, code, urban_rural) %>%
  summarize(interview_year = max(survey_year),
            interview_month = max(survey_month),
            wealth_factor_original = mean(wealth_factor, na.rm=T),
            wealth_factor_harmonized_all = -mean(wealth_factor_harmonized_all),
            wealth_factor_harmonized_efree1 = -mean(wealth_factor_harmonized_efree1),
            wealth_factor_harmonized_efree2 = -mean(wealth_factor_harmonized_efree2),
            has_electricity = mean(has_electricity)) %>%
  filter(!is.na(latitude)) %>%
  mutate(survey_code = substr(code, 1, 6),
         country_iso = countrycode(substr(code, 1, 2), 'dhs', 'iso3c')) %>%
  data.frame

write.csv(writedat, '~/mortalityblob/dhs/wealth_export_efree.csv', row.names=F)
