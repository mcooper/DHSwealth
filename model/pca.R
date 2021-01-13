library(tidyverse)

data <- read.csv('~/mortalityblob/dhs/wealthvars_clean.csv')

data <- na.omit(data %>% select(-water_source_nondrinking))

mm <- model.matrix( ~ water_source_drinking + has_electricity + has_radio + has_television + has_refrigerator + has_bicycle + has_motorcycle + has_car + has_telephone + toilet_type + material_wall + material_floor + material_roof, data=data)

pca <- princomp(mm)

data$wealth_factor_harmonized <- pca$scores[ , c('Comp.1')]

d <- data %>%
  group_by(surveycode) %>%
  summarize(cc = cor(wealth_factor, wealth_factor_harmonized),
            mm = mean(wealth_factor_harmonized))
