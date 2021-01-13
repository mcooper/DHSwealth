library(tidyverse)
library(pcaMethods)

data <- read.csv('~/mortalityblob/dhs/wealthvars_clean.csv')

pca_vars <- c("water_source_drinking", "has_electricity", "has_radio", "has_television",
              "has_refrigerator", "has_bicycle", "has_motorcycle", "has_car", "has_telephone",
              "toilet_type", "material_wall", "material_floor", "material_roof",
              "number_sleeping_rooms")

options(na.action='na.pass')

sel <- data[ , pca_vars]

sel$number_sleeping_rooms <- sel$number_sleeping_rooms/max(sel$number_sleeping_rooms, na.rm=T)

mm <- model.matrix( ~ water_source_drinking + has_electricity + has_radio + has_television + has_refrigerator + has_bicycle + has_motorcycle + has_car + has_telephone + toilet_type + material_wall + material_floor + material_roof + number_sleeping_rooms, data=sel)

pca <- pca(mm, method = 'svdImpute', center=TRUE, nPcs = 1, verbose=TRUE)

data$wealth_factor_harmonized <- scores(pca)

d <- data %>%
  group_by(surveycode) %>%
  summarize(cc = cor(wealth_factor, wealth_factor_harmonized),
            mm = mean(wealth_factor_harmonized))
