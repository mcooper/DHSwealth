library(tidyverse)
library(pcaMethods)
library(countrycode)

data <- read.csv('~/mortalityblob/dhs/wealthvars_clean.csv')

data <- data %>%
  filter(!is.na(urban)) %>%
  filter(!surveycode %in% c('JO-2-1', 'NG-2-1', 'PY-2-1'))

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

data$wfh <- scores(pca)[ , 1]

#Get Wealth Factor from All Vars
mm <- model.matrix( ~ water_source_drinking + has_electricity + has_radio + has_television + has_refrigerator + has_bicycle + has_motorcycle + has_car + has_telephone + toilet_type + number_sleeping_rooms, data=sel)

pca <- pca(mm, method = 'svdImpute', center=TRUE, nPcs = 1, verbose=TRUE)

data$wfh2 <- scores(pca)[ , 1]

# d <- data %>%
#   group_by(surveycode, urban) %>%
#   summarize(wfh=mean(wfh),
#             wfh2=mean(wfh2)) %>%
#   gather(method, value, -surveycode, -urban) %>%
#   spread(urban, value) %>%
#   mutate(gap = `FALSE` - `TRUE`) %>%
#   data.frame


write.csv(data, '~/mortalityblob/dhs/wealth_export.csv', row.names=F)
