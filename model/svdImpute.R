library(tidyverse)
library(pcaMethods)
library(countrycode)

data <- read.csv('~/mortalityblob/dhs/mics_dhs_wealth.csv')

data$nas <- rowSums(is.na(data))

data <- data %>%
  filter(nas <= 5)

options(na.action='na.pass')

data$number_sleeping_rooms <- data$number_sleeping_rooms/max(data$number_sleeping_rooms, na.rm=T)

#Get Wealth Factor from All Vars
mm <- model.matrix( ~ water_source_drinking + has_electricity + has_radio + has_television + has_refrigerator + has_bicycle + has_motorcycle + has_car + toilet_type + material_wall + material_floor + material_roof + number_sleeping_rooms, data=data)

pca <- pca(mm, method = 'svdImpute', center=TRUE, nPcs = 1, verbose=TRUE)

data$wfh <- scores(pca)[ , 1]

#Get Wealth Factor from Vars robust to urban-rural differences
mm <- model.matrix( ~ water_source_drinking + has_electricity + has_radio + has_television + has_refrigerator + has_bicycle + has_motorcycle + has_car + has_telephone + toilet_type + number_sleeping_rooms, data=data)

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
