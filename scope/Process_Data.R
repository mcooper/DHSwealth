library(tidyverse)

data <- read.csv('~/mortalityblob/dhs/wealthvars_raw.csv')

####################
#Clean Variables
####################

#Possessions
for (var in c("has_electricity", "has_radio", "has_television",
              "has_refrigerator", "has_bicycle", "has_motorcycle", "has_car",
              "has_telephone")){
  data[ , var][data[ , var] == 9] <- NA
  data[ , var] <- data[ , var] == 1
  data[ , paste0(var, '_chr')] <- NULL
}

#Water, toilets
data$water_source_nondrinking <- case_when(data$water_source_nondrinking < 20 ~ 'piped',
                                           data$water_source_nondrinking < 30 ~ 'tube well',
                                           data$water_source_nondrinking < 40 ~ 'dug well',
                                           data$water_source_nondrinking < 60 ~ 'surface water',
                                           data$water_source_nondrinking < 80 ~ 'purchased',
                                           data$water_source_nondrinking < 100 ~ NA_character_)
data$water_source_nondrinking_chr <- NULL

data$water_source_drinking <- case_when(data$water_source_drinking < 20 ~ 'piped',
                                        data$water_source_drinking < 30 ~ 'tube well',
                                        data$water_source_drinking < 40 ~ 'dug well',
                                        data$water_source_drinking < 60 ~ 'surface water',
                                        data$water_source_drinking < 90 ~ 'purchased',
                                        data$water_source_drinking < 100 ~ NA_character_)
data$water_source_drinking_chr <- NULL

data$toilet_type <- case_when(data$toilet_type < 20 ~ 'flush toilet',
                              data$toilet_type < 30 ~ 'pit latrine',
                              data$toilet_type < 40 ~ 'no facility',
                              data$toilet_type < 90 ~ 'other',
                              data$toilet_type < 100 ~ NA_character_)
data$toilet_type_chr <- NULL

#Materials
data$material_wall <- case_when(data$material_wall < 20 ~ "natural",
                                data$material_wall < 30 ~ 'rudimentary',
                                data$material_wall < 40 ~ 'finished',
                                data$material_wall < 90 ~ 'other',
                                data$material_wall < 100 ~ NA_character_)
data$material_wall_chr <- NULL

data$material_floor <- case_when(data$material_floor < 20 ~ "natural",
                                data$material_floor < 30 ~ 'rudimentary',
                                data$material_floor < 40 ~ 'finished',
                                data$material_floor < 90 ~ 'other',
                                data$material_floor < 100 ~ NA_character_)
data$material_floor_chr <- NULL


data$material_roof <- case_when(data$material_roof < 20 ~ "natural",
                                data$material_roof < 30 ~ 'rudimentary',
                                data$material_roof < 40 ~ 'finished',
                                data$material_roof < 90 ~ 'other',
                                data$material_roof < 100 ~ NA_character_)
data$material_roof_chr <- NULL

#Sleeping rooms is already good
data$number_sleeping_rooms_chr <- NULL

#Urban-Rural
data$urban <- data$urban_rural == 1
data$urban[data$urban_rural == 3] <- NA

data$urban_rural <- NULL
data$urban_rural_chr <- NULL

#Dates
data$date_cmc <- data$survey_month
data$date_cmc[data$date_cmc > 1500] <- data$date_cmc[data$date_cmc > 1500] - 681

data$survey_year <- 1900 + floor((data$date_cmc - 1)/12)
data$survey_month <- data$date_cmc - 12 * (data$survey_year - 1900)

###############################
# Save Data
##############################

#Write Geospatial Data
write.csv(data %>% select(code, latitude, longitude, survey_year) %>%  unique,
          '~/mortalityblob/dhs/wealthvars_geo.csv', row.names=F)

final <- data %>%
  select(code, householdno, wealth_quintile, wealth_factor, survey_month, 
         water_source_nondrinking, water_source_drinking, has_electricity, has_radio,
         has_television, has_refrigerator, has_bicycle, has_motorcycle, has_car, has_telephone,
         toilet_type, material_wall, material_floor, material_roof, number_sleeping_rooms,
         surveycode, latitude, longitude, urban, survey_year)

write.csv(final, '~/mortalityblob/dhs/wealthvars_clean.csv', row.names=F)


