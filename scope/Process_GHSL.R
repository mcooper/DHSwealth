library(tidyverse)
library(imputeTS)

data <- read.csv('~/mortalityblob/dhs/wealthvars_built_raw.csv')
year <- read.csv('~/mortalityblob/dhs/wealthvars_clean.csv') %>%
  select(code, survey_year) %>%
  group_by(code) %>%
  summarize(survey_year = max(survey_year))

data <- merge(data, year, all=F)

data$total <- rowSums(data %>% select(X2, X3, X4, X5, X6))

#Get Percent
data <- data %>%
  mutate(X1 = X1/total,
         X2 = X2/total,
         X3 = X3/total,
         X4 = X4/total,
         X5 = X5/total,
         X6 = X6/total)

#Get Percent cover in 1975, 1990, 2000, and 2014
data <- data %>%
  mutate(perc1975 = X6,
         perc1990 = X6 + X5,
         perc2000 = X6 + X5 + X4,
         perc2014 = X6 + X5 + X4 + X3) %>%
  select(code, survey_year, matches('perc'))

fill <- data %>%
  filter(is.nan(perc1975)) %>%
  gather(year, value, -code, -survey_year) %>%
  mutate(year = as.numeric(substr(year, 5, 8))) %>%
  merge(expand.grid(list(code=unique(data$code), year=seq(1990, 2020))), all=T)

fill2014 <- fill %>%
  filter(year <= 2014) %>%
  group_by(code) %>%
  mutate(value = na_interpolation(value))

fill <- fill %>%
  group_by(code, year) %>%


#Function to Interpolate between years
impute <- function(syear, y1975, y1990, y2000, y2014)
