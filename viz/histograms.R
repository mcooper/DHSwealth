library(tidyverse)
library(countrycode)
library(sf)
library(rnaturalearth)

d <- read.csv('~/mortalityblob/dhs/wealth_export_chi.csv', stringsAsFactors=F)

d2 <- d %>%
  group_by(iso3, survey, urban_rural) %>%
  summarize(poverty_hci = mean(in_poverty) * n()) %>%
  spread(urban_rural, poverty_hci) %>%
  mutate(perc.poor.rural = Rural/(Urban + Rural))

d3 <- d2 %>%
  ungroup %>%
  group_by(iso3) %>%
  filter(survey == max(survey))

cty <- ne_countries(returnclass='sf') %>%
  filter(sovereignt != 'Antarctica')

cty <- merge(cty, d3, by.x='iso_a3', by.y='iso3', all.x=T)

ggplot(cty) + 
  geom_sf(aes(fill=perc.poor.rural)) + 
  theme_void()
ggsave('~/DHSwealth/res/perc.poor.rural.png', width=8, height=4)

d5 <- d %>%
  filter(survey == 'AO5')

ggplot(d5) + 
  geom_histogram(aes(x=wfh2, fill=urban_rural)) + 
  geom_vline(aes(xintercept=-0.466156), linetype=2)
ggsave('~/DHSwealth/res/example_histogram.png', width=8, height=4)
