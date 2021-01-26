library(tidyverse)
library(countrycode)
library(sf)
library(rnaturalearth)

dat <- read.csv('~/mortalityblob/dhs/mics_dhs_wealth.csv')

getmode <- function(v) {
   uniqv <- unique(v)
   uniqv[which.max(tabulate(match(v, uniqv)))]
}

sel <- dat %>%
  group_by(survey, program, has_geo, country) %>%
  summarize(year = getmode(survey_year),
            n = n())

sel$group <- case_when(sel$program == 'MICS' ~ 'MICS',
                       sel$has_geo ~ 'DHS with GPS',
                       TRUE ~ 'DHS without GPS')

sel$year[sel$year == 1952] <- 1996

ggplot(sel) + 
  geom_bar(aes(x=year, y=..count.., fill=group)) +
  labs(title = 'Number of Available Surveys', fill ='')
ggsave('~/DHSwealth/res/Survey_count_time.png', width=8, heigh=4)

cty <- ne_countries(returnclass='sf')

sel2 <- sel %>%
  mutate(iso_a3 = countrycode(country, 'country.name', 'iso3c')) %>%
  group_by(iso_a3) %>%
  summarize(survey_count=n())

cty2 <- merge(cty, sel2, all.x=T) %>%
  filter(sovereignt != 'Antarctica')

ggplot(cty2) + 
  geom_sf(aes(fill=survey_count)) + 
  theme_void()
ggsave('~/DHSwealth/res/Survey_count_space.png', width=8, heigh=4)

