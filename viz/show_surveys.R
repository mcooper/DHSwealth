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

ggplot(sel) + 
  geom_bar(aes(x=year, y=..count.., fill=group)) +
  labs(title = 'Number of Available Surveys', fill ='')
ggsave('~/DHSwealth/res/Survey_count_time.png', width=8, heigh=4)

cty <- ne_countries(returnclass='sf')
cty$iso_a3[cty$soverignt == 'Somaliland'] <- 'SOM'
cty$iso_a3[cty$soverignt == 'Western Sahara'] <- 'SOM'

sel2 <- sel %>%
  mutate(iso_a3 = countrycode(country, 'country.name', 'iso3c')) %>%
  group_by(iso_a3) %>%
  summarize(survey_count=n())

cty2 <- merge(cty, sel2, all.x=T) %>%
  filter(sovereignt != 'Antarctica')

cty2$survey_count[is.na(cty2$survey_count)] <- 0
cty2$survey_ct <- cut(cty2$survey_count, c(-1, 0, 1, 3, 5, 10))

ggplot(cty2) + 
  geom_sf(aes(fill=survey_ct), size=0.25) + 
  theme_void() + 
  scale_fill_manual(labels=c('(-1,0]'='None', '(0,1]'='1', '(1,3]'='2-3', 
                               '(3,5]'='4-5', '(5,10]'='6+'),
                    values=c('(-1,0]'='#f2f0f7', '(0,1]'='#cbc9e2', '(1,3]'='#9e9ac8', 
                               '(3,5]'='#756bb1', '(5,10]'='#54278f')) +
  labs(fill='Number of Surveys')

ggsave('~/DHSwealth/res/Survey_count_space.png', width=8, heigh=4)

