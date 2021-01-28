library(tidyverse)
library(countrycode)
library(sf)
library(rnaturalearth)

d <- read.csv('~/mortalityblob/dhs/wealth_export_chi.csv', stringsAsFactors=F)


threshs <- d %>%
  group_by(survey) %>%
  summarize(n=n(),
            cutoff1 = max(wfh[wfh_q == T]),
            cutoff2 = max(wfh2[wfh_q2 == T]),
            cutoff3 = max(wfh3[wfh_q3 == T]))

threshsg <- threshs %>%
  summarize(cutoff1 = weighted.mean(cutoff1[!is.infinite(cutoff1)], w=n[!is.infinite(cutoff1)]),
            cutoff2 = weighted.mean(cutoff2[!is.infinite(cutoff2)], w=n[!is.infinite(cutoff2)]),
            cutoff3 = weighted.mean(cutoff3[!is.infinite(cutoff3)], w=n[!is.infinite(cutoff3)]))

d$phc3 <- d$wfh3 < threshsg$cutoff3

d2 <- d %>%
  group_by(iso3, survey, urban_rural) %>%
  summarize(phc = weighted.mean(wfh_q3, na.rm=T, w=hhweight) * n()) %>%
  spread(urban_rural, phc) %>%
  mutate(perc.poor.rural = Rural/(Urban + Rural)) %>%
  ungroup %>%
  group_by(iso3) %>%
  filter(survey == max(survey))

cty <- ne_countries(returnclass='sf') %>%
  filter(sovereignt != 'Antarctica')

cty1 <- merge(cty, d2, by.x='iso_a3', by.y='iso3', all.x=T)

ggplot(cty1) + 
  geom_sf(aes(fill=perc.poor.rural), size=0.25) + 
  theme_void() + 
  scale_fill_continuous(limits=c(0, 1)) + 
  labs(title='Percent of Poor in Rural Areas (latest survey, national defs)')
ggsave('~/DHSwealth/res/perc.poor.rural_nat.png', width=8, height=4)


d3 <- d %>%
  group_by(iso3, survey, urban_rural) %>%
  summarize(phc3 = weighted.mean(phc3, na.rm=T, w=hhweight) * n()) %>%
  spread(urban_rural, phc3) %>%
  mutate(perc.poor.rural = Rural/(Urban + Rural)) %>% 
  ungroup %>%
  group_by(iso3) %>%
  filter(survey == max(survey))

cty3 <- merge(cty, d3, by.x='iso_a3', by.y='iso3', all.x=T)

ggplot(cty3) + 
  geom_sf(aes(fill=perc.poor.rural), size=0.25) + 
  theme_void() + 
  scale_fill_continuous(limits=c(0, 1)) + 
  labs(title='Percent of Poor in Rural Areas (latest survey, harmonized defs)')
ggsave('~/DHSwealth/res/perc.poor.rural_int.png', width=8, height=4)


### Example country histogram
ex <- d %>% 
  filter(survey == 'India 2015 DHS')

xdf <- data.frame(vals=c(threshs$cutoff3[threshs$survey == 'India 2015 DHS'], threshsg$cutoff3),
                  def=c('National', 'Harmonized'))

ggplot() + 
  geom_histogram(data = ex, aes(x=wfh3, y=..density.., weight=hhweight, fill=urban_rural)) +
  geom_vline(data = xdf, aes(xintercept = vals, linetype=def)) + 
  labs(x='Wealth Scale (Possessions)', fill='', linetype='Poverty Mark',
       title='Distribution of Wealth in India 2015 DHS, with Poverty Threshholds')
ggsave('~/DHSwealth/res/example_histogram.png', width=8, height=4)

#######################################
### Example multicountry histogram
ex <- d %>% 
  filter(survey %in% c('Jamaica 2011 MICS', 'Chad 2015 DHS', 'Belarus 2012 MICS'))


xdf1 <- data.frame(vals=c(threshs$cutoff3[threshs$survey == 'Jamaica 2011 MICS'],
                         threshsg$cutoff3),
                  def=c('National', 'Harmonized'),
                  survey = rep('Jamaica 2011 MICS', 2))
xdf2 <- data.frame(vals=c(threshs$cutoff3[threshs$survey == 'Chad 2015 DHS'],
                         threshsg$cutoff3),
                  def=c('National', 'Harmonized'),
                  survey = rep('Chad 2015 DHS', 2))
xdf3 <- data.frame(vals=c(min(d$wfh3[d$survey == 'Belarus 2012 MICS']),
                         threshsg$cutoff3),
                  def=c('National', 'Harmonized'),
                  survey = rep('Belarus 2012 MICS', 2))

xdf <- bind_rows(xdf1, xdf2, xdf3)

ggplot(ex) + 
  geom_histogram(aes(x=wfh, y=..density.., weight=hhweight, fill=urban_rural)) + 
  facet_wrap(survey ~ ., ncol=1) + 
  geom_vline(data = xdf, aes(xintercept = vals, linetype=def)) + 
  labs(x='Wealth Scale (Possessions)', fill='', linetype='Poverty Mark',
       title='Distribution of Wealth in Belarus, Chad and Jamaica, with Poverty Threshholds')

ggsave('~/DHSwealth/res/example_histogram_multiple.png', width=8, height=4)



