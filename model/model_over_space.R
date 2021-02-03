library(tidyverse)
library(zoo)
library(countrycode)
library(cowplot)
library(data.table)

d <- fread('~/mortalityblob/dhs/wealth_export_chi.csv', stringsAsFactors=F)

fill <- function(x, upto=5){
  #Interpolate any two values
  #Fill edges up to "upto" values
  if (all(is.na(x))){
    return(x)
  }

  x <- na.approx(x, na.rm=F)

  w <- which(!is.na(x))

  s <- w[1]
  e <- w[length(w)]

  sr <- s - upto
  sr <- ifelse(sr < 0, 0, sr)
  
  er <- e + upto
  er <- ifelse(er > length(x), length(x), er)

  x[sr:s] <- x[s]
  x[er:e] <- x[e]

  x
}

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

d$phc2 <- d$wfh2 < threshsg$cutoff2

cty_yr <- d %>%
  group_by(survey, urban_rural, year, iso3, country, program) %>%
  summarize(phc2 = weighted.mean(phc2, weights=hhweight)) %>%
  spread(urban_rural, phc2) %>%
  mutate(perc.rural.poor = Rural/(Rural + Urban)) %>%
  filter(!is.nan(perc.rural.poor), !is.na(perc.rural.poor))

cty_yr_m <- cty_yr %>%
  ungroup %>%
  select(iso3, year, survey, perc.rural.poor) %>%
  merge(expand.grid(list(year=1990:2020, iso3=unique(cty_yr$iso3))), all=T) %>%
  group_by(iso3) %>%
  arrange(year) %>%
  mutate(perc.rural.poor_est = fill(perc.rural.poor, upto=8))


ag_gdp <- read.csv('~/mortalityblob/dhs/API_NV.AGR.TOTL.ZS_DS2_en_csv_v2_1928314.csv', skip=4) %>%
  select(-`Country.Name`, -`Indicator.Name`, -`Indicator.Code`) %>%
  gather(year, ag_gdp, -Country.Code) %>%
  mutate(year = as.numeric(substr(year, 2, 5)),
         iso3 = countrycode(Country.Code, 'wb', 'iso3c')) %>%
  select(-Country.Code) %>%
  filter(!is.na(iso3)) %>%
  group_by(iso3) %>%
  mutate(ag_gdp = fill(ag_gdp, upto=7))

urb <- read.csv('~/mortalityblob/dhs/API_SP.URB.TOTL.IN.ZS_DS2_en_csv_v2_1929300.csv', skip=4) %>%
  select(-`Country.Name`, -`Indicator.Name`, -`Indicator.Code`) %>%
  gather(year, urb, -Country.Code) %>%
  mutate(year = as.numeric(substr(year, 2, 5)),
         iso3 = countrycode(Country.Code, 'wb', 'iso3c')) %>%
  select(-Country.Code) %>%
  filter(!is.na(iso3)) %>%
  group_by(iso3) %>%
  mutate(urb = fill(urb, upto=7))


all <- Reduce(x=list(cty_yr_m, ag_gdp, urb), 
              f=function(x, y){merge(x, y, all=T)}) %>%
  filter(!is.na(year))

p1 <- ggplot(all) + 
  geom_point(aes(y=perc.rural.poor, x=ag_gdp)) + 
  labs(x='Ag as a % of GDP', y="Percent of Poor Are Rural")

p2 <- ggplot(all) + 
  geom_point(aes(y=perc.rural.poor, x=urb)) + 
  labs(x='Percent of Population is Urban', y="Percent of Poor Are Rural")

plot_grid(p1, p2, ncol=2)
ggsave('~/DHSwealth/res/Compare_other_vars.png', width=8, height=4)

mod <- lm(perc.rural.poor ~ ag_gdp + urb, data=all)
