library(tidyverse)
library(pcaMethods)
library(countrycode)
library(data.table)

data <- fread('~/mortalityblob/dhs/mics_dhs_wealth.csv')

data$nas <- rowSums(is.na(data))

data <- data %>%
  filter(nas <= 5)

data <- data %>%
  mutate(water_source_drinking = toupper(water_source_drinking),
         toilet_type = toupper(toilet_type),
         material_wall = toupper(material_wall),
         material_floor = toupper(material_floor),
         material_roof = toupper(material_roof))

options(na.action='na.pass')

data$number_sleeping_rooms <- data$number_sleeping_rooms/max(data$number_sleeping_rooms, na.rm=T)

#Get Wealth Factor from All Vars
mm <- model.matrix( ~ water_source_drinking + has_electricity + has_radio + has_television + has_refrigerator + has_bicycle + has_motorcycle + has_car + toilet_type + material_wall + material_floor + material_roof + number_sleeping_rooms, data=data)

pca1 <- pca(mm, method = 'svdImpute', center=TRUE, nPcs = 1, verbose=TRUE)

data$wfh <- scores(pca1)[ , 1]

#Get Wealth Factor from Vars robust to urban-rural differences
mm <- model.matrix( ~ water_source_drinking + has_electricity + has_radio + has_television + has_refrigerator + has_bicycle + has_motorcycle + has_car + toilet_type, data=data)

pca2 <- pca(mm, method = 'svdImpute', center=TRUE, nPcs = 1, verbose=TRUE)

data$wfh2 <- scores(pca2)[ , 1]
data$wfh2 <- -data$wfh2

#Get Wealth Factor from Vars robust to urban-rural differences
mm <- model.matrix( ~ has_electricity + has_radio + has_television + has_refrigerator + has_bicycle + has_motorcycle + has_car, data=data)

pca3 <- pca(mm, method = 'svdImpute', center=TRUE, nPcs = 1, verbose=TRUE)

data$wfh3 <- scores(pca3)[ , 1]
data$wfh3 <- -data$wfh3

write.csv(data, '~/mortalityblob/dhs/wealth_export.csv', row.names=F)

d <- data %>%
  filter(!is.na(wealth_factor)) %>%
  group_by(survey) %>%
  summarize(wfhcor = cor(wfh, wealth_factor),
            wfh2cor = cor(wfh2, wealth_factor),
            wfh3cor = cor(wfh3, wealth_factor)) %>%
  gather(pca, cor, -survey) %>%
  mutate(pca = case_when(grepl('2', pca) ~ "Possessions, sanitation",
                         grepl('3', pca) ~ "Possessions",
                         TRUE ~ "Possessions, sanitation, household materials"))

d %>% arrange(cor) %>% filter(cor < 0)

ggplot(d) + 
  geom_bar(aes(x=cor, fill=pca), stat='bin', position='dodge', binwidth=0.05) + 
  labs(x="Correlation", title = "Correlation between survey-specific wealth index and harmonized wealth index", caption='Worst Surveys: Ukraine 2005 MICS, Albania 2000 MICS, Cuba 2019 MICS, Tunisia 2018 MICS, Turkey 2008 DHS', y='Count of Surveys') + 
  theme(legend.position=c(0.25, 0.75))
ggsave('~/DHSwealth/res/correlation_histogram_all.png', width=8, height=4)


s <- data %>%
  select(urban_rural, wfh, wfh2, wfh3) %>%
  gather(pca, value, -urban_rural) %>%
  mutate(pca = case_when(grepl('2', pca) ~ "Possessions, sanitation",
                         grepl('3', pca) ~ "Possessions",
                         TRUE ~ "Possessions, sanitation, household materials"))

ggplot(s) + 
  geom_histogram(aes(x=value, fill=urban_rural)) + 
  facet_wrap(pca ~ ., ncol =1) + 
  scale_x_continuous(limits=c(-1.5, 1.5))+
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

ggsave('~/DHSwealth/res/urban_rural_histograms.png', width=8, height=4)

loads <- bind_rows(pca3 %>%
                    loadings %>%
                    data.frame %>%
                    mutate(pca = 'Possessions',
                           variable = row.names(.),
                           PC1 = -PC1),
                   pca2 %>%
                    loadings %>%
                    data.frame %>%
                    mutate(pca = 'Possessions, sanitation',
                           variable = row.names(.),
                           PC1 = -PC1),
                   pca1 %>%
                    loadings %>%
                    data.frame %>%
                    mutate(pca = 'Possessions, sanitation, household materials',
                           variable = row.names(.)))

ggplot(loads) + 
  geom_bar(aes(x=variable, y=PC1), stat='identity') + 
  coord_flip() + 
  facet_wrap(pca ~ ., ncol=3)
ggsave('~/DHSwealth/res/PCA_loadings.png', width=10, height=5)











