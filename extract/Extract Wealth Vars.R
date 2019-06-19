setwd('/home/mattcoop/')

library(haven)
library(tidyverse)
library(foreign)

options(stringsAsFactors=F)

##############################
#Scope Available Datasets 
#################################

#For now, only use surveys that have GPS data and PR data (surveys without PR data are missing wealth data anyway)
scope <- read.csv('Wealth_Scope.csv') %>%
  filter(!is.na(GE) & !is.na(PR)) %>%
  #Note: Only the DHS7 in India has GPS and no PR - so do a tacky workaround
  bind_rows(data.frame(PR='IAHR74FL.DTA', WI=NA, GE='IAGE71FL.shp', cc='IA', num=7, subversion=1))

vardat <- read.csv('WealthVars.csv')

setwd('child-months/WealthTemp')
for (i in 1:nrow(scope)){
  print(paste0(round(i/nrow(scope), 3)*100, '% on ', scope$cc[i], '-', scope$num[i], '-', scope$subversion[i]))
  
  #Get values from PR files
  dat <- read_dta(paste0('../../climatedisk/DHS/', scope$PR[i]))
  
  #If wealth data is missing and there is no wi file, next
  if (!('hv271' %in% names(dat)) & is.na(scope$WI[i])){
    next
  }
  
  #Drop unnecessary columns
  dat <- dat[ , vardat$PR[vardat$PR %in% names(dat)]]
  
  #Rename columns
  for (n in 1:nrow(vardat)){
    names(dat)[names(dat)==vardat$PR[n]] <- vardat$variable[n]
  }
  
  if (!is.na(scope$WI[i])){
    #Get wealth data if it is in separate file
    wi <- read_dta(paste0('../../climatedisk/DHS/', scope$WI[i]))
    names(wi) <- c('hhid', 'wealth_factor', 'wealth_index')
    
    wi$wealth_factor <- as.character(wi$wealth_factor)
    wi <- wi[ , c('hhid', 'wealth_factor')]
    
    if (sum(!dat$hhid %in% wi$hhid) > 0){
      #If hhid doesnt match, try it with just hhno and clusterid
      padws <- function(vect, n=0){
        len <- max(nchar(vect))
        padded <- paste0("     ", vect)
        substr(padded, nchar(padded) - (len - 1), nchar(padded))
      }
      
      dat$hhid <- paste0(padws(dat$clusterid), padws(dat$householdno))
      idlen <- nchar(dat$hhid)[1]
      dat$hhid <- paste0(paste0(rep(" ", 12-idlen), collapse=''), dat$hhid)
      
    }
    
    if (sum(!dat$hhid %in% wi$hhid) > 0){
      #Check again for matching, if not, cat error
      cat('Mismatches in wealth data for ', scope$cc[i], scope$num[i], scope$subversion[i], '\n') 
    }
    
    dat <- merge(dat, wi, all.x=T, all.y=F)
  }
  
  #If wealth factor still missing or all NA, next
  if (!('wealth_factor' %in% names(dat))){
    next
  }
  if (sum(is.na(dat$wealth_factor)) == nrow(dat)){
    next
  }
  
  #Save character version of labelled vars in new column, and convert original to integer
  for (n in names(dat)){
    if (is.labelled(dat[ , n])){
      dat[ , paste0(n, '_chr')] <- as.character(as_factor(dat[ , n]))
      dat[ , n] <- as.character(as.integer(dat[ , n]))
    }
  }
  
  surveycode <- paste0(scope$cc[i], '-', scope$num[i], '-', scope$subversion[i])
  dat$surveycode <- surveycode
  dat$code <- paste0(dat$surveycode, '-', dat$clusterid)
  dat$hh_code <- paste0(dat$code, '-', dat$householdno)
  dat$resp_code <- paste0(dat$hh_code, '-', dat$respondent_no)
  
  #Now get spatial data
  spheadervars <- c('DHSCLUST', 'LATNUM', 'LONGNUM')
  
  spdat <- read.dbf(paste0('../../climatedisk/DHS/', gsub('.shp', '.dbf', scope$GE[i])), as.is=TRUE)
  if (!all(spheadervars %in% names(spdat))){
    cat(scope$GE[i], 'is missing necessary column names\n')
  }else{
    spdat <- spdat[ , spheadervars]
    spdat$num <- substr(scope$GE[i], 5, 5)
    spdat$cc <- toupper(substr(scope$GE[i], 1, 2))
    spdat$subversion <- ifelse(toupper(substr(scope$GE[i], 6, 6)) %in% as.character(seq(0, 9)), 1,
                               ifelse(toupper(substr(scope$GE[i], 6, 6)) %in% LETTERS[1:8], 2,
                                      ifelse(toupper(substr(scope$GE[i], 6, 6)) %in% LETTERS[9:17], 3,
                                             ifelse(toupper(substr(scope$GE[i], 6, 6)) %in% LETTERS[18:26], 4, 99))))
    spdat$code <- paste(spdat$cc, spdat$num, spdat$subversion, spdat$DHSCLUST, sep='-')
    spdat <- spdat %>%
      select(latitude=LATNUM, longitude=LONGNUM, code=code)
  }
  
  initialsize <- nrow(dat)
  dat <- merge(dat, spdat, all.x=T, all.y=F)
  
  if (initialsize != nrow(dat)){
    cat("Mismatches in Spatial and Womens data.  Initial size:", initialsize, " now:", nrow(ir_sel), '\n')
  }
  
  #Write and then read with a bind_rows to save on memory
  write.csv(dat, paste0(surveycode, '.csv'), row.names=F)
  
}

alldata <- Reduce(bind_rows, 
                  lapply(list.files(pattern='.csv$'), 
                         function(x){read.csv(x, colClasses='character')}))

write.csv(alldata, '/home/matt/wealthvars_raw.csv', row.names=F)
#alldata <- read.csv('/home/matt/wealthvars_raw.csv')

###############################
#Calculate Cutpoints
###############################

#High end anchoring cutpoints
alldata$telephone <- alldata$telephone == 1
alldata$television <- alldata$television == 1
alldata$car_truck <- alldata$car_truck == 1
alldata$refrigerator <- alldata$refrigerator == 1

#Low end anchoring cutpoints
#housing
#table(alldata[ , c('floor_materials_chr', 'floor_materials')]) %>% as.data.frame.matrix %>% View
alldata$inadequate_floor = alldata$floor_materials < 15
alldata$inadequate_walls = alldata$wall_materials < 30 & alldata$wall_materials != 3

alldata$inadequate_housing <- alldata$inadequate_floor | alldata$inadequate_walls

#Sanitation
alldata$inadequate_toilet = alldata$toilet_type >= 30

alldata$inadequate_water_urban = alldata$drinking_water != 1 & alldata$drinking_water != 71 & alldata$drinking_water != 61
alldata$inadequate_water_rural = alldata$drinking_water >= 30 & alldata$drinking_water < 60

alldata$urban <- alldata$urban_rural == 1

alldata$inadequate_sanitation <- alldata$inadequate_toilet | (alldata$inadequate_water_urban & alldata$urban) | (alldata$inadequate_water_urban & !alldata$urban)

#Crowding
alldata$crowding <- (as.numeric(alldata$hhsize)/as.numeric(alldata$sleeping_rooms)) > 3

#School
alldata$unfinished_primary = alldata$primary_school < 2

getHHheadPrimary <- function(unfinished_primary, individual_number, household_head){
  unfinished <- unfinished_primary[individual_number==unique(household_head)]
  if (length(unfinished) > 1){
    return(NA)
  } else{
    return(unfinished)
  }
}

hh <- alldata %>% 
  select(hhid, householdno, clusterid, surveycode, code, television, refrigerator, car_truck, wealth_factor,
         telephone, inadequate_housing, inadequate_sanitation, crowding, wealth_quintile, hhsize,
         survey_month, hh_code, resp_code, latitude, longitude, urban) %>%
  unique

head <- alldata %>% unique %>%
  group_by(hhid, householdno, code) %>%
  summarize(head_noprimary=getHHheadPrimary(unfinished_primary, individual_number, household_head)) %>%
  data.frame

hh <- merge(hh, head)

#Check that each household was unique for all the cutpoints
nrow(hh) == nrow(unique(alldata[ , c('hhid', 'householdno', 'clusterid', 'surveycode', 'code')]))

#Scale wealth vars from 0-1
hh <- hh %>%
  group_by(surveycode) %>%
  mutate(wealth_factor = as.numeric(wealth_factor),
         wealth_factor_resc=wealth_factor - min(wealth_factor, na.rm=T),
         wealth_factor_resc=wealth_factor_resc/max(wealth_factor_resc, na.rm=T)) %>%
  data.frame

write.csv(hh, '/home/matt/wealthvars_hh.csv', row.names=F)
#hh <- read.csv('/home/mattcoop/wealthvars_hh.csv', stringsAsFactors=F)

#Calculate Cutpoints
sdf <- hh %>% 
  select(surveycode) %>% 
  mutate(surveycode=as.character(surveycode)) %>%
  unique

baseline <- hh %>% filter(surveycode=='NG-5-1')

others <- sdf %>% filter(surveycode!='NG-5-1')

bad <- NULL

getAnchor <- function(var, data){
  #If they are all true, the anchor is the maximum wealth_factor
  if (all(data[ , var], na.rm=T)){
    anchor <- max(data$wealth_factor, na.rm=T)
    cat("all true on", var, "\n")
    return(anchor)
  }
  #If they are all false, the anchor is the minimum of wealth_factor
  if (all(!data[ , var], na.rm=T)){
    anchor <- min(data$wealth_factor, na.rm=T)
    cat("all false on", var, "\n")
    return(anchor)
  }
  #Normal case: Use logisitc regression to estimate anchor
  data$outcome <- data[ , var]
  
  #Just a quick catch because doing a logit without two values freezes R
  if (length(table(data$outcome)) != 2){
    stop("Cant do a logit")
  } else{
    mod <- glm(outcome ~ wealth_factor_resc, data = data, family = "binomial")
  }
  
  anchor <- -mod$coefficients[1]/mod$coefficients[2]
  
  return(anchor)
}

for (i in others$surveycode){
  sel <- hh %>% filter(surveycode==i)
  
  #Drop columns that are greater than half NA
  for (col in c("inadequate_housing", "inadequate_sanitation", "crowding", "head_noprimary",
                "television", "refrigerator", "car_truck", "wealth_factor", "telephone")){
    if (sum(is.na(sel[ , col])) > nrow(sel)/2){
      sel[ , col] <- NULL
    }
  }
  
  #Skip if there arent at least 2 points
  if (sum(c("inadequate_housing", "inadequate_sanitation", "crowding", "head_noprimary",
            "television", "refrigerator", "car_truck", "telephone") %in%
          names(sel)) < 2){
    bad <- c(bad, i)
    next
  }
  
  bsl <- baseline
  
  needs <- c("inadequate_housing", "inadequate_sanitation", "crowding", "head_noprimary")
  
  needs <- needs[needs %in% names(sel)]
  
  if (length(needs) > 1){
    bsl$deprivation <- rowSums(bsl[ , needs])
    sel$deprivation <- rowSums(sel[ , needs])
  } else{
    bsl$deprivation <- bsl[ , needs]
    sel$deprivation <- sel[ , needs]
  }
  
  bsl_anchors <- NULL
  sel_anchors <- NULL
  
  #First do deprivation, depending on the number of indicators of deprivation available
  if (length(needs) > 1){
    for (j in 1:length(needs)){
      bsl$depcut <- bsl$deprivation >= j
      sel$depcut <- sel$deprivation >= j
      
      bsl_anchors <- c(bsl_anchors, getAnchor('depcut', bsl))
      sel_anchors <- c(sel_anchors, getAnchor('depcut', sel))
    }
  } else{
    bsl_anchors <- c(bsl_anchors, getAnchor('deprivation', bsl))
    sel_anchors <- c(sel_anchors, getAnchor('deprivation', sel))
  }
  
  #Then do Wealth Anchors
  #Television
  if ("television" %in% names(sel)){
    bsl_anchors <- c(bsl_anchors, getAnchor('television', bsl))
    sel_anchors <- c(sel_anchors, getAnchor('television', sel))
  }
  
  #Telephone
  if ("telephone" %in% names(sel)){
    bsl_anchors <- c(bsl_anchors, getAnchor('telephone', bsl))
    sel_anchors <- c(sel_anchors, getAnchor('telephone', sel))
  }
  
  #Car or Truck
  if ("car_truck" %in% names(sel)){
    bsl_anchors <- c(bsl_anchors, getAnchor('car_truck', bsl))
    sel_anchors <- c(sel_anchors, getAnchor('car_truck', sel))
  }
  
  #Refrigerator
  if ("refrigerator" %in% names(sel)){
    bsl_anchors <- c(bsl_anchors, getAnchor('refrigerator', bsl))
    sel_anchors <- c(sel_anchors, getAnchor('refrigerator', sel))
  }
  
  #only regress anchors that are within a normal range
  ix <- which(sel_anchors < 2 & sel_anchors > -1)
  
  anchormod <- lm(bsl_anchors[ix] ~ sel_anchors[ix])
  
  others[others$surveycode==i, 'intercept'] <- anchormod$coefficients[1]
  others[others$surveycode==i, 'slope'] <- anchormod$coefficients[2]
  
  cat(i, which(others$surveycode==i)/nrow(others), '\n')
}

others <- bind_rows(others, data.frame(surveycode="NG-5-1", intercept=0, slope=1))

##########################################
#Model Cutputs in relation to Baseline
##########################################

#Baseline is Nigeria 5 1 from 2008.  Large sample size, complete records, wide variety of income levels

hh_adj <- hh

for (i in others$surveycode){
  if (i %in% bad){
    hh_adj <- hh_adj %>%
      filter(surveycode != i)
    next
  }
  
  hh_ix <- hh_adj$surveycode == i
  
  hh_adj$wealth_factor_harmonized[hh_ix] <- hh_adj$wealth_factor_resc[hh_ix]*others$slope[others$surveycode==i] + others$intercept[others$surveycode==i]
}

hh_adj %>% group_by(surveycode) %>% summarize(mean(wealth_factor_harmonized, na.rm=T), sd(wealth_factor_harmonized, na.rm=T), n()) %>% View

just_hh <- hh_adj %>% 
  select(hhid, householdno, code, clusterid, surveycode, resp_code,
         hhsize, survey_cmc=survey_month, hh_code, latitude, longitude, urban, 
         wealth_factor, wealth_quintile, wealth_factor_resc, wealth_factor_harmonized) %>%
  mutate(survey_cmc=as.numeric(survey_cmc),
         survey_year=1900 + floor(survey_cmc/12),
         survey_month= 1 + (survey_cmc - (survey_year - 1900)*12))

write.csv(just_hh, '../../../matt/hh_wealth_harmonized.csv', row.names=F)

#Export for Ian
ian <- hh_adj %>%
  group_by(code, surveycode, latitude, longitude, urban) %>%
  summarize(survey_cmc=mean(survey_month, na.rm=T),
            mean_wealth_factor_harmonized=mean(wealth_factor_harmonized, na.rm=T)) %>%
  mutate(survey_cmc=round(survey_cmc),
         survey_year=1900 + floor(survey_cmc/12),
         survey_month= 1 + (survey_cmc - (survey_year - 1900)*12))

write.csv(ian, '../../../matt/site_wealth_harmonized.csv', row.names=F)
