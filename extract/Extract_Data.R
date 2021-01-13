library(tidyverse)
library(haven)
library(foreign)

#Directory where DHS stata files and shapefiles are located
DATA_DIR <- '~/mortalityblob/dhsraw/'

#Directory where to write resulting data
WRITE_DIR <- '~/mortalityblob/dhs/'

#Directory scoping and var labels are stored
METADATA_DIR <- '~/DHSwealth/metadata/'

#Temporary directory for intermediate outputs
TEMP_DIR <- '/mnt/DHS/'

#Previously scoped surveys
scope <- read.csv(paste0(METADATA_DIR, 'Wealth_Scope.csv')) %>%
  filter(!is.na(GE) & !(is.na(PR) & is.na(IR)))  %>%
  select(num, cc, subversion, GE, PR, IR, WI)

#Remove processed surveys already in TEMP_DIR
scope <- scope %>%
  filter(!paste0(cc, '-', num, '-', subversion, '.csv') %in% list.files(TEMP_DIR))

vardat <- read.csv(paste0(METADATA_DIR, 'WealthVars.csv'))

setwd(TEMP_DIR)
for (i in 1:nrow(scope)){
  print(paste0(round(i/nrow(scope), 3)*100, '% on ', scope$cc[i], '-', scope$num[i], '-', scope$subversion[i]))
 
  #Read in data files
  if (!is.na(scope$PR[i])){
    ##### Try PR files #########
    dat <- read_dta(paste0(DATA_DIR, scope$PR[i]))

    #Drop unnecessary columns
    dat <- dat[ , vardat$PR[vardat$PR %in% names(dat)]]
    
    #Rename columns
    for (n in 1:nrow(vardat)){
      names(dat)[names(dat)==vardat$PR[n]] <- vardat$variable[n]
    }
  } else{
    ###### Else use IR ########
    dat <- read_dta(paste0(DATA_DIR, scope$IR[i]))

    #Drop unnecessary columns
    dat <- dat[ , vardat$IR[vardat$IR %in% names(dat)]]
    
    #Rename columns
    for (n in 1:nrow(vardat)){
      names(dat)[names(dat)==vardat$IR[n]] <- vardat$variable[n]
    }
  }

  # Subset to unique households
  dat <- unique(dat)
  
  if (!is.na(scope$WI[i])){
    #Get wealth data if it is in separate file
    wi <- read_dta(paste0(DATA_DIR, scope$WI[i]))
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
  
  #Now get spatial data
  spheadervars <- c('DHSCLUST', 'LATNUM', 'LONGNUM')
  
  spdat <- read.dbf(paste0(DATA_DIR, gsub('.shp', '.dbf', scope$GE[i])), as.is=TRUE)
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
  write.csv(dat, paste0(TEMP_DIR, surveycode, '.csv'), row.names=F)
}

alldata <- Reduce(bind_rows, 
                  lapply(list.files(pattern='.csv$'), 
                         function(x){read.csv(x, colClasses='character')}))

write.csv(alldata, paste0(WRITE_DIR, 'wealthvars_raw.csv'), row.names=F)
