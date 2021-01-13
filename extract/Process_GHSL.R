library(tidyverse)

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
  select(code, survey_year, matches('perc')) %>%
  filter(!is.nan(perc2014))

#Helpers
logit <- function(x){
  log(x/(1-x))
}

inv.logit <- function(x){
  1/(1 + exp(-x))
}


modelFuture <- function(var){
  #Model Future using methodology from Osgood-Zimmerman
  #Get year on year Average Rate of Change (AROC)
  #Get weighted average rate of change,
  #Then apply it to future

  #Var must be in order sequentially,
  # with only non-NAs preceeding NAs
  # must be a fraction (rate, or incidence)

  #Logit fails if any value is 1 or 0.
  #For my purposes, values very close to 1 and 0 will work
  #So set 1 = 0.99999
  
  if (any(var > 1, na.rm=T) | any(var < 0, na.rm=T)){
    stop("Value should be a proportion, cant be > 1 or < 0")
  }

  var[var == 1] <- 0.99999
  var[var == 0] <- 0.00001

  dat <- var[!is.na(var)]
  nas <- sum(is.na(var))

  roc <- mapply(function(y1, y2) logit(y2) - logit(y1),
         dat[1:(length(dat) -1)],
         dat[2:length(dat)])

  w <- (2:length(dat))/sum(2:length(dat)) 
  
  aroc <- sum(w*roc)

  pred <- inv.logit(logit(dat[length(dat)]) + aroc*(1:nas))
  
  pred[is.nan(pred)] <- 0
  res <- c(dat, pred)

  return(res)
}

#Function to Interpolate between years
impute <- function(syear, y1975, y1990, y2000, y2014){
  if (syear <= 1990){
    res <- approx(c(y1975, y1990), n=length(1975:1990))$y
    return(res[syear - 1974])
  }
  if (syear <= 2000){
    res <- approx(c(y1990, y2000), n=length(1990:2000))$y
    return(res[syear - 1989])
  }
  if (syear <= 2014){
    res <- approx(c(y2000, y2014), n=length(2000:2014))$y
    return(res[syear - 1999])
  }
  if (syear > 2014){
    res <- modelFuture(c(approx(c(y2000, y2014), n=length(2000:2014))$y, rep(NA, syear - 2014)))
    return(res[length(res)])
  }
}

data$built_up <- mapply(impute, syear=data$survey_year, y1975=data$perc1975, 
                        y1990=data$perc1990, y2000=data$perc2000, y2014=data$perc2014)

write.csv(data, '~/mortalityblob/dhs/wealthvars_built_clean.csv', row.names=T)

