library(tidyverse)
library(countrycode)
library(zoo)
library(Hmisc)

data <- read.csv('~/mortalityblob/dhs/wealth_export.csv')

data$iso3 <- countrycode(data$country, 'country.name', 'iso3c')
data$year <- data$survey_single_year

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


# I think this WB data is the same as povcal data (it was for a few places I spot checked)
# but Im not sure if its all the same
wb <- read.csv('~/mortalityblob/dhs/API_SI.POV.DDAY_DS2_en_csv_v2_1928965.csv', skip=4) %>%
  select(-`Country.Name`, -`Indicator.Name`, -`Indicator.Code`) %>%
  gather(year, phc, -Country.Code) %>%
  mutate(year = as.numeric(substr(year, 2, 5)),
         iso3 = countrycode(Country.Code, 'wb', 'iso3c')) %>%
  select(-Country.Code) %>%
  filter(!is.na(iso3)) %>%
  group_by(iso3) %>%
  mutate(phc = fill(phc, upto=7))

comb <- merge(data, wb, all.x=T, all.y=F)

# we have 432 surveys
# upto == 7 > 393 surveys with data
# upto == 5 > 370 surveys with data
# upto == 0 > 292 surveys with data

comb <- comb %>%
  filter(!is.na(phc))

# Get weighted quantiles
wtd.quantile<- function (x, weights = NULL, probs = c(0, 0.25, 0.5, 0.75, 1),
                         type = c("quantile", "(i-1)/(n-1)", "i/(n+1)", "i/n"),
                         na.rm = TRUE)  {
  # Function taken from HMISC, but issue solved which is documented here: https://github.com/harrelfe/Hmisc/issues/97#issuecomment-429634634
  normwt = FALSE
  if (!length(weights))      return(quantile(x, probs = probs, na.rm = na.rm))
  type <- match.arg(type)
  if (any(probs < 0 | probs > 1))      stop("Probabilities must be between 0 and 1 inclusive")
  nams <- paste(format(round(probs * 100, if (length(probs) >
                                              1) 2 - log10(diff(range(probs))) else 2)), "%", sep = "")

  if(na.rm & any(is.na(weights))){   ###### new
    i<- is.na(weights)
    x <- x[!i]
    weights <- weights[!i]
  }
  i <- weights <= 0         # nwe: kill negative and zero weights and associated data
  if (any(i)) {
    x <- x[!i]
    weights <- weights[!i]
  }
  if (type == "quantile") {
    if(sum(weights) < 1000000 ) {weights<- weights*1000000/sum(weights)}  ##### new
    w <- wtd.table(x, weights, na.rm = na.rm, normwt = normwt,
                   type = "list")
    x <- w$x
    wts <- w$sum.of.weights
    n <- sum(wts)
    order <- 1 + (n - 1) * probs
    low <- pmax(floor(order), 1)
    high <- pmin(low + 1, n)
    order <- order%%1
    allq <- approx(cumsum(wts), x, xout = c(low, high), method = "constant",
                   f = 1, rule = 2)$y
    k <- length(probs)
    quantiles <- (1 - order) * allq[1:k] + order * allq[-(1:k)]
    names(quantiles) <- nams
    return(quantiles)
  }
  w <- wtd.Ecdf(x, weights, na.rm = na.rm, type = type, normwt = normwt)
  structure(approx(w$ecdf, w$x, xout = probs, rule = 2)$y,
            names = nams)
}

combq <- comb %>%
  group_by(survey) %>%
  mutate(wfh_q = wfh < quantile(wfh, probs=mean(phc, na.rm=T)/100),
         wfh_q2 = wfh2 < quantile(wfh2, probs=mean(phc, na.rm=T)/100),
         wfh_q3 = wfh3 < quantile(wfh3, probs=mean(phc, na.rm=T)/100))

comb2 <- combq %>%
  select(iso3, year, urban_rural, hhweight, country, survey, program, wfh, wfh2, wfh3,
         phc, wfh_q, wfh_q2, wfh_q3, wealth_quintile, wealth_factor)

write.csv(comb2, '~/mortalityblob/dhs/wealth_export_chi.csv', row.names=F)
