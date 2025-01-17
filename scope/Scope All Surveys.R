##############################
#Set Directories
#############################

#Directory where DHS stata files and shapefiles are located
DATA_DIR <- '~/mortalityblob/dhsraw/'

#Directory where Scoping result and other metadata will be saved
OUTPUT_DIR <- '~/DHSwealth/metadata/'

##############################
#Run Script
#############################

setwd(DATA_DIR)

library(tidyverse)

makeFileNameDf <- function(f){
  num <- substr(f, 5, 5)
  cc <- toupper(substr(f, 1, 2))
  subversion <- ifelse(toupper(substr(f, 6, 6)) %in% as.character(seq(0, 9)), 1,
                       ifelse(toupper(substr(f, 6, 6)) %in% LETTERS[1:8], 2, 
                              ifelse(toupper(substr(f, 6, 6)) %in% LETTERS[9:17], 3, 
                                     ifelse(toupper(substr(f, 6, 6)) %in% LETTERS[18:26], 4))))
  data.frame(num, cc, subversion, file=f)
}

#Get all stata files or shapfiles
surveytypes <- list.files(pattern='^.{2}(GE|WI|HR|IR|PR).{5}(DTA|dta|SHP|shp)$') %>%
  substr(3, 4) %>%
  toupper %>% 
  unique

all <- data.frame(matrix(ncol = 3, nrow = 0))
names(all) <- c('num', 'cc', 'subversion')
for (type in surveytypes){
  sel <- list.files(pattern=paste0('^..(', type, '|', tolower(type), ').....(DTA|dta|SHP|shp)$')) %>%
    lapply(FUN = makeFileNameDf) %>%
    Reduce(f = bind_rows)
  
  names(sel)[4] <- type
  
  all <- merge(all, sel, all.x=T, all.y=T)
}

#in some cases, there is duplicate versioning!!
#So aggregate in such a way that you use the newer version
all <- all %>% 
  arrange(cc, num, subversion, HR, IR, GE, PR, WI) %>%
  group_by(num, cc, subversion) %>%
  summarize(HR=tail(HR, n=1),
            IR=tail(IR, n=1),
            GE=tail(GE, n=1),
            PR=tail(PR, n=1),
            WI=tail(WI, n=1))

write.csv(all, paste0(OUTPUT_DIR, 'Wealth_Scope.csv'))
