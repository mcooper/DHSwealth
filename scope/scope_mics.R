library(tidyverse)
library(haven)

setwd('~/gd/data/MICS')

options(stringsAsFactors=F)

fs <- list.files(recursive=T, pattern='sav|SAV$')

df <- data.frame(full=fs, stringsAsFactors=F)

df$basename <- basename(df$full)
df$dir <- dirname(df$full)

# df %>% 
#   group_by(dir) %>%
#   summarize(sm = sum(grepl('hh.sav|hh...sav', tolower(basename)))) %>%
#   filter(sm != 1)

#It looks like every survey has a file called HH or hh or Hh for Households, except
#Guinea Bissau MISC4, which is all in the file 'GNB MICS File.csv'

#Get all household files for surveys that were for an entire nation (not subnational)
hhfiles <- which(grepl('hh.sav|hh...sav', tolower(df$basename)) * !(grepl('\\(', df$dir) & !grepl('Sudan', df$dir)) & !grepl('Punjab', df$dir))

df <- df[hhfiles, ]

label_process <- function(x){
  lab <- attributes(x)$label
  
  if(is.null(attributes(x)$label)){
    lab <- ""
  }
  
  final <- paste0(as.character(lab), collapse="")
  
  return(final)
  
}

fulldict <- data.frame(codes=NA, labels=NA)
surveyheaders <- data.frame()

ur <- data.frame()
for (i in 1:nrow(df)){
  table <- tryCatch(read_sav(df$full[i], encoding='utf-8'),
                    error=function(x){read_sav(df$full[i], encoding='latin1')})
  
  print(i/nrow(df))
  
  labels <- sapply(table, label_process) %>% unlist
  codes <- names(table)
 
  fulldicttmp <- data.frame(codes=tolower(codes), labels=tolower(labels), 
                            nas=sapply(table, function(x){length(unique(x[!is.na(x)]))}))

  if (basename(df$dir[i]) %in% names(fulldict)){
    stop("already have ", surveylab)
  }

  names(fulldicttmp)[3] <- basename(df$dir[i])
  
  fulldict <- merge(fulldict, fulldicttmp, all.x=T, all.y=T)
  surveyheaders <- bind_rows(surveyheaders,
                             data.frame(survey=basename(df$dir[i]),
                                        codes=paste0(c('', tolower(codes), ''), collapse='##')))
}

write.csv(fulldict, '~/DHSwealth/scope/Micsmap_full.csv', row.names=F)

##########################################
# Make Survey-Specific Dictionaries
##########################################

surveys <- names(fulldict)[!names(fulldict) %in% c('codes', 'labels', 'codelabel')]

sd <- data.frame(surveys = c(surveys))
for (i in 1:nrow(sd)){
  survey <- sd$surveys[i]
  sel <- fulldict[!is.na(fulldict[ , survey]) , c('codes', 'labels', survey)]

  ################
  #hhid
  ################
  s <- sel$codes[sel$codes %in% c('hh2', 'hid', 'hi2')]
  if(length(s) > 1){
    sub <- sel[sel$codes %in% s, c('codes', survey)]
    s <- sub$codes[which.max(sub[ , survey])]
  }
  if (length(s) > 0){
    sd[i, 'hhid'] <- s
  }

  #################
  # urban_rural
  ################
  s <- sel$codes[sel$codes %in% c('hh6', 'hi6')]
  if (length(s) > 0){
    sd[i, 'urban_rural'] <- s
  }
 
  ################
  # survey_date
  ################
  #actual full date - one survey
  s <- sel$codes[sel$codes %in% c('dataentr')]
  if (length(s) > 0){
    sd[i, 'survey_date'] <- s
  }
  #day
  s <- sel$codes[sel$codes %in% c('hh5d', 'hi3d', 'hh5_d')]
  if(length(s) > 1){
    sub <- sel[sel$codes %in% s, c('codes', survey)]
    s <- sub$codes[which.max(sub[ , survey])]
  }
  if (length(s) > 0){
    sd[i, 'survey_day'] <- s
  }
  #month
  s <- sel$codes[sel$codes %in% c('hh5m', 'hi3m', 'hh5_m')]
  if (length(s) > 0){
    sd[i, 'survey_month'] <- s
  }
  #year
  s <- sel$codes[sel$codes %in% c('hh5y', 'hi3y', 'hh5_y')]
  if (length(s) > 0){
    sd[i, 'survey_year'] <- s
  }

  ######################
  # hhweight
  #####################
  s <- sel$codes[sel$codes %in% c('hhweight')]
  if (length(s) > 0){
    sd[i, 'hhweight'] <- s
  }

  ###################
  #wealth_quintile
  ##################
  s <- sel$codes[sel$codes %in% c('windex5', 'wlthind5')]
  if (length(s) > 0){
    sd[i, 'wealth_quintile'] <- s
  }

  ####################
  #wealth_factor
  ####################
  s <- sel$codes[sel$codes %in% c('wscore', 'wlthscor')]
  if (length(s) > 0){
    sd[i, 'wealth_factor'] <- s
  }

  ####################
  #water_source_drinking
  ####################
  s <- sel$codes[sel$codes == 'ws1']
  if (length(s) > 0){
    sd[i, 'water_source_drinking'] <- s
  }

  ####################
  #water_source_drinking
  ####################
  s <- sel$codes[sel$codes == 'ws2']
  if (length(s) > 0){
    sd[i, 'water_source_nondrinking'] <- s
  }
  
  ########################
  #toilet_type
  ########################
  s <- sel$codes[grepl('type de toilet|type of toilet|kind of toilet|tipo de servicio sanitario|tipo de instalac..n sanitaria|tipo de toilet|type de topilette|tipo de casa de banho', sel$labels)]
  if(length(s) > 1){
    sub <- sel[sel$codes %in% s, c('codes', survey)]
    s <- sub$codes[which.max(sub[ , survey])]
  }
  if (length(s) > 0){
    sd[i, 'toilet_type'] <- s
  }

  ########################
  #material_wall
  ########################
  s <- sel$codes[grepl('wall|parede|murs', sel$labels)]
  if(length(s) > 1){
    sub <- sel[sel$codes %in% s, c('codes', survey)]
    s <- sub$codes[which.max(sub[ , survey])]
  }
  if (length(s) > 0){
    sd[i, 'material_wall'] <- s
  }
  
  ########################
  #material_floor
  ########################
  s <- sel$codes[grepl('material of floor|de sol|floor material|piso|du sol|material of dwelling floor|plancher', sel$labels)]
  if(length(s) > 1){
    sub <- sel[sel$codes %in% s, c('codes', survey)]
    s <- sub$codes[which.max(sub[ , survey])]
  }
  if (length(s) > 0){
    sd[i, 'material_floor'] <- s
  }
  
  ########################
  #material_roof
  ########################
  s <- sel$codes[grepl('roof|telhado|cobertura|toit|toiture|techo|tejado', sel$labels)]
  if(length(s) > 1){
    sub <- sel[sel$codes %in% s, c('codes', survey)]
    s <- sub$codes[which.max(sub[ , survey])]
  }
  if (length(s) > 0){
    sd[i, 'material_roof'] <- s
  }
  
  ########################
  #has_electricty
  ########################
  s <- sel$codes[grepl('lectrici', sel$labels)]
  if(length(s) > 1){
    sub <- sel[sel$codes %in% s, c('codes', survey)]
    s <- sub$codes[which.max(sub[ , survey])]
  }
  if (length(s) > 0){
    sd[i, 'has_electricity'] <- s
  }

  ########################
  #has_radio
  ########################
  s <- sel$codes[grepl('radio', sel$labels)]
  if(length(s) > 1){
    sub <- sel[sel$codes %in% s, c('codes', survey)]
    s <- sub$codes[which.max(sub[ , survey])]
  }
  if (length(s) > 0){
    sd[i, 'has_radio'] <- s
  }

  ########################
  #has_television
  ########################
  s <- sel$codes[grepl('television|télévision|televisão|televisión', sel$labels)]
  if(length(s) > 1){
    sub <- sel[sel$codes %in% s, c('codes', survey)]
    s <- sub$codes[which.max(sub[ , survey])]
  }
  if (length(s) > 0){
    sd[i, 'has_television'] <- s
  }

  ########################
  #has_refrigerator
  ########################
  s <- sel$codes[grepl('refrigerator|refrigerador|réfrigérateur|frigorífic', sel$labels)]
  if(length(s) > 1){
    sub <- sel[sel$codes %in% s, c('codes', survey)]
    s <- sub$codes[which.max(sub[ , survey])]
  }
  if (length(s) > 0){
    sd[i, 'has_refrigerator'] <- s
  }

  ########################
  #has_bicycle
  ########################
  s <- sel$codes[grepl('bicycle|bicicleta|vélo', sel$labels)]
  if(length(s) > 1){
    sub <- sel[sel$codes %in% s, c('codes', survey)]
    s <- sub$codes[which.max(sub[ , survey])]
  }
  if (length(s) > 0){
    sd[i, 'has_bicycle'] <- s
  }

  ########################
  #has_motorcycle
  ########################
  s <- sel$codes[grepl('moto', sel$labels)]
  if(length(s) > 1){
    sub <- sel[sel$codes %in% s, c('codes', survey)]
    s <- sub$codes[which.max(sub[ , survey])]
  }
  if (length(s) > 0){
    sd[i, 'has_motorcycle'] <- s
  }

  ########################
  #has_car
  ########################
  s <- sel$codes[grepl(' car$| car |^car |^car$|truck|voiture|carro|coche', sel$labels)]
  if(length(s) > 1){
    sub <- sel[sel$codes %in% s, c('codes', survey)]
    s <- sub$codes[which.max(sub[ , survey])]
  }
  if (length(s) > 0){
    sd[i, 'has_car'] <- s
  }

  ########################
  #has_telephone
  ########################
  s <- sel$codes[grepl('telephon|telefon|téléphon|teléphon', sel$labels)]
  if(length(s) > 1){
    sub <- sel[sel$codes %in% s, c('codes', survey)]
    s <- sub$codes[which.max(sub[ , survey])]
  }
  if (length(s) > 0){
    sd[i, 'has_telephone'] <- s
  }

  ########################
  #number_sleeping_rooms
  ########################
  s <- sel$codes[grepl('cuart.s|rooms|pièces|habitaciones', sel$labels)]
  if(length(s) > 1){
    sub <- sel[sel$codes %in% s, c('codes', survey)]
    s <- sub$codes[which.max(sub[ , survey])]
  }
  if (length(s) > 0){
    sd[i, 'number_sleeping_rooms'] <- s
  }
}

allmics <- data.frame()
for (i in 1:nrow(df)){
  print(i/nrow(df))
  table <- tryCatch(read_sav(df$full[i], encoding='utf-8'),
                    error=function(x){read_sav(df$full[i], encoding='latin1')})

  sel <- sd[i, -1]
  sel <- sel[, which(!is.na(sel)), drop=T]

  table <- table[ , match(sel, tolower(names(table)))]

  names(table) <- names(sel)

  for (n in names(table)){
    if (is.labelled(table[ , n, drop=T])){
      table[ , paste0(n, '_chr')] <- as.character(as_factor(table[ , n, drop=T]))
      table[ , n] <- as.character(as.integer(table[ , n, drop=T]))
    }
    else{
      table[ , n] <- as.character(table[ , n, drop=T])
    }
  }

  table$country <- trimws(gsub('-|_|[[:digit:]]+', '', substr(basename(df$dir[i]), 1, sapply(basename(df$dir[i]), function(x){gregexpr('MICS', x)[[1]][1] - 2}))))
  table$survey <- basename(df$dir[i])

  allmics <- bind_rows(allmics, table)
}


a <- allmics


#####################
# hhid
#####################
a$hhid_chr <- NULL

####################
# urban_rural
##################
a$urban_rural <- case_when(grepl("NON-MUNI|NON URBAN|RURAL|INTERIOR|CAMP|TRIBAL|SOUM",
                                 toupper(a$urban_rural_chr)) ~ "Rural",
                           grepl("AIMAG|VILLE|URB|OUAGA|MUNICI|CAPITAL|ANTAN|KIGALI|CHEF|KMA", 
                                 toupper(a$urban_rural_chr)) ~ "Urban",
                           grepl("OTHER", a$urban_rural_chr) ~ NA_character_)
a$urban_rural_chr <- NULL
                             
#################
# survey_year
################
a$survey_year[is.na(a$survey_year)] <- substr(a$survey_date[is.na(a$survey_year)], 1, 4)
a$survey_year <- as.numeric(a$survey_year)
a$survey_year[grepl('Thailand', a$survey) & a$survey_year > 2300] <- a$survey_year[grepl('Thailand', a$survey) & a$survey_year > 2300] - 543
a$survey_year[grepl('Nepal', a$survey)] <- a$survey_year[grepl('Nepal', a$survey)] - 56

a$survey_year[a$survey_year %in% c(0, 9999)] <- NA

a$survey_year[a$survey == 'Gambia 2000 MICS_Datasets'] <- 2000
a$survey_year[a$survey == 'Indonesia MICS2 2000_Datasets'] <- 2000
a$survey_year[a$survey == 'Kenya 2000 MICS_Datasets'] <- 2000
a$survey_year[a$survey == 'Philippines 1999 MICS_Datasets'] <- 1999
a$survey_year[a$survey == 'Uruguay MICS 2012-13 SPSS Datasets'] <- 2012
a$survey_year[a$survey == 'Zambia 1999 MICS_Datasets'] <- 1999

a$survey_day <- NULL
a$survey_month <- NULL
a$survey_month_chr <- NULL
a$survey_year_chr <- NULL
a$survey_day_chr <- NULL
a$survey_date <- NULL

########################
# hhweight
###################

a$hhweight <- as.numeric(a$hhweight)

####################
# wealth_quintile
#######################
a$wealth_quintile <- as.numeric(a$wealth_quintile)
a$wealth_quintile[a$wealth_quintile > 5] <- NA

a$wealth_quintile_chr <- NULL

#####################
# wealth_factor
######################
a$wealth_factor <- as.numeric(a$wealth_factor)

#####################
# toilet_type
####################

a$toilet_type <- case_when(grepl('FLUSH|CHASE|CHASSE|INODORO|PTIC|PTIQ|EGOUT|CANTARILLA|WC LINKED TO SEWER', 
                                 toupper(a$toilet_type_chr)) ~ 'flush toilet',
                           grepl('COMPOST|LETRINA|LATRINE|PIT|LATRINA|VAULT|WC NOT LINKED TO SEWER|FOSSE', 
                                 toupper(a$toilet_type_chr)) ~ 'pit latrine',
                           grepl('NONE|OPEN|NO TIENE|BUSH|NO FACILITIES|NÃO TEM CASA|OUVERT|BROUSSE|CHAMP|PAS DE|NATURE|RIVER|MATO|NO HAY|BUCKET|MISSING|OUED|CAMPO',
                                 toupper(a$toilet_type_chr)) ~ 'none',
                           grepl('OTHER|OTRO|AUTRE', 
                                 toupper(a$toilet_type_chr)) ~ 'other',
                           is.na(a$toilet_type_chr) ~ NA_character_,
                           TRUE ~ "other")

a$toilet_type_chr <- NULL

#################
# material_wall
##################

a$material_wall <- case_when(grepl('CEMENT|BRICKS|CIMENT|BRIQUES|CONCRET|PLASTER', 
                                   toupper(a$material_wall_chr)) ~ 'finished',
                             grepl('STONE|ADOBE|WOOD|TERRE|BOUE|MADEIRA',
                                   toupper(a$material_wall_chr)) ~ 'natural',
                             grepl('DIRT|BAMBO|SHEET|PALM|TIN|SOIL|MUD|TABOOG',
                                   toupper(a$material_wall_chr)) ~ 'rudimentary',
                             grepl('OTHER|PAS DE|NO WALLS|AUTRE',
                                   toupper(a$material_wall_chr)) ~ 'other',
                             is.na(a$material_wall_chr) ~ NA_character_,
                             grepl('YES|NO', 
                                   toupper(a$material_wall_chr)) ~ NA_character_,
                             TRUE ~ 'other')

a$material_wall_chr <- NULL


#################
# material_floor
##################
a$material_floor <- case_when(grepl('CERÁMICA|MOSAIC|MARBLE|TERMINADO|CERAMIC|CEMENT|BRICKS|CIMENT|BRIQUES|CONCRET|PLASTER|CARPET|ALFOM|VINYL|TILES|LAMINATE', 
                                   toupper(a$material_floor_chr)) ~ 'finished',
                             grepl('STONE|ADOBE|WOOD|TERRE|BOUE|MADEIRA|NATURAL|TERRA|SHINGLES',
                                   toupper(a$material_floor_chr)) ~ 'natural',
                             grepl('TIERRA|DIRT|BAMBO|SHEET|PALM|TIN|SOIL|MUD|SAND|NATUREL|DUNG|RUDIMENT|CLAY',
                                   toupper(a$material_floor_chr)) ~ 'rudimentary',
                             grepl('OTHER|PAS DE|NO WALLS|AUTRE',
                                   toupper(a$material_floor_chr)) ~ 'other',
                             is.na(a$material_floor_chr) ~ NA_character_,
                             grepl('YES|NO', 
                                   toupper(a$material_floor_chr)) ~ NA_character_,
                             TRUE ~ 'other')

a$material_floor_chr <- NULL

#################
# material_roof
##################

a$material_roof <- case_when(grepl('IRON|CERAMIC|METAL|CEMENT|BRICKS|CIMENT|BRIQUES|CONCRET|PLASTER|TÔL|ZINC|SHINGLES|SLATE|ASBESTO|RUBEROID|MÉTAL|COROGATED', 
                                   toupper(a$material_roof_chr)) ~ 'finished',
                             grepl('STONE|ADOBE|WOOD|TERRE|BOUE|MADEIRA|NATURAL|BOIS',
                                   toupper(a$material_roof_chr)) ~ 'natural',
                             grepl('DIRT|BAMBO|SHEET|PALM|TIN|SOIL|MUD|TABOOG|SOD|PAILLE|HERBE|RUSTIC|BANCO|THATCH|GRASS',
                                   toupper(a$material_roof_chr)) ~ 'rudimentary',
                             grepl('OTHER|PAS DE|NO WALLS|AUTRE',
                                   toupper(a$material_roof_chr)) ~ 'other',
                             is.na(a$material_roof_chr) ~ NA_character_,
                             grepl('YES|NO', 
                                   toupper(a$material_roof_chr)) ~ NA_character_,
                             TRUE ~ 'other')

a$material_roof_chr <- NULL

#################
# has_electricity
##################

a$has_electricity <- case_when(a$has_electricity == "1" & is.na(a$has_electricity_chr) ~ "Yes",
                               a$has_electricity == "2" & is.na(a$has_electricity_chr) ~ "No",
                               grepl('SEM|^M|FALTA|KNOW|REPONSE|CLAR|OMITIDO',
                                     toupper(a$has_electricity_chr)) ~ NA_character_,
                               grepl('YES|OUI|^S|CTRICI', toupper(a$has_electricity_chr)) ~ 'Yes',
                               grepl('^N', toupper(a$has_electricity_chr)) ~ 'No',
                               TRUE ~ NA_character_)

a$has_electricity_chr <- NULL

#################
# has_radio
##################

a$has_radio <- case_when(a$has_radio == "1" & is.na(a$has_radio_chr) ~ "Yes",
                         a$has_radio == "2" & is.na(a$has_radio_chr) ~ "No",
                         grepl('SEM|^M|FALTA|KNOW|REPONSE|CLAR|OMITIDO|DK',
                               toupper(a$has_radio_chr)) ~ NA_character_,
                         grepl('YES|OUI|^S|RADIO|1', toupper(a$has_radio_chr)) ~ 'Yes',
                         grepl('^N|0', toupper(a$has_radio_chr)) ~ 'No',
                         TRUE ~ a$has_radio_chr)

a$has_radio_chr <- NULL

#################
# has_television
##################

a$has_television <- case_when(a$has_television == "1" & is.na(a$has_television_chr) ~ "Yes",
                         a$has_television == "2" & is.na(a$has_television_chr) ~ "No",
                         grepl('SEM|^M|FALTA|KNOW|REPONSE|CLAR|OMITIDO|DK',
                               toupper(a$has_television_chr)) ~ NA_character_,
                         grepl('YES|OUI|^S|TELEVISION|1', toupper(a$has_television_chr)) ~ 'Yes',
                         grepl('^N|0|WOULD LIKE|DON\'T WANT', toupper(a$has_television_chr)) ~ 'No',
                         TRUE ~ a$has_television_chr)

a$has_television_chr <- NULL

#################
# has_refrigerator
##################

a$has_refrigerator <- case_when(a$has_refrigerator == "1" & is.na(a$has_refrigerator_chr) ~ "Yes",
                         a$has_refrigerator == "2" & is.na(a$has_refrigerator_chr) ~ "No",
                         grepl('SEM|^M|FALTA|KNOW|REPONSE|CLAR|OMITIDO|DK',
                               toupper(a$has_refrigerator_chr)) ~ NA_character_,
                         grepl('YES|OUI|^S|REFRIGERATOR|1', toupper(a$has_refrigerator_chr)) ~ 'Yes',
                         grepl('^N|0|WOULD LIKE|DON\'T WANT', toupper(a$has_refrigerator_chr)) ~ 'No',
                         TRUE ~ a$has_refrigerator_chr)

a$has_refrigerator_chr <- NULL

#################
# has_bicycle
##################

a$has_bicycle <- case_when(a$has_bicycle == "1" & is.na(a$has_bicycle_chr) ~ "Yes",
                         a$has_bicycle == "2" & is.na(a$has_bicycle_chr) ~ "No",
                         grepl('SEM|^M|FALTA|KNOW|REPONSE|CLAR|OMITIDO|DK',
                               toupper(a$has_bicycle_chr)) ~ NA_character_,
                         grepl('YES|OUI|^S|BICYCLE|1', toupper(a$has_bicycle_chr)) ~ 'Yes',
                         grepl('^N|0|WOULD LIKE|DON\'T WANT', toupper(a$has_bicycle_chr)) ~ 'No',
                         TRUE ~ a$has_bicycle_chr)

a$has_bicycle_chr <- NULL

#################
# has_motorcycle
##################

a$has_motorcycle <- case_when(a$has_motorcycle == "1" & is.na(a$has_motorcycle_chr) ~ "Yes",
                         a$has_motorcycle == "2" & is.na(a$has_motorcycle_chr) ~ "No",
                         grepl('SEM|^M|FALTA|KNOW|REPONSE|CLAR|OMITIDO|DK',
                               toupper(a$has_motorcycle_chr)) ~ NA_character_,
                         grepl('YES|OUI|^S|MOTO|1', toupper(a$has_motorcycle_chr)) ~ 'Yes',
                         grepl('^N|0|WOULD LIKE|DON\'T WANT', toupper(a$has_motorcycle_chr)) ~ 'No',
                         TRUE ~ a$has_motorcycle_chr)

a$has_motorcycle_chr <- NULL


###########################
# number_sleeping_rooms
############################
a$number_sleeping_rooms <- as.numeric(a$number_sleeping_rooms)

a$number_sleeping_rooms[a$number_sleeping_rooms > 12] <- NA
a$number_sleeping_rooms_chr <- NULL

########################
# has_car
#######################

a$has_car <- case_when(a$has_car == "1" & is.na(a$has_car_chr) ~ "Yes",
                         a$has_car == "2" & is.na(a$has_car_chr) ~ "No",
                         grepl('SEM|^M|FALTA|KNOW|REPONSE|CLAR|OMITIDO|DK',
                               toupper(a$has_car_chr)) ~ NA_character_,
                         grepl('YES|VOITURE|OUI|^S|CAR|1', toupper(a$has_car_chr)) ~ 'Yes',
                         grepl('^N|0|WOULD LIKE|DON\'T WANT', toupper(a$has_car_chr)) ~ 'No',
                         TRUE ~ a$has_car_chr)

a$has_car_chr <- NULL

########################
# has_telephone
#######################

a$has_telephone <- case_when(a$has_telephone == "1" & is.na(a$has_telephone_chr) ~ "Yes",
                         a$has_telephone == "2" & is.na(a$has_telephone_chr) ~ "No",
                         grepl('SEM|^M|FALTA|KNOW|REPONSE|CLAR|OMITIDO|DK',
                               toupper(a$has_telephone_chr)) ~ NA_character_,
                         grepl('YES|OUI|^S|TELEPHONE|1', toupper(a$has_telephone_chr)) ~ 'Yes',
                         grepl('^N|0|WOULD LIKE|DON\'T WANT', toupper(a$has_telephone_chr)) ~ 'No',
                         TRUE ~ a$has_telephone_chr)

a$has_telephone <- NULL
a$has_telephone_chr <- NULL

########################
# water_source_nondrinking
#######################

a$water_source_nondrinking <- NULL
a$water_source_nondrinking_chr <- NULL

########################
# water_source_drinking
#######################

a$water_source_drinking <- case_when(grepl('pipe|robinet|blic|parcelle', 
                                   tolower(a$water_source_drinking_chr)) ~ 'piped',
                             grepl('tube|bore|deep well|pump|forage|pomp',
                                   tolower(a$water_source_drinking_chr)) ~ 'tube well',
                             grepl('protected|puits prot|source prot|dug well|proteg|protég|amenagee',
                                   tolower(a$water_source_drinking_chr)) ~ 'dug well',
                             grepl('river|unprotected|rain|surface|non pro|não protegido|rio|pluie|rivier|lagoa|rio|fleuve|non couvert|lluvia|pond',
                                   tolower(a$water_source_drinking_chr)) ~ 'surface water',
                             grepl('borne|purchased|bottle|botell|bouteill|truck|other|osmosis|kiosk|camion|sachet|tank|citerne|packaged|dispenser',
                                   tolower(a$water_source_drinking_chr)) ~ 'purchased',
                             is.na(a$water_source_drinking_chr) ~ NA_character_,
                             TRUE ~ NA_character_)

a$water_source_drinking_chr <- NULL

write.csv(a, '~/mortalityblob/dhs/mics_clean.csv', row.names=F)

