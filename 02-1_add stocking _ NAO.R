 ##### 01. SETUP #########
# subsetting and preparing Breitenbach datasets for the analysis#
#package  installation
pckg <- installed.packages()
req.pkg <- c("readxl", "openxlsx", "dplyr", "tidyr")
inst.pkg <- req.pkg[!(req.pkg %in% pckg[,1])]
install.packages(inst.pkg)
lapply(req.pkg, library, character.only=T)
rm(inst.pkg,req.pkg, pckg)

##### 02. DATA IMPORT, CLEANUP AND PREPARATION #########
fish <- read_excel("elevation & temperature data.xlsx")
fish.raw <- read_excel("raw data.xlsx", col_types = c("text", "text", "date", "numeric", "numeric", "numeric", "text",
                                                  "text", "numeric", "numeric", "numeric", "numeric", "numeric",
                                                  "numeric", "numeric", "numeric", "numeric", "numeric", "numeric",
                                                  "numeric", "numeric", "numeric", "numeric", "numeric", "numeric"))
# fish.area <- read_excel("REPOBLADOS BD_peces_Cantabria_2010-2020_V6.xlsm", sheet="Estimaciónes C-S SIN ADIPOSA",
#                         col_types = c("skip", "text", "date", "skip", "skip", "skip", "numeric", "numeric", "skip", "skip",
#                                       "skip", "skip", "skip", "skip", "skip", "skip", "skip", "skip", "skip", "skip",
#                                       "skip", "skip", "skip", "skip", "skip", "skip", "skip", "skip", "skip", "skip",
#                                       "skip", "skip", "skip", "skip", "skip", "skip", "skip", "skip", "skip", "skip",
#                                       "skip", "skip", "skip", "skip", "skip", "skip", "skip", "skip", "skip", "skip",
#                                       "skip", "skip", "skip", "skip", "skip", "skip", "skip", "skip", "skip", "skip",
#                                       "skip", "skip", "skip", "skip", "skip", "skip", "skip", "skip", "skip", "skip",
#                                       "skip", "skip", "skip", "skip", "skip", "skip", "skip", "skip", "skip", "skip",
#                                       "skip", "skip", "skip", "skip", "skip", "skip", "skip", "skip", "skip", "skip",
#                                       "skip", "skip", "skip", "skip", "skip", "skip", "skip", "skip", "skip", "skip",
#                                       "skip", "skip", "skip", "skip", "skip"))
# colnames(fish.area) <- c("station", "code", "length", "width")
fish.area.all <- read_excel("sampling sites_area.xlsx", col_types = c("text", "date", "numeric", "numeric"))
# stations <- unique(fish.area)
stations.all <- unique(fish.area.all)

fish$old.code <- tolower(fish.raw$old_code)
fish$new.code <- fish.raw$code_2019

fish <- fish[,c(28,29,1:27)]

## fix rivers
fish$river[fish$river=="Aguera"] <- "Agüera"
fish$river[fish$river=="Arroyo de Sebrando (p)"] <- "Arroyo de Sebrando"
fish$river[fish$river=="Bullon"] <- "Bullón"
fish$river[fish$river=="Bustablao"] <- "Bustablado"
fish$river[fish$river=="Frio"] <- "Frío"
fish$river[fish$river=="Gandara"] <- "Gándara"
fish$river[fish$river=="Hijar"] <- "Híjar"
fish$river[fish$river=="Tanea-LamAson"] <- "Tanea-Lamason"
fish$river[fish$river=="Urdon"] <- "Urdón"

## calculate ratio of stocked individuals
captures <- read_excel("raw data.xlsx", sheet="Capturas 2010-2019", col_types = c("text", "text", "text", "date", "text",
                                                                                  "skip", "numeric", "numeric", "skip", "numeric",
                                                                                  "skip", "skip", "skip", "text", "skip", "skip", "skip"))

captures.sum <- group_by(captures, old_code, code_2018, code_2019, date, species, age) %>% summarise_at(vars(adip, field_weight, weight), funs(sum(.,na.rm=T)))
captures.n <- group_by(captures, old_code, code_2018, code_2019, date, species, age) %>% count()
captures.sum$ind <- captures.n$n
rm(captures.n, captures)

captures.sum$ratio <- captures.sum$adip / captures.sum$ind
capt <- ungroup(captures.sum)
rm(captures.sum)

## create unique ID per sampling
fish <- fish[order(fish$date),]
samples <- unique(fish[,c(3,5,6)]) # unique combination of location & date -> different ID for each sampling
samples$ID <- seq(1, nrow(samples), 1)

# add ID to fish
fish$ID <- mapply(function(date, X, Y) samples$ID[samples$date==date & samples$UTM.X==X & samples$UTM.Y==Y], date=fish$date, X=fish$UTM.X, Y=fish$UTM.Y)

## create unique ID per site
sites <- unique(capt[,c(1,3)])
sites$old_code <- tolower(sites$old_code)
sites <- unique(sites)
sites$code_2019[is.na(sites$code_2019)] <- sites$old_code[is.na(sites$code_2019)]
sites <- data.frame(unique(sites$code_2019))
sites <- data.frame(sites[order(sites[,1]),])
colnames(sites) <- "code"
sites$ID <- seq(1, nrow(sites), 1)

# add site ID to fish
fish$site.ID <- mapply(function(old.code, code19) ifelse(is.na(code19),
                                                         sites$ID[sites$code==old.code],
                                                         sites$ID[sites$code==code19])
                       , old.code=fish$old.code, code19=fish$new.code)

## NAO
NAO <- read_excel("NAOIndex_CRU.xlsx")
NAO <- NAO[190:199,1:13]
colnames(NAO) <- c("year", seq(01,12,1))
NAO <- pivot_longer(NAO, c(2:13), names_to = "month")

##### 03. SUMMARISE DATA PER SAMPLING #####
samplings <- pivot_wider(capt, id_cols = c(old_code, code_2018, code_2019, date, species), names_from = age, values_from = c(adip, field_weight, weight, ind, ratio))
samplings$adip_tot <- rowSums(samplings[,6:9], na.rm = T)
samplings$field.weight_tot <- rowSums(samplings[,10:13], na.rm = T)
samplings$weight_tot <- rowSums(samplings[,14:17], na.rm = T)
samplings$ind_tot <- rowSums(samplings[,18:21], na.rm = T)
samplings$ratio_tot <- samplings$adip_tot/samplings$ind_tot
samplings$year <- format(samplings$date, format="%Y")
samplings <- samplings[,c(1:4,31,5:9,26,10:13,27,14:17,28,18:21,29,22:25,30)]

# add ID to samplings
samplings$ID <- NA
samplings$old_code <- tolower(samplings$old_code) #change all to lower case
samplings$ID[samplings$year<2018] <- mapply(function(code, date) fish$ID[fish$old.code==code & fish$date==date][1],
                                            code=samplings$old_code[samplings$year<2018], date=samplings$date[samplings$year<2018])
samplings$ID[samplings$year==2018] <- mapply(function(code, date) fish$ID[fish$old.code==code & fish$date==date][1],
                                            code=samplings$code_2018[samplings$year==2018], date=samplings$date[samplings$year==2018])
samplings$ID[samplings$year==2019] <- mapply(function(code, date) fish$ID[fish$new.code==code & fish$date==date][1],
                                             code=samplings$code_2019[samplings$year==2019], date=samplings$date[samplings$year==2019])
samplings$ID <- as.numeric(samplings$ID)

# add site ID to samplings
samplings$site.ID <- mapply(function(old.code, code19) ifelse(is.na(code19),
                                                         sites$ID[sites$code==old.code],
                                                         sites$ID[sites$code==code19])
                       , old.code=samplings$old_code, code19=samplings$code_2019)

##### 04. ADD LENGTH AND WIDTH #####
# add length to samplings
samplings$length <- NA
stations.all$station <- tolower(stations.all$station) #change all to lower case
samplings$length[samplings$year<2018] <- mapply(function(code, date) stations.all$length[stations.all$station==code & stations.all$date==date],
                                            code=samplings$old_code[samplings$year<2018], date=samplings$date[samplings$year<2018])
samplings$length[samplings$year==2018] <- mapply(function(code, date) stations.all$length[stations.all$station==code & stations.all$date==date],
                                             code=samplings$code_2018[samplings$year==2018], date=samplings$date[samplings$year==2018])
samplings$length[samplings$year==2019] <- mapply(function(code, date) stations.all$length[stations.all$station==code & stations.all$date==date],
                                             code=samplings$code_2019[samplings$year==2019], date=samplings$date[samplings$year==2019])
samplings$length <- as.numeric(samplings$length)

# add width to samplings
samplings$width <- NA
samplings$width[samplings$year<2018] <- mapply(function(code, date) stations.all$width[stations.all$station==code & stations.all$date==date],
                                                code=samplings$old_code[samplings$year<2018], date=samplings$date[samplings$year<2018])
samplings$width[samplings$year==2018] <- mapply(function(code, date) stations.all$width[stations.all$station==code & stations.all$date==date],
                                                 code=samplings$code_2018[samplings$year==2018], date=samplings$date[samplings$year==2018])
samplings$width[samplings$year==2019] <- mapply(function(code, date) stations.all$width[stations.all$station==code & stations.all$date==date],
                                                 code=samplings$code_2019[samplings$year==2019], date=samplings$date[samplings$year==2019])
samplings$width <- as.numeric(samplings$width)

# calculate area
samplings$area <- samplings$length * samplings$width

## reorder
samplings <- samplings[,c(1:3,33:36,4:32)]

##### 05. CALCULATE BIOMASS & DENSITY #####
## biomass
samplings$bm.0 <- samplings$`weight_0+` / samplings$area
samplings$bm.1 <- samplings$`weight_1+` / samplings$area
samplings$bm.adult <- samplings$`weight_adult` / samplings$area
samplings$bm_NA <- samplings$`weight_NA` / samplings$area
samplings$bm <- samplings$`weight_tot` / samplings$area

## density
samplings$dens.0 <- samplings$`ind_0+` / samplings$area
samplings$dens.1 <- samplings$`ind_1+` / samplings$area
samplings$dens.adult <- samplings$`ind_adult` / samplings$area
samplings$dens_NA <- samplings$`ind_NA` / samplings$area
samplings$dens <- samplings$`ind_tot` / samplings$area

##### 06. ADD NAO #####
fish$month <- as.numeric(format(fish$date,"%m"))
fish$NAO <- mapply(function(year, month) NAO$value[NAO$year==year & NAO$month==month], year=fish$year, month=fish$month)

##### 07. CREATE DATAFRAME WITH RIVERS & COORDINATES & CAPTURES #####
newdata <- data.frame(full_join(samplings[,4:46], fish[,c(7,21:30,32:33)], by="ID"))
newdata <- newdata[,c(5:7,1,33,54,44,2:4,50:53,45:49,55,8:32,34:43)]
newdata <- newdata[!is.na(newdata$ID),] #keep samplings with coordinates only
newdata <- newdata[!is.na(newdata$length),] #keep samplings with area information only

#remove duplicates
newdata <- newdata[!duplicated(newdata),]

## add number of years site was sampled
newdata$no.years <- as.numeric(sapply(newdata$site.ID, function(i) length(unique(newdata$year[newdata$site.ID==i]))))
newdata <- newdata[,c(1:4,56,5:55)]

#remove NA age class
newdata.age <- dplyr::select(newdata, -contains("_NA"))

noEbro <- newdata.age[newdata.age$river!="Ebro",]

##add sampling effort
newdata.age$effort <- sapply(newdata.age$year, function(i) length(unique(newdata.age$date[newdata.age$year==i])))
noEbro$effort <- sapply(noEbro$year, function(i) length(unique(noEbro$date[noEbro$year==i])))

newdata.age <- newdata.age[,c(1:4,50,5:49)]
noEbro <- noEbro[,c(1:4,50,5:49)]

##### 08. SAVE FILES #####
write.xlsx(newdata, "data from captures_with NA age class.xlsx")
write.xlsx(newdata.age, "data from captures.xlsx")
write.xlsx(noEbro, "data from captures_without Ebro.xlsx")
