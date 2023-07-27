##### 01. SETUP #########
#package  installation
pckg <- installed.packages()
req.pkg <- c("readxl", "rgdal","elevatr", "sp", "raster", "ncdf4", "openxlsx")
inst.pkg <- req.pkg[!(req.pkg %in% pckg[,1])]
install.packages(inst.pkg)
lapply(req.pkg, library, character.only=T)
rm(inst.pkg,req.pkg, pckg)

##### 02. DATA IMPORT, CLEANUP AND PREPARATION #########
fish <- read_excel("fixed data.xlsx", col_types = c("date", "numeric", "numeric", "numeric", "text",
                                                  "text", "numeric", "numeric", "numeric", "numeric", "numeric",
                                                  "numeric", "numeric", "numeric", "numeric", "numeric", "numeric",
                                                  "numeric", "numeric", "numeric", "numeric", "numeric", "numeric"))
fish$date <- as.Date(fish$date)
fish.coord <- SpatialPoints(fish[,c(3,4)], proj4string=CRS("+proj=utm +zone=30"))  
fish.WGS <- spTransform(fish.coord, CRS("+proj=longlat"))

##### 03. ADD DATA #########
#### 3.1 elevation ####
elev <- get_elev_point(fish.WGS, src = "aws")

fish$WGSn <- elev@coords[,2]
fish$WGSe <- elev@coords[,1]

fish$elevation <- elev@data$elevation

#### 3.2 air temperature ####
# function to extract mean air temperature
# Download file "tg_0.25deg_reg_v17.0.nc.gz" from: https://www.ecad.eu/download/ensembles/download.php#datafiles
getAIR <- function(lon, lat, timepoint) {    
  # Mean daily temperature
  ncin <- nc_open("tg_ens_mean_0.25deg_reg_1995-2010_v21.0e.nc")
  ncin2 <- nc_open("tg_ens_mean_0.25deg_reg_2011-2019_v21.0e.nc")
  
  print(ncin)
  print(ncin2)
  
  # 1995-2010
  t <- ncvar_get(ncin,"time")
  tunits <- ncatt_get(ncin,"time","units")
  nt <- dim(t)
  obsoutput <- ncvar_get(ncin, # look for closest lon and lat
                         start = c(which.min(abs(ncin$dim$longitude$vals - lon)),
                                   which.min(abs(ncin$dim$latitude$vals - lat)),
                                   1),
                         count = c(1,1,-1))
  DataMeanT <- data.frame(DateN = t, MeanDailyT = obsoutput)
  nc_close(ncin)
  
  # 2011-2019
  t <- ncvar_get(ncin2,"time")
  tunits <- ncatt_get(ncin2,"time","units")
  nt <- dim(t)
  obsoutput <- ncvar_get(ncin2, # look for closest lon and lat
                         start = c(which.min(abs(ncin2$dim$longitude$vals - lon)),
                                   which.min(abs(ncin2$dim$latitude$vals - lat)),
                                   1),
                         count = c(1,1,-1))
  
  DataMeanT2 <- data.frame(DateN = t, MeanDailyT = obsoutput)
  nc_close(ncin2) 
  
  #combine data
  Data=rbind(DataMeanT, DataMeanT2)
  Data$Date=as.Date(Data$DateN,origin="1950-01-01")
  
  return(Data$MeanDailyT[Data$Date == timepoint])
} 

fish$Tair <- mapply(getAIR, fish$WGSe, fish$WGSn, fish$date)

##### 04. store data #####
write.xlsx(fish, "elevation & temperature data.xlsx")
