##### 01. SETUP #########
#package  installation
pckg <- installed.packages()
req.pkg <- c("readxl", "raster", "maps", "rgdal", "mapdata", "modifiedmk", "openxlsx")
inst.pkg <- req.pkg[!(req.pkg %in% pckg[,1])]
install.packages(inst.pkg)
lapply(req.pkg, library, character.only=T)
rm(inst.pkg,req.pkg, pckg)

##### 02. DATA IMPORT, CLEANUP AND PREPARATION #########
# fish <- read_excel("data from captures.xlsx")
fish <- read_excel("data from captures_restricted.xlsx")
# fish.nEbro <- read_excel("data from captures_without Ebro.xlsx")
colnames(fish) <- c("date", "year", "species", "siteID", "effort", "no.years", "ID", "month", "river", "length", "width", "area", "WGSn", "WGSe", "elevation",
                    "Tair", "Twater", "pH", "conductivity", "pc.O2", "O2", "NAO", "adip.0", "adip.1", "adip.adult", "adip",
                    "field_weight.0", "field_weight.1", "field_weight.adult", "field.weight", "weight.0", "weight.1", "weight.adult",
                    "weight", "ind.0", "ind.1", "ind.adult", "ind", "ratio.0", "ratio.1", "ratio.adult", "ratio", "bm.0", "bm.1",
                    "bm.adult", "bm", "dens.0", "dens.1", "dens.adult", "dens")
# colnames(fish.nEbro) <- c("date", "year", "species", "siteID", "effort", "no.years", "ID", "month", "river", "length", "width", "area", "WGSn", "WGSe", "elevation",
#                           "Tair", "Twater", "pH", "conductivity", "pc.O2", "O2", "NAO", "adip.0", "adip.1", "adip.adult", "adip",
#                           "field_weight.0", "field_weight.1", "field_weight.adult", "field.weight", "weight.0", "weight.1", "weight.adult",
#                           "weight", "ind.0", "ind.1", "ind.adult", "ind", "ratio.0", "ratio.1", "ratio.adult", "ratio", "bm.0", "bm.1",
#                           "bm.adult", "bm", "dens.0", "dens.1", "dens.adult", "dens")
fish$year <- as.numeric(fish$year)
# fish.nEbro$year <- as.numeric(fish.nEbro$year)

fish <- fish[order(fish$date),]

sites <- read_excel("sites_restricted.xlsx", sheet = "Sheet 1")
sites$no.years <- as.numeric(sites$no.years)
# sites <- fish[,c(1:4,6,10:15)]
# sites <- sites[order(sites$siteID, sites$year),]
# sites <- sites[!duplicated(sites[,c(1,2,4:11)]),c(1,2,4:11)]
# sites.unique <- unique(sites[,c(3,4)])
# sites.unique$years <- sapply(sites.unique$siteID, function(i) sites$year[sites$siteID==i])
# sites.unique$WGSn <- sapply(sites.unique$siteID, function(i) mean(sites$WGSn[sites$siteID==i], na.rm=T))
# sites.unique$WGSe <- sapply(sites.unique$siteID, function(i) mean(sites$WGSe[sites$siteID==i], na.rm=T))
# sites.unique$elevation <- sapply(sites.unique$siteID, function(i) mean(sites$elevation[sites$siteID==i], na.rm=T))

samplings <- read_excel("samplings_restricted.xlsx")

# write.xlsx(sites, "all sampled sites.xlsx")
# write.xlsx(sites.unique, "sites summary.xlsx")

##### 03. CORRELATION T WATER & T AIR ######
plot(fish$Twater, fish$Tair)
cor.test(fish$Twater, fish$Tair)
cor.test(fish$Twater, fish$Tair, method="spearman")

##### 04. SPATIAL PLOT ######
# mp <- NULL
# mapWorld <- borders("world", "Spain", colour="gray40", fill="gray80") # create a layer of borders
# mp <- ggplot() +   mapWorld
# 
# #Now Layer the cities on top
# mp +
#   geom_point(aes(x=fish$WGSe, y=fish$WGSn) ,color="blue", size=0.5)

spain<-getData("GADM",country="ESP",level=1)
rivers <- shapefile("gis_osm_waterways_free_1.shp")
weirs <- shapefile("hi_presa_l_ES018.shp")
weirs <- spTransform(weirs, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))
weirs.pt <- as(weirs, "SpatialPointsDataFrame")
weirs.pt@coords <- weirs.pt@coords[c(1,6,9,12,21,117,124,127,130,170,175,182,186,193,195,199,207,214,220,223),] # only keep one point per weir
weirs2 <- shapefile("hi_presa_s_ES018.shp")
weirs2 <- spTransform(weirs2, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))
weirs3 <- read_excel("weirs.xlsx", sheet = "Spain - study area")

plot(spain,xlim=c(-4.85,-3.15),ylim=c(42.8,43.5),col="gray90",border="gray40",axes=T,las=1)
plot(rivers, col="lightblue", add=T)
points(fish$WGSe, fish$WGSn, pch=19, col="red", cex=0.8)
points(weirs3$Longitude_WGS84, weirs3$Latitude_WGS84, pch=45, col="black")
plot(weirs.pt, pch=45, cex=1.2, add=T, col="black")

svg(filename="map spain.svg")
plot(spain,xlim=c(-4.85,-3.15),ylim=c(42.8,43.5),col="gray90",border="gray40",axes=T,las=1)
plot(rivers, col="#88C7FF", add=T)
points(sites$WGSe, sites$WGSn, pch=19, col="#D81B60", cex=1)
dev.off()

############### unused maps #################
# svg(filename="map spain_weirs.svg")
# plot(spain,xlim=c(-4.85,-3.15),ylim=c(42.8,43.5),col="gray90",border="gray40",axes=T,las=1)
# plot(rivers, col="lightblue", add=T)
# points(fish$WGSe, fish$WGSn, pch=19, col="red", cex=0.8)
# points(weirs3$Longitude_WGS84, weirs3$Latitude_WGS84, pch=45, col="black")
# dev.off()
# 
# ##years
# svg(filename="map spain 2010.svg")
# plot(spain,xlim=c(-4.85,-3.15),ylim=c(42.8,43.5),col="gray90",border="gray40",axes=T,las=1)
# plot(rivers, col="lightblue", add=T)
# points(sites$WGSe[sites$year==2010], sites$WGSn[sites$year==2010], pch=19, col="red", cex=0.8)
# dev.off()
# 
# svg(filename="map spain 2011.svg")
# plot(spain,xlim=c(-4.85,-3.15),ylim=c(42.8,43.5),col="gray90",border="gray40",axes=T,las=1)
# plot(rivers, col="lightblue", add=T)
# points(sites$WGSe[sites$year==2011], sites$WGSn[sites$year==2011], pch=19, col="red", cex=0.8)
# dev.off()
# 
# svg(filename="map spain 2012.svg")
# plot(spain,xlim=c(-4.85,-3.15),ylim=c(42.8,43.5),col="gray90",border="gray40",axes=T,las=1)
# plot(rivers, col="lightblue", add=T)
# points(sites$WGSe[sites$year==2012], sites$WGSn[sites$year==2012], pch=19, col="red", cex=0.8)
# dev.off()
# 
# svg(filename="map spain 2013.svg")
# plot(spain,xlim=c(-4.85,-3.15),ylim=c(42.8,43.5),col="gray90",border="gray40",axes=T,las=1)
# plot(rivers, col="lightblue", add=T)
# points(sites$WGSe[sites$year==2013], sites$WGSn[sites$year==2013], pch=19, col="red", cex=0.8)
# dev.off()
# 
# svg(filename="map spain 2014.svg")
# plot(spain,xlim=c(-4.85,-3.15),ylim=c(42.8,43.5),col="gray90",border="gray40",axes=T,las=1)
# plot(rivers, col="lightblue", add=T)
# points(sites$WGSe[sites$year==2014], sites$WGSn[sites$year==2014], pch=19, col="red", cex=0.8)
# dev.off()
# 
# svg(filename="map spain 2015.svg")
# plot(spain,xlim=c(-4.85,-3.15),ylim=c(42.8,43.5),col="gray90",border="gray40",axes=T,las=1)
# plot(rivers, col="lightblue", add=T)
# points(sites$WGSe[sites$year==2015], sites$WGSn[sites$year==2015], pch=19, col="red", cex=0.8)
# dev.off()
# 
# svg(filename="map spain 2016.svg")
# plot(spain,xlim=c(-4.85,-3.15),ylim=c(42.8,43.5),col="gray90",border="gray40",axes=T,las=1)
# plot(rivers, col="lightblue", add=T)
# points(sites$WGSe[sites$year==2016], sites$WGSn[sites$year==2016], pch=19, col="red", cex=0.8)
# dev.off()
# 
# svg(filename="map spain 2017.svg")
# plot(spain,xlim=c(-4.85,-3.15),ylim=c(42.8,43.5),col="gray90",border="gray40",axes=T,las=1)
# plot(rivers, col="lightblue", add=T)
# points(sites$WGSe[sites$year==2017], sites$WGSn[sites$year==2017], pch=19, col="red", cex=0.8)
# dev.off()
# 
# svg(filename="map spain 2018.svg")
# plot(spain,xlim=c(-4.85,-3.15),ylim=c(42.8,43.5),col="gray90",border="gray40",axes=T,las=1)
# plot(rivers, col="lightblue", add=T)
# points(sites$WGSe[sites$year==2018], sites$WGSn[sites$year==2018], pch=19, col="red", cex=0.8)
# dev.off()
# 
# svg(filename="map spain 2019.svg")
# plot(spain,xlim=c(-4.85,-3.15),ylim=c(42.8,43.5),col="gray90",border="gray40",axes=T,las=1)
# plot(rivers, col="lightblue", add=T)
# points(sites$WGSe[sites$year==2019], sites$WGSn[sites$year==2019], pch=19, col="red", cex=0.8)
# dev.off()
# 
# ##10 years
# svg(filename="map spain 10 years.svg")
# plot(spain,xlim=c(-4.85,-3.15),ylim=c(42.8,43.5),col="gray90",border="gray40",axes=T,las=1)
# plot(rivers, col="lightblue", add=T)
# points(sites.unique$WGSe[sites.unique$no.years==10], sites.unique$WGSn[sites.unique$no.years==10], pch=19, col="red", cex=0.8)
# dev.off()
# 
# ##>9 years
# svg(filename="map spain 9+ years.svg")
# plot(spain,xlim=c(-4.85,-3.15),ylim=c(42.8,43.5),col="gray90",border="gray40",axes=T,las=1)
# plot(rivers, col="lightblue", add=T)
# points(sites.unique$WGSe[sites.unique$no.years>=9], sites.unique$WGSn[sites.unique$no.years>=9], pch=19, col="red", cex=0.8)
# dev.off()
# 
# ##>8 years
# svg(filename="map spain 8+ years.svg")
# plot(spain,xlim=c(-4.85,-3.15),ylim=c(42.8,43.5),col="gray90",border="gray40",axes=T,las=1)
# plot(rivers, col="lightblue", add=T)
# points(sites.unique$WGSe[sites.unique$no.years>=8], sites.unique$WGSn[sites.unique$no.years>=8], pch=19, col="red", cex=0.8)
# dev.off()

##### 05. MANN-KENDALL TEST #####
# create function
autoCOR <- function(variable){
  # create timeseries object
  output.ts <- ts(variable, start = min(fish$year), frequency = 100)
  # check temporal autocorrelation
  acf(output.ts, na.action = na.pass)
  pacf(output.ts, na.action = na.pass)
}
autoCORall <- function(data){
  par(mfrow=c(1,2))
  autoCOR(data$ind)
  mtext(expression(bold("indidviduals")), side = 3, line = -1.2, outer = T, cex=1.5)
  autoCOR(data$ind.0)
  mtext(expression(bold("indidividuals age class 0+")), side = 3, line = -1.2, outer = T, cex=1.5)
  autoCOR(data$ind.1)
  mtext(expression(bold("indidividuals age class 1+")), side = 3, line = -1.2, outer = T, cex=1.5)
  autoCOR(data$ind.adult)
  mtext(expression(bold("indidividuals age class adult")), side = 3, line = -1.2, outer = T, cex=1.5)
  autoCOR(data$dens)
  mtext(expression(bold("density")), side = 3, line = -1.2, outer = T, cex=1.5)
  autoCOR(data$dens.0)
  mtext(expression(bold("density age class 0+")), side = 3, line = -1.2, outer = T, cex=1.5)
  autoCOR(data$dens.1)
  mtext(expression(bold("density age class 1+")), side = 3, line = -1.2, outer = T, cex=1.5)
  autoCOR(data$dens.adult)
  mtext(expression(bold("density age class adult")), side = 3, line = -1.2, outer = T, cex=1.5)
  autoCOR(data$bm)
  mtext(expression(bold("biomass")), side = 3, line = -1.2, outer = T, cex=1.5)
  autoCOR(data$bm.0)
  mtext(expression(bold("biomass age class 0+")), side = 3, line = -1.2, outer = T, cex=1.5)
  autoCOR(data$bm.1)
  mtext(expression(bold("biomass age class 1+")), side = 3, line = -1.2, outer = T, cex=1.5)
  autoCOR(data$bm.adult)
  mtext(expression(bold("biomass age class adult")), side = 3, line = -1.2, outer = T, cex=1.5)
  autoCOR(data$ratio)
  mtext(expression(bold("stocking ratio")), side = 3, line = -1.2, outer = T, cex=1.5)
  autoCOR(data$ratio.0)
  mtext(expression(bold("stocking ratio age class 0+")), side = 3, line = -1.2, outer = T, cex=1.5)
  autoCOR(data$ratio.1)
  mtext(expression(bold("stocking ratio age class 1+")), side = 3, line = -1.2, outer = T, cex=1.5)
  autoCOR(data$ratio.adult)
  mtext(expression(bold("stocking ratio age class adult")), side = 3, line = -1.2, outer = T, cex=1.5)
  autoCOR(data$Twater)
  mtext(expression(bold("T water")), side = 3, line = -1.2, outer = T, cex=1.5)
  autoCOR(data$Tair)
  mtext(expression(bold("T air")), side = 3, line = -1.2, outer = T, cex=1.5)
  autoCOR(data$pH)
  mtext(expression(bold("pH")), side = 3, line = -1.2, outer = T, cex=1.5)
  autoCOR(data$conductivity)
  mtext(expression(bold("conductivity")), side = 3, line = -1.2, outer = T, cex=1.5)
  autoCOR(data$pc.O2)
  mtext(expression(bold("% O2")), side = 3, line = -1.2, outer = T, cex=1.5)
  autoCOR(data$O2)
  mtext(expression(bold("O2")), side = 3, line = -1.2, outer = T, cex=1.5)
  autoCOR(data$NAO)
  mtext(expression(bold("NAO")), side = 3, line = -1.2, outer = T, cex=1.5)
}

autoCORall(fish)
autoCORall(fish[fish$species=="Salmo salar",])
autoCORall(fish[fish$species=="Salmo trutta fario",])

dev.off()

MannKendallTEST <- function(variable, name) cbind(variable=name, data.frame(as.list(My.mmkh(variable))))
MannKendallTESTresponse <- function(data){
  rbind(MannKendallTEST(data$ind, "ind"),
        MannKendallTEST(data$ind.0, "ind.0"),
        MannKendallTEST(data$ind.1, "ind.1"),
        MannKendallTEST(data$ind.adult, "ind.adult"),
        MannKendallTEST(data$dens, "dens"),
        MannKendallTEST(data$dens.0, "dens.0"),
        MannKendallTEST(data$dens.1, "dens.1"),
        MannKendallTEST(data$dens.adult, "dens.adult"),
        MannKendallTEST(data$bm, "bm"),
        MannKendallTEST(data$bm.0, "bm.0"),
        MannKendallTEST(data$bm.1, "bm.1"),
        MannKendallTEST(data$bm.adult, "bm.adult"),
        MannKendallTEST(data$ratio, "ratio"),
        MannKendallTEST(data$ratio.0, "ratio.0"),
        MannKendallTEST(data$ratio.1, "ratio.1"),
        MannKendallTEST(data$ratio.adult, "ratio.adult"))
}
MannKendallTESTresponse.s <- function(data){
  rbind(MannKendallTEST(data$ind, "ind"),
        MannKendallTEST(data$ind.0, "ind.0"),
        MannKendallTEST(data$ind.1, "ind.1"),
        MannKendallTEST(data$dens, "dens"),
        MannKendallTEST(data$dens.0, "dens.0"),
        MannKendallTEST(data$dens.1, "dens.1"),
        MannKendallTEST(data$bm, "bm"),
        MannKendallTEST(data$bm.0, "bm.0"),
        MannKendallTEST(data$bm.1, "bm.1"),
        MannKendallTEST(data$ratio, "ratio"),
        MannKendallTEST(data$ratio.0, "ratio.0"),
        MannKendallTEST(data$ratio.1, "ratio.1"))
}
MannKendallTESTenv <- function(data){
  rbind(MannKendallTEST(data$Twater, "Twater"),
        MannKendallTEST(data$Tair, "Tair"),
        MannKendallTEST(data$pH, "pH"),
        MannKendallTEST(data$conductivity, "conductivity"),
        MannKendallTEST(data$pc.O2, "pc.O2"),
        MannKendallTEST(data$O2, "O2"),
        MannKendallTEST(data$effort, "sampling effort"))
}
MKresults.response <- MannKendallTESTresponse(fish)
MKresults.env <- MannKendallTESTenv(fish)

MKresults.salmon <- MannKendallTESTresponse.s(fish[fish$species=="Salmo salar",])
MKresults.trout <- MannKendallTESTresponse(fish[fish$species=="Salmo trutta fario",])

colnames(MKresults.response)[2:6] <- c("Z.corr", "p.value", "N.N", "Z.original", "p.value.old")
colnames(MKresults.env)[2:6] <- c("Z.corr", "p.value", "N.N", "Z.original", "p.value.old")
colnames(MKresults.salmon)[2:6] <- c("Z.corr", "p.value", "N.N", "Z.original", "p.value.old")
colnames(MKresults.trout)[2:6] <- c("Z.corr", "p.value", "N.N", "Z.original", "p.value.old")

write.xlsx(MKresults.response, "Mann-Kendall results response variables.xlsx")
write.xlsx(MKresults.env, "Mann-Kendall results environmental variables.xlsx")
write.xlsx(MKresults.salmon, "Mann-Kendall results_salmon.xlsx")
write.xlsx(MKresults.trout, "Mann-Kendall results_trout.xlsx")

#### 5.1 plot results #####
MKresults.response <- MKresults.response[c(nrow(MKresults.response):1), ]
MKresults.env <- MKresults.env[c(nrow(MKresults.env):1), ]
MKresults.salmon <- MKresults.salmon[c(nrow(MKresults.salmon):1),]
MKresults.trout <- MKresults.trout[c(nrow(MKresults.trout):1),]

par(mfrow=c(1, 1), omi = c(0.2, 0.2, 0, 0))
par(mar = c(2.5, 14, 1.5, 1.5)) # this sets the margins (bottom, left, top, right)
labels_resp <- c("Individuals", "Individuals age class 0", "Individuals age class 1+", "Individuals adult",
                    "Density", "Density age class 0", "Density age class 1+", "Density adult",
                    "Biomass", "Biomass age class 0", "Biomass age class 1+", "Biomass adult",
                 "Stocking ratio", "Stocking ratio age class 0", "Stocking ratio age class 1+", "Stocking ratio adult")
labels_env <- c("T water", "T air", "pH", "conductivity", "% O2", "O2", "sampling effort")
labels_salmon <- c("Individuals", "Individuals age class 0", "Individuals age class 1+",
                 "Density", "Density age class 0", "Density age class 1+",
                 "Biomass", "Biomass age class 0", "Biomass age class 1+",
                 "Stocking ratio", "Stocking ratio age class 0", "Stocking ratio age class 1+")
labels_resp <- rev(labels_resp)
labels_env <- rev(labels_env)
labels_salmon <- rev(labels_salmon)

axiscolors_resp <- rep("#000000", length(labels_resp))
axiscolors_env <- rep("#000000", length(labels_env))
axiscolors_salmon <- rep("#000000", length(labels_salmon))

plotMANNKENDALL <- function(data, labels, axiscolors="black",title=NULL){
  plot(data$Tau, c(1:length(data$Tau)), 
       main = "", frame.plot = TRUE, 
       xlab = "", ylab = "", axes = F, xlim=c(-0.3, 0.2),
       col = ifelse(data$p.value < 0.05, "black", "darkgrey"), 
       lty = ifelse(data$p.value < 0.05, 'solid', 'dashed'),
       pch = 15, lwd = 5) #if we want to code specific colours to a point use col = "column name"
  axis(1, seq(-0.4, 0.3, 0.1), labels = T)
  axis(2, at = c(1:length(data$Tau)), labels = F,
       rownames(data), las = 1, cex.axis = 1)
  abline(v = 0, lty = 2)
  segments(x0 = 0, x1 = data$Tau, col = ifelse(data$p.value < 0.05, "black", "darkgrey"), 
           lty = ifelse(data$p.value < 0.05, 'solid', 'dashed'),
           y0 = c(1:length(data$Tau)), 
           y1 = c(1:length(data$Tau)))
  mtext("Mann-Kendall Tau", side = 1, outer = T, at = 0.73, las = 1, cex = 1, font = 2)
  Map(axis, side = 2, at = 1:32, col.axis = axiscolors, labels = labels, las = 1)
  title(title)
}

svg(filename="MK all.svg")
par(mfrow=c(1, 1), omi = c(0.2, 0.2, 0, 0))
par(mar = c(2.5, 14, 1.5, 1.5)) # this sets the margins (bottom, left, top, right)
plotMANNKENDALL(MKresults.response, labels_resp, axiscolors_resp)
dev.off()

svg(filename="MK env.svg")
par(mfrow=c(1, 1), omi = c(0.2, 0.2, 0, 0))
par(mar = c(2.5, 14, 1.5, 1.5)) # this sets the margins (bottom, left, top, right)
plotMANNKENDALL(MKresults.env, labels_env, axiscolors_env)
dev.off()

svg(filename="MK salmon.svg")
par(mfrow=c(1, 1), omi = c(0.2, 0.2, 0, 0))
par(mar = c(2.5, 14, 1.5, 1.5)) # this sets the margins (bottom, left, top, right)
plotMANNKENDALL(MKresults.salmon, labels_salmon, axiscolors_salmon, "Salmo salar")
dev.off()

svg(file="MK trout.svg")
par(mfrow=c(1, 1), omi = c(0.2, 0.2, 0, 0))
par(mar = c(2.5, 14, 1.5, 1.5)) # this sets the margins (bottom, left, top, right)
plotMANNKENDALL(MKresults.trout, labels_resp, axiscolors_resp, "Salmo trutta fario")
dev.off()


###### Mann-Kendall sites


df <- read_excel("data from captures_restricted.xlsx")
head(df)

df1 <- df %>% filter(species=="Salmo salar")
df2 <- df %>% filter(species=="Salmo trutta fario")


xy.list <- split(df2$ind, df2$siteID) #132 time series 
xy.list <- xy.list[lengths(xy.list) >= 3]  # 110 time series
length(xy.list)

MK <-as.data.frame(do.call(rbind,lapply(xy.list[1:72],function(x)unlist(My.mmkh(x))))) #96 are time series,
head(MK)
setDT(MK, keep.rownames = TRUE)[]
colnames(MK)[1] = "siteID"
colnames(MK)[6] = "P_value"
colnames(MK)[11] = "S_statistic2"

Mk1<- MK %>% mutate(Species="Salmo Salar")

c<- right_join(Mk1, MK, by ="siteID")
c <- c[,c(1,11,23)]

write.xlsx(c,"Plot.xlsx")














