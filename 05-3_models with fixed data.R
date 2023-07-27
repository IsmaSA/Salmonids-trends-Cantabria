##### 01. SETUP #########
#package  installation
pckg <- installed.packages()
req.pkg <- c("mgcv", "visreg", "ggplot2", "dplyr", "readxl", "openxlsx")

inst.pkg <- req.pkg[!(req.pkg %in% pckg[,1])]
install.packages(inst.pkg)
lapply(req.pkg, library, character.only=T)
rm(inst.pkg,req.pkg, pckg)

Sys.setenv(LANG = "en")
Sys.setlocale("LC_TIME", "C")

##### 02. MODEL FITTING INDIVIDUALS #####
fish$river <- as.factor(fish$river)
#fish$species <- as.factor(fish$species)
fish$effort <- as.factor(fish$effort)

fish<- read_excel("data from captures_restricted.xlsx")
head(fish)
colnames(fish)
str(fish)

#He cambiado  . --> _

### 2.1 Salmon ####
ind_0.s = gam(ind.0 ~ s(year) +
                s(WGSn) + 
                s(WGSe) + 
                s(elevation) +
                s(NAO) +
                s(ratio.0) +
                s(Temp)+
                s(Prec)+
                s(river, bs="re") +
                s(effort, bs="re"), 
              data = fish[fish$species=="Salmo salar",],
              family=nb(link=log),
              method="ML",
              select=TRUE)
op <- par(mfrow=c(2,2), mar=c(4,4,1,1))
gam.check(ind_0.s)
par(op)
summary.gam(ind_0.s)
op <- par(mfrow=c(3,3), mar=c(4,4,1,1))
plot(ind_0.s, all.terms = TRUE, rug=TRUE, shade= TRUE, shade.col = "lightblue", seWithMean = TRUE, residuals=TRUE)
par(op)

ind_1.s = gam(ind.1 ~ s(year) +
                s(WGSn) + 
                s(WGSe) + 
                s(elevation) +
                s(NAO) +
                s(ratio.1) +
                s(Temp)+
                s(Prec)+
                s(river,bs="re") +
                s(effort, bs="re"), 
              data = fish[fish$species=="Salmo salar",],
              family=nb(link=log),
              method="ML",
              select=TRUE)

op <- par(mfrow=c(2,2), mar=c(4,4,1,1))
gam.check(ind_1.s)
par(op)
summary.gam(ind_1.s)
op <- par(mfrow=c(3,3), mar=c(4,4,1,1))
plot(ind_1.s, all.terms = TRUE, rug=TRUE, shade= TRUE, shade.col = "lightblue", seWithMean = TRUE, residuals=TRUE)
par(op)

### 2.2 Trout ####
ind_0.t = gam(ind.0 ~ s(year) +
                s(WGSn) + 
                s(WGSe) + 
                s(elevation) +
                s(NAO) +
                s(ratio.0, k=7) +
                s(Temp)+
                s(Prec)+
                s(river, bs="re") +
                s(effort, bs="re"), 
              data = fish[fish$species=="Salmo trutta fario",],
              family=nb(link=log),
              method="ML",
              select=TRUE)
op <- par(mfrow=c(2,2), mar=c(4,4,1,1))
gam.check(ind_0.t)
par(op)
summary.gam(ind_0.t)
op <- par(mfrow=c(3,3), mar=c(4,4,1,1))
plot(ind_0.t, all.terms = TRUE, rug=TRUE, shade= TRUE, shade.col = "lightblue", seWithMean = TRUE, residuals=TRUE)
par(op)

ind_1.t = gam(ind.1 ~ s(year) +
                s(WGSn) + 
                s(WGSe) + 
                s(elevation) +
                s(NAO) +
                s(ratio.1) +
                s(Temp)+
                s(Prec)+
                s(river,bs="re") +
                s(effort, bs="re"), 
              data = fish[fish$species=="Salmo trutta fario",],
              family=nb(link=log),
              method="ML",
              select=TRUE)

op <- par(mfrow=c(2,2), mar=c(4,4,1,1))
gam.check(ind_1.t)
par(op)
summary.gam(ind_1.t)
op <- par(mfrow=c(3,3), mar=c(4,4,1,1))
plot(ind_1.t, all.terms = TRUE, rug=TRUE, shade= TRUE, shade.col = "lightblue", seWithMean = TRUE, residuals=TRUE)
par(op)

##### 03. MODEL FITTING BIOMASS #####
### 3.1 Salmon ####
bm_0.s = gam(bm.0 ~ s(year) +
               s(WGSn) + 
               s(WGSe) + 
               s(elevation) +
               s(NAO) +
               s(ratio.0) +
               s(Temp)+
               s(Prec)+
               s(river, bs="re") +
               s(effort, bs="re"), 
             data = fish[fish$species=="Salmo salar",],
             family=Gamma(link="log"),
             method="ML",
             select=TRUE)
op <- par(mfrow=c(2,2), mar=c(4,4,1,1))
gam.check(bm_0.s)
par(op)
summary.gam(bm_0.s)
op <- par(mfrow=c(3,3), mar=c(4,4,1,1))
plot(bm_0.s, all.terms = TRUE, rug=TRUE, shade= TRUE, shade.col = "lightblue", seWithMean = TRUE, residuals=TRUE)
par(op)

bm_1.s = gam(bm.1 ~ s(year) +
               s(WGSn) + 
               s(WGSe) + 
               s(elevation) +
               s(NAO) +
               s(ratio.1) +
               s(Temp)+
               s(Prec)+
               s(river,bs="re") +
               s(effort, bs="re"), 
             data = fish[fish$species=="Salmo salar",],
             family=Gamma(link="log"),
             method="ML",
             select=TRUE)

op <- par(mfrow=c(2,2), mar=c(4,4,1,1))
gam.check(bm_1.s)
par(op)
summary.gam(bm_1.s)
op <- par(mfrow=c(3,3), mar=c(4,4,1,1))
plot(bm_1.s, all.terms = TRUE, rug=TRUE, shade= TRUE, shade.col = "lightblue", seWithMean = TRUE, residuals=TRUE)
par(op)

### 3.2 Trout ####
bm_0.t = gam(bm.0 ~ s(year) +
               s(WGSn) + 
               s(WGSe) + 
               s(elevation) +
               s(NAO) +
               s(ratio.0, k=7) +
               s(Temp)+
               s(Prec)+
               s(river, bs="re") +
               s(effort, bs="re"), 
             data = fish[fish$species=="Salmo trutta fario",],
             family=Gamma(link="log"),
             method="ML",
             select=TRUE)
op <- par(mfrow=c(2,2), mar=c(4,4,1,1))
gam.check(bm_0.t)
par(op)
summary.gam(bm_0.t)
op <- par(mfrow=c(3,3), mar=c(4,4,1,1))
plot(bm_0.t, all.terms = TRUE, rug=TRUE, shade= TRUE, shade.col = "lightblue", seWithMean = TRUE, residuals=TRUE)
par(op)

bm_1.t = gam(bm.1 ~ s(year) +
               s(WGSn) + 
               s(WGSe) + 
               s(elevation) +
               s(NAO) +
               s(ratio.1) +
               s(Temp)+
               s(Prec)+
               s(river,bs="re") +
               s(effort, bs="re"), 
             data = fish[fish$species=="Salmo trutta fario",],
             family=Gamma(link="log"),
             method="ML",
             select=TRUE)

op <- par(mfrow=c(2,2), mar=c(4,4,1,1))
gam.check(bm_1.t)
par(op)
summary.gam(bm_1.t)
op <- par(mfrow=c(3,3), mar=c(4,4,1,1))
plot(bm_1.t, all.terms = TRUE, rug=TRUE, shade= TRUE, shade.col = "lightblue", seWithMean = TRUE, residuals=TRUE)
par(op)

##### 04. MODEL FITTING DENSITY #####
### 4.1 Salmon ####
dens_0.s = gam(dens.0 ~ s(year) +
                 s(WGSn) + 
                 s(WGSe) + 
                 s(elevation) +
                 s(NAO) +
                 s(ratio.0) +
                 s(Temp)+
                 s(Prec)+
                 s(river, bs="re") +
                 s(effort, bs="re"), 
               data = fish[fish$species=="Salmo salar",],
               family=Gamma(link="log"),
               method="ML",
               select=TRUE)
op <- par(mfrow=c(2,2), mar=c(4,4,1,1))
gam.check(dens_0.s)
par(op)
summary.gam(dens_0.s)
op <- par(mfrow=c(3,3), mar=c(4,4,1,1))
plot(dens_0.s, all.terms = TRUE, rug=TRUE, shade= TRUE, shade.col = "lightblue", seWithMean = TRUE, residuals=TRUE)
par(op)

dens_1.s = gam(dens.1 ~ s(year) +
                 s(WGSn) + 
                 s(WGSe) + 
                 s(elevation) +
                 s(NAO) +
                 s(ratio.1) +
                 s(Temp)+
                 s(Prec)+
                 s(river,bs="re") +
                 s(effort, bs="re"), 
               data = fish[fish$species=="Salmo salar",],
               family=Gamma(link="log"),
               method="ML",
               select=TRUE)

op <- par(mfrow=c(2,2), mar=c(4,4,1,1))
gam.check(dens_1.s)
par(op)
summary.gam(dens_1.s)
op <- par(mfrow=c(3,3), mar=c(4,4,1,1))
plot(dens_1.s, all.terms = TRUE, rug=TRUE, shade= TRUE, shade.col = "lightblue", seWithMean = TRUE, residuals=TRUE)
par(op)

### 4.2 Trout ####
dens_0.t = gam(dens.0 ~ s(year) +
                 s(WGSn) + 
                 s(WGSe) + 
                 s(elevation) +
                 s(NAO) +
                 s(ratio.0, k=7) +
                 s(Temp)+
                 s(Prec)+
                 s(river, bs="re") +
                 s(effort, bs="re"), 
               data = fish[fish$species=="Salmo trutta fario",],
               family=Gamma(link="log"),
               method="ML",
               select=TRUE)
op <- par(mfrow=c(2,2), mar=c(4,4,1,1))
gam.check(dens_0.t)
par(op)
summary.gam(dens_0.t)
op <- par(mfrow=c(3,3), mar=c(4,4,1,1))
plot(dens_0.t, all.terms = TRUE, rug=TRUE, shade= TRUE, shade.col = "lightblue", seWithMean = TRUE, residuals=TRUE)
par(op)

dens_1.t = gam(dens.1 ~ s(year) +
                 s(WGSn) + 
                 s(WGSe) + 
                 s(elevation) +
                 s(NAO) +
                 s(ratio.1) +
                 s(Temp)+
                 s(Prec)+
                 s(river,bs="re") +
                 s(effort, bs="re"), 
               data = fish[fish$species=="Salmo trutta fario",],
               family=Gamma(link="log"),
               method="ML",
               select=TRUE)

op <- par(mfrow=c(2,2), mar=c(4,4,1,1))
gam.check(dens_1.t)
par(op)
summary.gam(dens_1.t)
op <- par(mfrow=c(3,3), mar=c(4,4,1,1))
plot(dens_1.t, all.terms = TRUE, rug=TRUE, shade= TRUE, shade.col = "lightblue", seWithMean = TRUE, residuals=TRUE)
par(op)

##### 05. PLOTS #####
#### 5.1 functions ####
createDATA <- function(model0, model1, explanatory){
  T1 <- visreg(model0, scale="response", explanatory, line.par = list(col = "black"), plot=FALSE)
  T2 <- visreg(model1, scale="response", explanatory, line.par = list(col = "blue"), plot=FALSE)
  
  output <- bind_rows(mutate(T1$fit, plt="Age 0+"),
                      mutate(T2$fit, plt="Age 1+"))
  return(output)
}

createDATAstocking <- function(model0, model1){
  T1 <- visreg(model0, scale='response', "ratio.0", line.par = list(col = 'black'), plot=FALSE)
  T2 <- visreg(model1, scale='response', "ratio.1", line.par = list(col = 'blue'), plot=FALSE)
  colnames(T1$fit)[colnames(T1$fit)=="ratio.0"] <- "ratio"
  colnames(T2$fit)[colnames(T2$fit)=="ratio.1"] <- "ratio"
  
  output <- bind_rows(mutate(T1$fit, plt="Age 0+"),
                      mutate(T2$fit, plt="Age 1+"))
  return(output)
}

plotGAMyear <- function(data, ylab, color_lab, fill_lab, title=NULL){
  ribbon <- ggplot(data = data, aes(year, visregFit, ymin=visregLwr, ymax=visregUpr, group=plt, fill=plt))
  ribbon +
    geom_ribbon(alpha=0.5) +
    geom_line(aes(color=plt), size=1.2) +
    scale_size_manual(values = c(1,1,1))+
    xlab("Year") +
    ylab(ylab) +
    labs(color=color_lab, fill=fill_lab, title=title) +
    theme_bw()
}

plotGAMnorthing <- function(data, ylab, color_lab, fill_lab, title=NULL){
  ribbon <- ggplot(data = data, aes(WGSn, visregFit, ymin=visregLwr, ymax=visregUpr, group=plt, fill=plt))
  ribbon +
    geom_ribbon(alpha=0.5) +
    geom_line(aes(color=plt), size=1.2) +
    scale_size_manual(values = c(1,1,1))+
    xlab("WGS84 Northing") +
    ylab(ylab) +
    labs(color=color_lab, fill=fill_lab, title=title) +
    theme_bw()
}

plotGAMeasting <- function(data, ylab, color_lab, fill_lab, title=NULL){
  ribbon <- ggplot(data = data, aes(WGSe, visregFit, ymin=visregLwr, ymax=visregUpr, group=plt, fill=plt))
  ribbon +
    geom_ribbon(alpha=0.5) +
    geom_line(aes(color=plt), size=1.2) +
    scale_size_manual(values = c(1,1,1))+
    xlab("WGS84 Easting") +
    ylab(ylab) +
    labs(color=color_lab, fill=fill_lab, title=title) +
    theme_bw()
}

plotGAMelev <- function(data, ylab, color_lab, fill_lab, title=NULL){
  ribbon <- ggplot(data = data, aes(elevation, visregFit, ymin=visregLwr, ymax=visregUpr, group=plt, fill=plt))
  ribbon +
    geom_ribbon(alpha=0.5) +
    geom_line(aes(color=plt), size=1.2) +
    scale_size_manual(values = c(1,1,1))+
    xlab("Elevation [masl]") +
    ylab(ylab) +
    labs(color=color_lab, fill=fill_lab, title=title) +
    theme_bw()
}

plotGAMnao <- function(data, ylab, color_lab, fill_lab, title=NULL){
  ribbon <- ggplot(data = data, aes(NAO, visregFit, ymin=visregLwr, ymax=visregUpr, group=plt, fill=plt))
  ribbon +
    geom_ribbon(alpha=0.5) +
    geom_line(aes(color=plt), size=1.2) +
    scale_size_manual(values = c(1,1,1))+
    xlab("NAO Index") +
    ylab(ylab) +
    labs(color=color_lab, fill=fill_lab, title=title) +
    theme_bw()
}

plotGAMstocking <- function(data, ylab, color_lab, fill_lab, title=NULL){
  ribbon <- ggplot(data = data, aes(ratio, visregFit, ymin=visregLwr, ymax=visregUpr, group=plt, fill=plt))
  ribbon +
    geom_ribbon(alpha=0.5) +
    geom_line(aes(color=plt), size=1.2) +
    scale_size_manual(values = c(1,1,1))+
    xlab("Stocking Ratio") +
    ylab(ylab) +
    labs(color=color_lab, fill=fill_lab, title=title) +
    theme_bw()
}


#### 5.2 individuals Salmon ####
fits_year <- createDATA(ind_0.s, ind_1.s, "year")
fits_WGSn <- createDATA(ind_0.s, ind_1.s, "WGSn")
fits_WGSe <- createDATA(ind_0.s, ind_1.s, "WGSe")
fits_elev <- createDATA(ind_0.s, ind_1.s, "elevation")
fits_NAO <- createDATA(ind_0.s, ind_1.s, "NAO")
fits_stocking <- createDATAstocking(ind_0.s, ind_1.s)

svg("plot gam individuals year - salmon.svg", height=10, width=10)
plotGAMyear(fits_year, "Individuals", "Age Class", "", "Salmon")
dev.off()

svg("plot gam individuals WGSn - salmon.svg", height=10, width=10)
plotGAMnorthing(fits_WGSn, "Individuals", "Age Class", "", "Salmon")
dev.off()

svg("plot gam individuals WGSe - salmon.svg", height=10, width=10)
plotGAMeasting(fits_WGSe, "Individuals", "Age Class", "", "Salmon")
dev.off()

svg("plot gam individuals elevation - salmon.svg", height=10, width=10)
plotGAMelev(fits_elev, "Individuals", "Age Class", "", "Salmon")
dev.off()

svg("plot gam individuals NAO - salmon.svg", height=10, width=10)
plotGAMnao(fits_NAO, "Individuals", "Age Class", "", "Salmon")
dev.off()

svg("plot gam individuals stocking ratio - salmon.svg", height=10, width=10)
plotGAMstocking(fits_stocking, "Individuals", "Age Class", "", "Salmon")
dev.off()

#### 5.6 biomass Salmon ####
fits_year <- createDATA(bm_0.s, bm_1.s, "year")
fits_WGSn <- createDATA(bm_0.s, bm_1.s, "WGSn")
fits_WGSe <- createDATA(bm_0.s, bm_1.s, "WGSe")
fits_elev <- createDATA(bm_0.s, bm_1.s, "elevation")
fits_NAO <- createDATA(bm_0.s, bm_1.s, "NAO")
fits_stocking <- createDATAstocking(bm_0.s, bm_1.s)

svg("plot gam bm year - salmon.svg", height=10, width=10)
plotGAMyear(fits_year, as.expression(bquote("Biomass [kg/" ~ m^2 ~ "]")), "Age Class", "", "Salmon")
dev.off()

svg("plot gam bm WGSn - salmon.svg", height=10, width=10)
plotGAMnorthing(fits_WGSn, as.expression(bquote("Biomass [kg/" ~ m^2 ~ "]")), "Age Class", "", "Salmon")
dev.off()

svg("plot gam bm WGSe - salmon.svg", height=10, width=10)
plotGAMeasting(fits_WGSe, as.expression(bquote("Biomass [kg/" ~ m^2 ~ "]")), "Age Class", "", "Salmon")
dev.off()

svg("plot gam bm elevation - salmon.svg", height=10, width=10)
plotGAMelev(fits_elev, as.expression(bquote("Biomass [kg/" ~ m^2 ~ "]")), "Age Class", "", "Salmon")
dev.off()

svg("plot gam bm NAO - salmon.svg", height=10, width=10)
plotGAMnao(fits_NAO, as.expression(bquote("Biomass [kg/" ~ m^2 ~ "]")), "Age Class", "", "Salmon")
dev.off()

svg("plot gam bm stocking ratio - salmon.svg", height=10, width=10)
plotGAMstocking(fits_stocking, as.expression(bquote("Biomass [kg/" ~ m^2 ~ "]")), "Age Class", "", "Salmon")
dev.off()

#### 5.7. density Salmon ####
fits_year <- createDATA(dens_0.s, dens_1.s, "year")
fits_WGSn <- createDATA(dens_0.s, dens_1.s, "WGSn")
fits_WGSe <- createDATA(dens_0.s, dens_1.s, "WGSe")
fits_elev <- createDATA(dens_0.s, dens_1.s, "elevation")
fits_NAO <- createDATA(dens_0.s, dens_1.s, "NAO")
fits_stocking <- createDATAstocking(dens_0.s, dens_1.s)

svg("plot gam density year - salmon.svg", height=10, width=10)
plotGAMyear(fits_year, as.expression(bquote("Density [ind/" ~ m^2 ~ "]")), "Age Class", "", "Salmon")
dev.off()

svg("plot gam density WGSn - salmon.svg", height=10, width=10)
plotGAMnorthing(fits_WGSn, as.expression(bquote("Density [ind/" ~ m^2 ~ "]")), "Age Class", "", "Salmon")
dev.off()

svg("plot gam density WGSe - salmon.svg", height=10, width=10)
plotGAMeasting(fits_WGSe, as.expression(bquote("Density [ind/" ~ m^2 ~ "]")), "Age Class", "", "Salmon")
dev.off()

svg("plot gam density elevation - salmon.svg", height=10, width=10)
plotGAMelev(fits_elev, as.expression(bquote("Density [ind/" ~ m^2 ~ "]")), "Age Class", "", "Salmon")
dev.off()

svg("plot gam density NAO - salmon.svg", height=10, width=10)
plotGAMnao(fits_NAO, as.expression(bquote("Density [ind/" ~ m^2 ~ "]")), "Age Class", "", "Salmon")
dev.off()

svg("plot gam density stocking ratio - salmon.svg", height=10, width=10)
plotGAMstocking(fits_stocking, as.expression(bquote("Density [ind/" ~ m^2 ~ "]")), "Age Class", "", "Salmon")
dev.off()

#### 5.8. individuals trout ####
fits_year <- createDATA(ind_0.t, ind_1.t, "year")
fits_WGSn <- createDATA(ind_0.t, ind_1.t, "WGSn")
fits_WGSe <- createDATA(ind_0.t, ind_1.t, "WGSe")
fits_elev <- createDATA(ind_0.t, ind_1.t, "elevation")
fits_NAO <- createDATA(ind_0.t, ind_1.t, "NAO")
fits_stocking <- createDATAstocking(ind_0.t, ind_1.t)

svg("plot gam individuals year - trout.svg", height=10, width=10)
plotGAMyear(fits_year, "Individuals", "Age Class", "", "Trout")
dev.off()

svg("plot gam individuals WGSn - trout.svg", height=10, width=10)
plotGAMnorthing(fits_WGSn, "Individuals", "Age Class", "", "Trout")
dev.off()

svg("plot gam individuals WGSe - trout.svg", height=10, width=10)
plotGAMeasting(fits_WGSe, "Individuals", "Age Class", "", "Trout")
dev.off()

svg("plot gam individuals elevation - trout.svg", height=10, width=10)
plotGAMelev(fits_elev, "Individuals", "Age Class", "", "Trout")
dev.off()

svg("plot gam individuals NAO - trout.svg", height=10, width=10)
plotGAMnao(fits_NAO, "Individuals", "Age Class", "", "Trout")
dev.off()

svg("plot gam individuals stocking ratio - trout.svg", height=10, width=10)
plotGAMstocking(fits_stocking, "Individuals", "Age Class", "", "Trout")
dev.off()

#### 5.9 biomass trout ####
fits_year <- createDATA(bm_0.t, bm_1.t, "year")
fits_WGSn <- createDATA(bm_0.t, bm_1.t, "WGSn")
fits_WGSe <- createDATA(bm_0.t, bm_1.t, "WGSe")
fits_elev <- createDATA(bm_0.t, bm_1.t, "elevation")
fits_NAO <- createDATA(bm_0.t, bm_1.t, "NAO")
fits_stocking <- createDATAstocking(bm_0.t, bm_1.t)

svg("plot gam bm year - trout.svg", height=10, width=10)
plotGAMyear(fits_year, as.expression(bquote("Biomass [kg/" ~ m^2 ~ "]")), "Age Class", "", "Trout")
dev.off()

svg("plot gam bm WGSn - trout.svg", height=10, width=10)
plotGAMnorthing(fits_WGSn, as.expression(bquote("Biomass [kg/" ~ m^2 ~ "]")), "Age Class", "", "Trout")
dev.off()

svg("plot gam bm WGSe - trout.svg", height=10, width=10)
plotGAMeasting(fits_WGSe, as.expression(bquote("Biomass [kg/" ~ m^2 ~ "]")), "Age Class", "", "Trout")
dev.off()

svg("plot gam bm elevation - trout.svg", height=10, width=10)
plotGAMelev(fits_elev, as.expression(bquote("Biomass [kg/" ~ m^2 ~ "]")), "Age Class", "", "Trout")
dev.off()

svg("plot gam bm NAO - trout.svg", height=10, width=10)
plotGAMnao(fits_NAO, as.expression(bquote("Biomass [kg/" ~ m^2 ~ "]")), "Age Class", "", "Trout")
dev.off()

svg("plot gam bm stocking ratio - trout.svg", height=10, width=10)
plotGAMstocking(fits_stocking, as.expression(bquote("Biomass [kg/" ~ m^2 ~ "]")), "Age Class", "", "Trout")
dev.off()

#### 5.10. density trout ####
fits_year <- createDATA(dens_0.t, dens_1.t, "year")
fits_WGSn <- createDATA(dens_0.t, dens_1.t, "WGSn")
fits_WGSe <- createDATA(dens_0.t, dens_1.t, "WGSe")
fits_elev <- createDATA(dens_0.t, dens_1.t, "elevation")
fits_NAO <- createDATA(dens_0.t, dens_1.t, "NAO")
fits_stocking <- createDATAstocking(dens_0.t, dens_1.t)

svg("plot gam density year - trout.svg", height=10, width=10)
plotGAMyear(fits_year, as.expression(bquote("Density [ind/" ~ m^2 ~ "]")), "Age Class", "", "Trout")
dev.off()

svg("plot gam density WGSn - trout.svg", height=10, width=10)
plotGAMnorthing(fits_WGSn, as.expression(bquote("Density [ind/" ~ m^2 ~ "]")), "Age Class", "", "Trout")
dev.off()

svg("plot gam density WGSe - trout.svg", height=10, width=10)
plotGAMeasting(fits_WGSe, as.expression(bquote("Density [ind/" ~ m^2 ~ "]")), "Age Class", "", "Trout")
dev.off()

svg("plot gam density elevation - trout.svg", height=10, width=10)
plotGAMelev(fits_elev, as.expression(bquote("Density [ind/" ~ m^2 ~ "]")), "Age Class", "", "Trout")
dev.off()

svg("plot gam density NAO - trout.svg", height=10, width=10)
plotGAMnao(fits_NAO, as.expression(bquote("Density [ind/" ~ m^2 ~ "]")), "Age Class", "", "Trout")
dev.off()

svg("plot gam density stocking ratio - trout.svg", height=10, width=10)
plotGAMstocking(fits_stocking, as.expression(bquote("Density [ind/" ~ m^2 ~ "]")), "Age Class", "", "Trout")
dev.off()

##### 06. SUMMARY #####
#### 6.1 individuals ####
results_ind0.s <- summary(ind_0.s)
results_ind1.s <- summary(ind_1.s)

results_ind0.t <- summary(ind_0.t)
results_ind1.t <- summary(ind_1.t)

#### 6.2 biomass ####
results_bm0.s <- summary(bm_0.s)
results_bm1.s <- summary(bm_1.s)

results_bm0.t <- summary(bm_0.t)
results_bm1.t <- summary(bm_1.t)

#### 6.3 density ####
results_dens0.s <- summary(dens_0.s)
results_dens1.s <- summary(dens_1.s)

results_dens0.t <- summary(dens_0.t)
results_dens1.t <- summary(dens_1.t)

#### 6.4 combine results ####
results <- read_excel("results_gam_fixed data.xlsx", col_types = c(rep("text", 3), rep("numeric",32)))

### 6.4.1 year ####
## individuals
results[1,4:7] <- as.list(results_ind0.s$s.table[1,])
results[2,4:7] <- as.list(results_ind1.s$s.table[1,])

results[3,4:7] <- as.list(results_ind0.t$s.table[1,])
results[4,4:7] <- as.list(results_ind1.t$s.table[1,])

## biomass
results[5,4:7] <- as.list(results_bm0.s$s.table[1,])
results[6,4:7] <- as.list(results_bm1.s$s.table[1,])

results[7,4:7] <- as.list(results_bm0.t$s.table[1,])
results[8,4:7] <- as.list(results_bm1.t$s.table[1,])

## density
results[9,4:7] <- as.list(results_dens0.s$s.table[1,])
results[10,4:7] <- as.list(results_dens1.s$s.table[1,])

results[11,4:7] <- as.list(results_dens0.t$s.table[1,])
results[12,4:7] <- as.list(results_dens1.t$s.table[1,])

### 6.4.2 northing ####
## individuals
results[1,8:11] <- as.list(results_ind0.s$s.table[2,])
results[2,8:11] <- as.list(results_ind1.s$s.table[2,])

results[3,8:11] <- as.list(results_ind0.t$s.table[2,])
results[4,8:11] <- as.list(results_ind1.t$s.table[2,])

## biomass
results[5,8:11] <- as.list(results_bm0.s$s.table[2,])
results[6,8:11] <- as.list(results_bm1.s$s.table[2,])

results[7,8:11] <- as.list(results_bm0.t$s.table[2,])
results[8,8:11] <- as.list(results_bm1.t$s.table[2,])

## density
results[9,8:11] <- as.list(results_dens0.s$s.table[2,])
results[10,8:11] <- as.list(results_dens1.s$s.table[2,])

results[11,8:11] <- as.list(results_dens0.t$s.table[2,])
results[12,8:11] <- as.list(results_dens1.t$s.table[2,])

### 6.4.3 easting ####
## individuals
results[1,12:15] <- as.list(results_ind0.s$s.table[3,])
results[2,12:15] <- as.list(results_ind1.s$s.table[3,])

results[3,12:15] <- as.list(results_ind0.t$s.table[3,])
results[4,12:15] <- as.list(results_ind1.t$s.table[3,])

## biomass
results[5,12:15] <- as.list(results_bm0.s$s.table[3,])
results[6,12:15] <- as.list(results_bm1.s$s.table[3,])

results[7,12:15] <- as.list(results_bm0.t$s.table[3,])
results[8,12:15] <- as.list(results_bm1.t$s.table[3,])

## density
results[9,12:15] <- as.list(results_dens0.s$s.table[3,])
results[10,12:15] <- as.list(results_dens1.s$s.table[3,])

results[11,12:15] <- as.list(results_dens0.t$s.table[3,])
results[12,12:15] <- as.list(results_dens1.t$s.table[3,])

### 6.4.4 elevation ####
## individuals
results[1,16:19] <- as.list(results_ind0.s$s.table[4,])
results[2,16:19] <- as.list(results_ind1.s$s.table[4,])

results[3,16:19] <- as.list(results_ind0.t$s.table[4,])
results[4,16:19] <- as.list(results_ind1.t$s.table[4,])

## biomass
results[5,16:19] <- as.list(results_bm0.s$s.table[4,])
results[6,16:19] <- as.list(results_bm1.s$s.table[4,])

results[7,16:19] <- as.list(results_bm0.t$s.table[4,])
results[8,16:19] <- as.list(results_bm1.t$s.table[4,])

## density
results[9,16:19] <- as.list(results_dens0.s$s.table[4,])
results[10,16:19] <- as.list(results_dens1.s$s.table[4,])

results[11,16:19] <- as.list(results_dens0.t$s.table[4,])
results[12,16:19] <- as.list(results_dens1.t$s.table[4,])

### 6.4.5 NAO ####
## individuals
results[1,20:23] <- as.list(results_ind0.s$s.table[5,])
results[2,20:23] <- as.list(results_ind1.s$s.table[5,])

results[3,20:23] <- as.list(results_ind0.t$s.table[5,])
results[4,20:23] <- as.list(results_ind1.t$s.table[5,])

## biomass
results[5,20:23] <- as.list(results_bm0.s$s.table[5,])
results[6,20:23] <- as.list(results_bm1.s$s.table[5,])

results[7,20:23] <- as.list(results_bm0.t$s.table[5,])
results[8,20:23] <- as.list(results_bm1.t$s.table[5,])

## density
results[9,20:23] <- as.list(results_dens0.s$s.table[5,])
results[10,20:23] <- as.list(results_dens1.s$s.table[5,])

results[11,20:23] <- as.list(results_dens0.t$s.table[5,])
results[12,20:23] <- as.list(results_dens1.t$s.table[5,])


### 6.4.6 stocking ratio ####
## individuals
results[1,24:27] <- as.list(results_ind0.s$s.table[6,])
results[2,24:27] <- as.list(results_ind1.s$s.table[6,])

results[3,24:27] <- as.list(results_ind0.t$s.table[6,])
results[4,24:27] <- as.list(results_ind1.t$s.table[6,])

## biomass
results[5,24:27] <- as.list(results_bm0.s$s.table[6,])
results[6,24:27] <- as.list(results_bm1.s$s.table[6,])

results[7,24:27] <- as.list(results_bm0.t$s.table[6,])
results[8,24:27] <- as.list(results_bm1.t$s.table[6,])

## density
results[9,24:27] <- as.list(results_dens0.s$s.table[6,])
results[10,24:27] <- as.list(results_dens1.s$s.table[6,])

results[11,24:27] <- as.list(results_dens0.t$s.table[6,])
results[12,24:27] <- as.list(results_dens1.t$s.table[6,])

### 6.4.7 river ####
## individuals
results[1,28:31] <- as.list(results_ind0.s$s.table[7,])
results[2,28:31] <- as.list(results_ind1.s$s.table[7,])

results[3,28:31] <- as.list(results_ind0.t$s.table[7,])
results[4,28:31] <- as.list(results_ind1.t$s.table[7,])

## biomass
results[5,28:31] <- as.list(results_bm0.s$s.table[7,])
results[6,28:31] <- as.list(results_bm1.s$s.table[7,])

results[7,28:31] <- as.list(results_bm0.t$s.table[7,])
results[8,28:31] <- as.list(results_bm1.t$s.table[7,])

## density
results[9,28:31] <- as.list(results_dens0.s$s.table[7,])
results[10,28:31] <- as.list(results_dens1.s$s.table[7,])

results[11,28:31] <- as.list(results_dens0.t$s.table[7,])
results[12,28:31] <- as.list(results_dens1.t$s.table[7,])

### 6.4.8 sampling ####
## individuals
results[1,32:35] <- as.list(results_ind0.s$s.table[8,])
results[2,32:35] <- as.list(results_ind1.s$s.table[8,])

results[3,32:35] <- as.list(results_ind0.t$s.table[8,])
results[4,32:35] <- as.list(results_ind1.t$s.table[8,])

## biomass
results[5,32:35] <- as.list(results_bm0.s$s.table[8,])
results[6,32:35] <- as.list(results_bm1.s$s.table[8,])

results[7,32:35] <- as.list(results_bm0.t$s.table[8,])
results[8,32:35] <- as.list(results_bm1.t$s.table[8,])

## density
results[9,32:35] <- as.list(results_dens0.s$s.table[8,])
results[10,32:35] <- as.list(results_dens1.s$s.table[8,])

results[11,32:35] <- as.list(results_dens0.t$s.table[8,])
results[12,32:35] <- as.list(results_dens1.t$s.table[8,])

#### 6.5 round results ####
results[,c(4:6,8:10,12:14,16:18,20:22,24:26,28:30,32:34)] <- round(results[,c(4:6,8:10,12:14,16:18,20:22,24:26,28:30,32:34)], 4)
write.xlsx(results, "results_gam_filled.xlsx")

#### 6.6 change p-values ####
final_results <- results
final_results[,4:35] <- round(final_results[,4:35],2)

final_results$year_p[final_results$year_p<0.001] <- "< 0.001"
final_results$year_p[final_results$year_p<0.01 & final_results$year_p>=0.001] <- "< 0.01"
final_results$year_p[final_results$year_p<0.05 & final_results$year_p>=0.01] <- "< 0.05"

final_results$northing_p[final_results$northing_p<0.001] <- "< 0.001"
final_results$northing_p[final_results$northing_p<0.01 & final_results$northing_p>=0.001] <- "< 0.01"
final_results$northing_p[final_results$northing_p<0.05 & final_results$northing_p>=0.01] <- "< 0.05"

final_results$easting_p[final_results$easting_p<0.001] <- "< 0.001"
final_results$easting_p[final_results$easting_p<0.01 & final_results$easting_p>=0.001] <- "< 0.01"
final_results$easting_p[final_results$easting_p<0.05 & final_results$easting_p>=0.01] <- "< 0.05"

final_results$elevation_p[final_results$elevation_p<0.001] <- "< 0.001"
final_results$elevation_p[final_results$elevation_p<0.01 & final_results$elevation_p>=0.001] <- "< 0.01"
final_results$elevation_p[final_results$elevation_p<0.05 & final_results$elevation_p>=0.01] <- "< 0.05"

final_results$NAO_p[final_results$NAO_p<0.001] <- "< 0.001"
final_results$NAO_p[final_results$NAO_p<0.01 & final_results$NAO_p>=0.001] <- "< 0.01"
final_results$NAO_p[final_results$NAO_p<0.05 & final_results$NAO_p>=0.01] <- "< 0.05"

final_results$stocking_p[final_results$stocking_p<0.001] <- "< 0.001"
final_results$stocking_p[final_results$stocking_p<0.01 & final_results$stocking_p>=0.001] <- "< 0.01"
final_results$stocking_p[final_results$stocking_p<0.05 & final_results$stocking_p>=0.01] <- "< 0.05"

final_results$river_p[final_results$river_p<0.001] <- "< 0.001"
final_results$river_p[final_results$river_p<0.01 & final_results$river_p>=0.001] <- "< 0.01"
final_results$river_p[final_results$river_p<0.05 & final_results$river_p>=0.01] <- "< 0.05"

final_results$effort_p[final_results$effort_p<0.001] <- "< 0.001"
final_results$effort_p[final_results$effort_p<0.01 & final_results$effort_p>=0.001] <- "< 0.01"
final_results$effort_p[final_results$effort_p<0.05 & final_results$effort_p>=0.01] <- "< 0.05"

write.xlsx(final_results, "results gam final.xlsx")


##############Plot model
library(sjPlot)

#Age0: rojo
#Age1: azul

p1<- plot_model(bm_1.s, type = "pred", terms = "year", line.size = 1)
p1+ theme_classic2()+theme_cleveland() + geom_line(aes(color="red")) 

#Salmo salar
#Biomasa
colnames(fish)

p0<- plot_model(dens_0.s, type = "pred", terms = "year", line.size = 1)
p0<- p0+ theme_classic2()+theme_cleveland()  
p0

p1<- plot_model(dens_1.s, type = "pred", terms = "year", line.size = 1)
p1<- p1+ theme_classic2()+theme_cleveland() 
p1


p2<- plot_model(dens_0.t, type = "pred", terms = "year", line.size = 1)
p2<- p2+ theme_classic2()+theme_cleveland()  
p2

p3<- plot_model(dens_1.t, type = "pred", terms = "year", line.size = 1)
p3<- p3+ theme_classic2()+theme_cleveland() 
p3

p4<- plot_model(ind_1.s, type = "pred", terms = "Temp", line.size = 1)
p4<- p4+ theme_classic2()+theme_cleveland()  
p4

p5<- plot_model(ind_1.t, type = "pred", terms = "Temp", line.size = 1)
p5<- p5+ theme_classic2()+theme_cleveland() 
p5



p0+p1+p2+p3+p4+p5
p0+p1+p2+p3

head(fish)
df
df<- read.csv2("Temp_data.csv")
df<- read.csv2("Prec_data.csv")

str(df)
df$Mean_temp<- as.numeric(df$Mean_temp)
df$Mean_prec<- as.numeric(df$Mean_prec)
df$Site<- as.factor(df$Site)
#df<- df %>% group_by(Year) %>% summarise(Temp=mean(Mean_prec, na.rm=T))
str(df)
df1 <- subset(df, df$Year >= 1980 & df$Year <= 2020)
df2<- subset(df, df$Year >= 2010 & df$Year <= 2019)
head(df1)
#a<- gam(Mean_temp~Year+ Site, data = df1)

a<- glm(Mean_prec~Year, data = df2)
summary(a)

plot_model(a, type = "pred", terms = "Year", line.size = 1)



###########################################################################
head(fish)

### 2.1 Salmon ####
ind_0.s = gam(abun ~ s(year) +
                s(WGSn) + 
                s(WGSe) + 
                s(elevation) +
                s(NAO) +
                s(ratio.0) +
                s(Temp)+
                s(Prec)+
                s(river, bs="re") +
                s(effort, bs="re"), 
              data = fish[fish$species=="Salmo salar",],
              family=nb(link=log),
              method="ML",
              select=TRUE)
op <- par(mfrow=c(2,2), mar=c(4,4,1,1))
gam.check(ind_0.s)
par(op)
summary.gam(ind_0.s)
op <- par(mfrow=c(3,3), mar=c(4,4,1,1))
plot(ind_0.s, all.terms = TRUE, rug=TRUE, shade= TRUE, shade.col = "lightblue", seWithMean = TRUE, residuals=TRUE)
par(op)

ind_1.s = gam(ind.1 ~ s(year) +
                s(WGSn) + 
                s(WGSe) + 
                s(elevation) +
                s(NAO) +
                s(ratio.1) +
                s(Temp)+
                s(Prec)+
                s(river,bs="re") +
                s(effort, bs="re"), 
              data = fish[fish$species=="Salmo salar",],
              family=nb(link=log),
              method="ML",
              select=TRUE)

op <- par(mfrow=c(2,2), mar=c(4,4,1,1))
gam.check(ind_1.s)
par(op)
summary.gam(ind_1.s)
op <- par(mfrow=c(3,3), mar=c(4,4,1,1))
plot(ind_1.s, all.terms = TRUE, rug=TRUE, shade= TRUE, shade.col = "lightblue", seWithMean = TRUE, residuals=TRUE)
par(op)

### 2.2 Trout ####
ind_0.t = gam(ind.0 ~ s(year) +
                s(WGSn) + 
                s(WGSe) + 
                s(elevation) +
                s(NAO) +
                s(ratio.0, k=7) +
                s(Temp)+
                s(Prec)+
                s(river, bs="re") +
                s(effort, bs="re"), 
              data = fish[fish$species=="Salmo trutta fario",],
              family=nb(link=log),
              method="ML",
              select=TRUE)
op <- par(mfrow=c(2,2), mar=c(4,4,1,1))
gam.check(ind_0.t)
par(op)
summary.gam(ind_0.t)
op <- par(mfrow=c(3,3), mar=c(4,4,1,1))
plot(ind_0.t, all.terms = TRUE, rug=TRUE, shade= TRUE, shade.col = "lightblue", seWithMean = TRUE, residuals=TRUE)
par(op)

ind_1.t = gam(ind.1 ~ s(year) +
                s(WGSn) + 
                s(WGSe) + 
                s(elevation) +
                s(NAO) +
                s(ratio.1) +
                s(Temp)+
                s(Prec)+
                s(river,bs="re") +
                s(effort, bs="re"), 
              data = fish[fish$species=="Salmo trutta fario",],
              family=nb(link=log),
              method="ML",
              select=TRUE)

op <- par(mfrow=c(2,2), mar=c(4,4,1,1))
gam.check(ind_1.t)
par(op)
summary.gam(ind_1.t)
op <- par(mfrow=c(3,3), mar=c(4,4,1,1))
plot(ind_1.t, all.terms = TRUE, rug=TRUE, shade= TRUE, shade.col = "lightblue", seWithMean = TRUE, residuals=TRUE)
par(op)


###################### Sensitivity analyses #################


fish<- read_excel("data from captures_restricted.xlsx", sheet = "Hoja1")
head(fish)
colnames(fish)
str(fish)
fish$river <- as.factor(fish$river)
fish$effort <- as.factor(fish$effort)
#He cambiado  . --> _

### 2.1 Salmon ####
hist(fish$ind.0)
ind_0.s <- gam(ind.0 ~ s(year) +
                s(WGSn) + 
                s(WGSe) + 
                s(elevation) +
                s(NAO) +
                s(ratio.0) +
                s(Temp)+
                s(Prec)+
                s(river, bs="re") +
                s(effort, bs="re"), 
              data = fish[fish$species=="Salmo salar",],
              family=nb(link=log),
              method="ML",
              select=TRUE)
op <- par(mfrow=c(2,2), mar=c(4,4,1,1))
gam.check(ind_0.s)
par(op)
summary.gam(ind_0.s)
op <- par(mfrow=c(3,3), mar=c(4,4,1,1))
plot(ind_0.s, all.terms = TRUE, rug=TRUE, shade= TRUE, shade.col = "lightblue", seWithMean = TRUE, residuals=TRUE)
par(op)

p0<- plot_model(ind_1.t , type = "pred", terms = "year", line.size = 1)
p0<- p0+ theme_classic2()+theme_cleveland()  
p0


ind_1.s = gam(ind.1 ~ s(year) +
                s(WGSn) + 
                s(WGSe) + 
                s(elevation) +
                s(NAO) +
                s(ratio.1) +
                s(Temp)+
                s(Prec)+
                s(river,bs="re") +
                s(effort, bs="re"), 
              data = fish[fish$species=="Salmo salar",],
              family=nb(link=log),
              method="ML",
              select=TRUE)

op <- par(mfrow=c(2,2), mar=c(4,4,1,1))
gam.check(ind_1.s)
par(op)
summary.gam(ind_1.s)
op <- par(mfrow=c(3,3), mar=c(4,4,1,1))
plot(bm.0 , all.terms = TRUE, rug=TRUE, shade= TRUE, shade.col = "lightblue", seWithMean = TRUE, residuals=TRUE)
par(op)

### 2.2 Trout ####
ind_0.t <- gam(ind.0 ~ s(year) +
                s(WGSn) + 
                s(WGSe) + 
                s(elevation) +
                s(NAO) +
                s(ratio.0, k=7) +
                s(Temp)+
                s(Prec)+
                s(river, bs="re") +
                s(effort, bs="re"), 
              data = fish[fish$species=="Salmo trutta fario",],
              family=nb(link=log),
              method="ML",
              select=TRUE)
op <- par(mfrow=c(2,2), mar=c(4,4,1,1))
gam.check(ind_0.t)
par(op)
summary.gam(ind_0.t)
op <- par(mfrow=c(3,3), mar=c(4,4,1,1))
plot(ind_0.t, all.terms = TRUE, rug=TRUE, shade= TRUE, shade.col = "lightblue", seWithMean = TRUE, residuals=TRUE)
par(op)

ind_1.t = gam(ind.1 ~ s(year) +
                s(WGSn) + 
                s(WGSe) + 
                s(elevation) +
                s(NAO) +
                s(ratio.1) +
                s(Temp)+
                s(Prec)+
                s(river,bs="re") +
                s(effort, bs="re"), 
              data = fish[fish$species=="Salmo trutta fario",],
              family=nb(link=log),
              method="ML",
              select=TRUE)

op <- par(mfrow=c(2,2), mar=c(4,4,1,1))
gam.check(ind_1.t)
par(op)
summary.gam(ind_1.t)
op <- par(mfrow=c(3,3), mar=c(4,4,1,1))
plot(ind_1.t, all.terms = TRUE, rug=TRUE, shade= TRUE, shade.col = "lightblue", seWithMean = TRUE, residuals=TRUE)
par(op)

##### 03. MODEL FITTING BIOMASS #####
### 3.1 Salmon ####
bm_0.s = gam(bm.0 ~ s(year) +
               s(WGSn) + 
               s(WGSe) + 
               s(elevation) +
               s(NAO) +
               s(ratio.0) +
               s(Temp)+
               s(Prec)+
               s(river, bs="re") +
               s(effort, bs="re"), 
             data = fish[fish$species=="Salmo salar",],
             family=Gamma(link="log"),
             method="ML",
             select=TRUE)
op <- par(mfrow=c(2,2), mar=c(4,4,1,1))
gam.check(bm_0.s)
par(op)
summary.gam(bm_0.s)
op <- par(mfrow=c(3,3), mar=c(4,4,1,1))
plot(bm_0.s, all.terms = TRUE, rug=TRUE, shade= TRUE, shade.col = "lightblue", seWithMean = TRUE, residuals=TRUE)
par(op)

bm_1.s = gam(bm.1 ~ s(year) +
               s(WGSn) + 
               s(WGSe) + 
               s(elevation) +
               s(NAO) +
               s(ratio.1) +
               s(Temp)+
               s(Prec)+
               s(river,bs="re") +
               s(effort, bs="re"), 
             data = fish[fish$species=="Salmo salar",],
             family=Gamma(link="log"),
             method="ML",
             select=TRUE)

op <- par(mfrow=c(2,2), mar=c(4,4,1,1))
gam.check(bm_1.s)
par(op)
summary.gam(bm_1.s)
op <- par(mfrow=c(3,3), mar=c(4,4,1,1))
plot(bm_1.s, all.terms = TRUE, rug=TRUE, shade= TRUE, shade.col = "lightblue", seWithMean = TRUE, residuals=TRUE)
par(op)

### 3.2 Trout ####
bm_0.t = gam(bm.0 ~ s(year) +
               s(WGSn) + 
               s(WGSe) + 
               s(elevation) +
               s(NAO) +
               s(ratio.0, k=7) +
               s(Temp)+
               s(Prec)+
               s(river, bs="re") +
               s(effort, bs="re"), 
             data = fish[fish$species=="Salmo trutta fario",],
             family=Gamma(link="log"),
             method="ML",
             select=TRUE)
op <- par(mfrow=c(2,2), mar=c(4,4,1,1))
gam.check(bm_0.t)
par(op)
summary.gam(bm_0.t)
op <- par(mfrow=c(3,3), mar=c(4,4,1,1))
plot(bm_0.t, all.terms = TRUE, rug=TRUE, shade= TRUE, shade.col = "lightblue", seWithMean = TRUE, residuals=TRUE)
par(op)

bm_1.t = gam(bm.1 ~ s(year) +
               s(WGSn) + 
               s(WGSe) + 
               s(elevation) +
               s(NAO) +
               s(ratio.1) +
               s(Temp)+
               s(Prec)+
               s(river,bs="re") +
               s(effort, bs="re"), 
             data = fish[fish$species=="Salmo trutta fario",],
             family=Gamma(link="log"),
             method="ML",
             select=TRUE)

op <- par(mfrow=c(2,2), mar=c(4,4,1,1))
gam.check(bm_1.t)
par(op)
summary.gam(bm_1.t)
op <- par(mfrow=c(3,3), mar=c(4,4,1,1))
plot(bm_1.t, all.terms = TRUE, rug=TRUE, shade= TRUE, shade.col = "lightblue", seWithMean = TRUE, residuals=TRUE)
par(op)

##### 04. MODEL FITTING DENSITY #####
### 4.1 Salmon ####
dens_0.s = gam(dens.0 ~ s(year) +
                 s(WGSn) + 
                 s(WGSe) + 
                 s(elevation) +
                 s(NAO) +
                 s(ratio.0) +
                 s(Temp)+
                 s(Prec)+
                 s(river, bs="re") +
                 s(effort, bs="re"), 
               data = fish[fish$species=="Salmo salar",],
               family=Gamma(link="log"),
               method="ML",
               select=TRUE)
op <- par(mfrow=c(2,2), mar=c(4,4,1,1))
gam.check(dens_0.s)
par(op)
summary.gam(dens_0.s)
op <- par(mfrow=c(3,3), mar=c(4,4,1,1))
plot(dens_0.s, all.terms = TRUE, rug=TRUE, shade= TRUE, shade.col = "lightblue", seWithMean = TRUE, residuals=TRUE)
par(op)

dens_1.s = gam(dens.1 ~ s(year) +
                 s(WGSn) + 
                 s(WGSe) + 
                 s(elevation) +
                 s(NAO) +
                 s(ratio.1) +
                 s(Temp)+
                 s(Prec)+
                 s(river,bs="re") +
                 s(effort, bs="re"), 
               data = fish[fish$species=="Salmo salar",],
               family=Gamma(link="log"),
               method="ML",
               select=TRUE)

op <- par(mfrow=c(2,2), mar=c(4,4,1,1))
gam.check(dens_1.s)
par(op)
summary.gam(dens_1.s)
op <- par(mfrow=c(3,3), mar=c(4,4,1,1))
plot(dens_1.s, all.terms = TRUE, rug=TRUE, shade= TRUE, shade.col = "lightblue", seWithMean = TRUE, residuals=TRUE)
par(op)

### 4.2 Trout ####
dens_0.t = gam(dens.0 ~ s(year) +
                 s(WGSn) + 
                 s(WGSe) + 
                 s(elevation) +
                 s(NAO) +
                 s(ratio.0, k=7) +
                 s(Temp)+
                 s(Prec)+
                 s(river, bs="re") +
                 s(effort, bs="re"), 
               data = fish[fish$species=="Salmo trutta fario",],
               family=Gamma(link="log"),
               method="ML",
               select=TRUE)
op <- par(mfrow=c(2,2), mar=c(4,4,1,1))
gam.check(dens_0.t)
par(op)
summary.gam(dens_0.t)
op <- par(mfrow=c(3,3), mar=c(4,4,1,1))
plot(dens_0.t, all.terms = TRUE, rug=TRUE, shade= TRUE, shade.col = "lightblue", seWithMean = TRUE, residuals=TRUE)
par(op)

dens_1.t = gam(dens.1 ~ s(year) +
                 s(WGSn) + 
                 s(WGSe) + 
                 s(elevation) +
                 s(NAO) +
                 s(ratio.1) +
                 s(Temp)+
                 s(Prec)+
                 s(river,bs="re") +
                 s(effort, bs="re"), 
               data = fish[fish$species=="Salmo trutta fario",],
               family=Gamma(link="log"),
               method="ML",
               select=TRUE)

op <- par(mfrow=c(2,2), mar=c(4,4,1,1))
gam.check(dens_1.t)
par(op)
summary.gam(dens_1.t)
op <- par(mfrow=c(3,3), mar=c(4,4,1,1))
plot(dens_1.t, all.terms = TRUE, rug=TRUE, shade= TRUE, shade.col = "lightblue", seWithMean = TRUE, residuals=TRUE)
par(op)




############## Stocked of individuals over time






