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
# #### 2.1 with Ebro ####
# # fish$river <- as.factor(fish$river)
# # fish$species <- as.factor(fish$species)
# 
# ### 2.1.1 combined ####
# ind_0 = gam(ind.0 ~ s(year) +
#               s(WGSn) + 
#               s(WGSe) + 
#               s(elevation) +
#               s(NAO) +
#               factor(species)+
#               s(river, bs="re"), 
#             data = fish,
#             family=nb(link=log),
#             method="ML",
#             select=TRUE)
# op <- par(mfrow=c(2,2), mar=c(4,4,1,1))
# gam.check(ind_0)
# par(op)
# summary.gam(ind_0)
# op <- par(mfrow=c(3,3), mar=c(4,4,1,1))
# plot(ind_0, all.terms = TRUE, rug=TRUE, shade= TRUE, shade.col = "lightblue", seWithMean = TRUE, residuals=TRUE)
# par(op)
# 
# ind_1 = gam(ind.1 ~ s(year) +
#               s(WGSn) + 
#               s(WGSe) + 
#               s(elevation) +
#               s(NAO) +
#               factor(species)+
#               s(river,bs="re"), 
#             data = fish,
#             family=nb(link=log),
#             method="ML",
#             select=TRUE)
# 
# op <- par(mfrow=c(2,2), mar=c(4,4,1,1))
# gam.check(ind_1)
# par(op)
# summary.gam(ind_1)
# op <- par(mfrow=c(3,3), mar=c(4,4,1,1))
# plot(ind_1, all.terms = TRUE, rug=TRUE, shade= TRUE, shade.col = "lightblue", seWithMean = TRUE, residuals=TRUE)
# par(op)
# 
# ### 2.1.2 Salmon ####
# ind_0.s = gam(ind.0 ~ s(year) +
#                 s(WGSn) + 
#                 s(WGSe) + 
#                 s(elevation) +
#                 s(NAO) +
#                 s(river, bs="re"), 
#               data = fish[fish$species=="Salmo salar",],
#               family=nb(link=log),
#               method="ML",
#               select=TRUE)
# op <- par(mfrow=c(2,2), mar=c(4,4,1,1))
# gam.check(ind_0.s)
# par(op)
# summary.gam(ind_0.s)
# op <- par(mfrow=c(2,3), mar=c(4,4,1,1))
# plot(ind_0.s, all.terms = TRUE, rug=TRUE, shade= TRUE, shade.col = "lightblue", seWithMean = TRUE, residuals=TRUE)
# par(op)
# 
# ind_1.s = gam(ind.1 ~ s(year) +
#                 s(WGSn) + 
#                 s(WGSe) + 
#                 s(elevation) +
#                 s(NAO) +
#                 s(river,bs="re"), 
#               data = fish[fish$species=="Salmo salar",],
#               family=nb(link=log),
#               method="ML",
#               select=TRUE)
# 
# op <- par(mfrow=c(2,2), mar=c(4,4,1,1))
# gam.check(ind_1.s)
# par(op)
# summary.gam(ind_1.s)
# op <- par(mfrow=c(2,3), mar=c(4,4,1,1))
# plot(ind_1.s, all.terms = TRUE, rug=TRUE, shade= TRUE, shade.col = "lightblue", seWithMean = TRUE, residuals=TRUE)
# par(op)
# 
# ### 2.1.3 Trout ####
# ind_0.t = gam(ind.0 ~ s(year) +
#                 s(WGSn) + 
#                 s(WGSe) + 
#                 s(elevation) +
#                 s(NAO) +
#                 s(river, bs="re"), 
#               data = fish[fish$species=="Salmo trutta fario",],
#               family=nb(link=log),
#               method="ML",
#               select=TRUE)
# op <- par(mfrow=c(2,2), mar=c(4,4,1,1))
# gam.check(ind_0.t)
# par(op)
# summary.gam(ind_0.t)
# op <- par(mfrow=c(2,3), mar=c(4,4,1,1))
# plot(ind_0.t, all.terms = TRUE, rug=TRUE, shade= TRUE, shade.col = "lightblue", seWithMean = TRUE, residuals=TRUE)
# par(op)
# 
# ind_1.t = gam(ind.1 ~ s(year) +
#                 s(WGSn) + 
#                 s(WGSe) + 
#                 s(elevation) +
#                 s(NAO) +
#                 s(river,bs="re"), 
#               data = fish[fish$species=="Salmo trutta fario",],
#               family=nb(link=log),
#               method="ML",
#               select=TRUE)
# 
# op <- par(mfrow=c(2,2), mar=c(4,4,1,1))
# gam.check(ind_1.t)
# par(op)
# summary.gam(ind_1.t)
# op <- par(mfrow=c(2,3), mar=c(4,4,1,1))
# plot(ind_1.t, all.terms = TRUE, rug=TRUE, shade= TRUE, shade.col = "lightblue", seWithMean = TRUE, residuals=TRUE)
# par(op)

#### 2.2 without Ebro ####
# fish.nEbro$river <- as.factor(fish.nEbro$river)
# fish.nEbro$species <- as.factor(fish.nEbro$species)

### 2.2.1 combined ####
ind_0.nE = gam(ind.0 ~ s(year) +
                 s(WGSn) + 
                 s(WGSe) + 
                 s(elevation) +
                 s(NAO) +
                 factor(species)+
                 s(ratio.0) +
                 s(river, bs="re"), 
               data = fish.nEbro,
               family=nb(link=log),
               method="ML",
               select=TRUE)
op <- par(mfrow=c(2,2), mar=c(4,4,1,1))
gam.check(ind_0.nE)
par(op)
summary.gam(ind_0.nE)
op <- par(mfrow=c(3,3), mar=c(4,4,1,1))
plot(ind_0.nE, all.terms = TRUE, rug=TRUE, shade= TRUE, shade.col = "lightblue", seWithMean = TRUE, residuals=TRUE)
par(op)

ind_1.nE = gam(ind.1 ~ s(year) +
                 s(WGSn) + 
                 s(WGSe) + 
                 s(elevation) +
                 s(NAO) +
                 factor(species)+
                 s(ratio.1) +
                 s(river,bs="re"), 
               data = fish.nEbro,
               family=nb(link=log),
               method="ML",
               select=TRUE)

op <- par(mfrow=c(2,2), mar=c(4,4,1,1))
gam.check(ind_1.nE)
par(op)
summary.gam(ind_1.nE)
op <- par(mfrow=c(3,3), mar=c(4,4,1,1))
plot(ind_1.nE, all.terms = TRUE, rug=TRUE, shade= TRUE, shade.col = "lightblue", seWithMean = TRUE, residuals=TRUE)
par(op)

### 2.2.2 Salmon ####
ind_0.s.nE = gam(ind.0 ~ s(year) +
                   s(WGSn) + 
                   s(WGSe) + 
                   s(elevation) +
                   s(NAO) +
                   s(ratio.0) +
                   s(river, bs="re"), 
                 data = fish.nEbro[fish.nEbro$species=="Salmo salar",],
                 family=nb(link=log),
                 method="ML",
                 select=TRUE)
op <- par(mfrow=c(2,2), mar=c(4,4,1,1))
gam.check(ind_0.s.nE)
par(op)
summary.gam(ind_0.s.nE)
op <- par(mfrow=c(3,3), mar=c(4,4,1,1))
plot(ind_0.s.nE, all.terms = TRUE, rug=TRUE, shade= TRUE, shade.col = "lightblue", seWithMean = TRUE, residuals=TRUE)
par(op)

ind_1.s.nE = gam(ind.1 ~ s(year) +
                   s(WGSn) + 
                   s(WGSe) + 
                   s(elevation) +
                   s(NAO) +
                   s(ratio.1) +
                   s(river,bs="re"), 
                 data = fish.nEbro[fish.nEbro$species=="Salmo salar",],
                 family=nb(link=log),
                 method="ML",
                 select=TRUE)

op <- par(mfrow=c(2,2), mar=c(4,4,1,1))
gam.check(ind_1.s.nE)
par(op)
summary.gam(ind_1.s.nE)
op <- par(mfrow=c(3,3), mar=c(4,4,1,1))
plot(ind_1.s.nE, all.terms = TRUE, rug=TRUE, shade= TRUE, shade.col = "lightblue", seWithMean = TRUE, residuals=TRUE)
par(op)

### 2.2.3 Trout ####
ind_0.t.nE = gam(ind.0 ~ s(year) +
                   s(WGSn) + 
                   s(WGSe) + 
                   s(elevation) +
                   s(NAO) +
                   s(ratio.0, k=7) +
                   s(river, bs="re"), 
                 data = fish.nEbro[fish.nEbro$species=="Salmo trutta fario",],
                 family=nb(link=log),
                 method="ML",
                 select=TRUE)
op <- par(mfrow=c(2,2), mar=c(4,4,1,1))
gam.check(ind_0.t.nE)
par(op)
summary.gam(ind_0.t.nE)
op <- par(mfrow=c(3,3), mar=c(4,4,1,1))
plot(ind_0.t.nE, all.terms = TRUE, rug=TRUE, shade= TRUE, shade.col = "lightblue", seWithMean = TRUE, residuals=TRUE)
par(op)

ind_1.t.nE = gam(ind.1 ~ s(year) +
                   s(WGSn) + 
                   s(WGSe) + 
                   s(elevation) +
                   s(NAO) +
                   s(ratio.1) +
                   s(river,bs="re"), 
                 data = fish.nEbro[fish.nEbro$species=="Salmo trutta fario",],
                 family=nb(link=log),
                 method="ML",
                 select=TRUE)

op <- par(mfrow=c(2,2), mar=c(4,4,1,1))
gam.check(ind_1.t.nE)
par(op)
summary.gam(ind_1.t.nE)
op <- par(mfrow=c(3,3), mar=c(4,4,1,1))
plot(ind_1.t.nE, all.terms = TRUE, rug=TRUE, shade= TRUE, shade.col = "lightblue", seWithMean = TRUE, residuals=TRUE)
par(op)

##### 03. MODEL FITTING BIOMASS #####
# #### 3.1 with Ebro ####
# ### 3.1.1 combined ####
# bm_0 = gam(bm.0 ~ s(year) +
#              s(WGSn) + 
#              s(WGSe) + 
#              s(elevation) +
#              s(NAO) +
#              factor(species)+
#              s(river, bs="re"), 
#            data = fish,
#            family=Gamma(link="log"),
#            method="ML",
#            select=TRUE)
# op <- par(mfrow=c(2,2), mar=c(4,4,1,1))
# gam.check(bm_0)
# par(op)
# summary.gam(bm_0)
# op <- par(mfrow=c(3,3), mar=c(4,4,1,1))
# plot(bm_0, all.terms = TRUE, rug=TRUE, shade= TRUE, shade.col = "lightblue", seWithMean = TRUE, residuals=TRUE)
# par(op)
# 
# bm_1 = gam(bm.1 ~ s(year) +
#              s(WGSn) + 
#              s(WGSe) + 
#              s(elevation) +
#              s(NAO) +
#              factor(species)+
#              s(river,bs="re"), 
#            data = fish,
#            family=Gamma(link="log"),
#            method="ML",
#            select=TRUE)
# 
# op <- par(mfrow=c(2,2), mar=c(4,4,1,1))
# gam.check(bm_1)
# par(op)
# summary.gam(bm_1)
# op <- par(mfrow=c(3,3), mar=c(4,4,1,1))
# plot(bm_1, all.terms = TRUE, rug=TRUE, shade= TRUE, shade.col = "lightblue", seWithMean = TRUE, residuals=TRUE)
# par(op)
# 
# ### 3.1.2 Salmon ####
# bm_0.s = gam(bm.0 ~ s(year) +
#                s(WGSn) + 
#                s(WGSe) + 
#                s(elevation) +
#                s(NAO) +
#                s(river, bs="re"), 
#              data = fish[fish$species=="Salmo salar",],
#              family=Gamma(link="log"),
#              method="ML",
#              select=TRUE)
# op <- par(mfrow=c(2,2), mar=c(4,4,1,1))
# gam.check(bm_0.s)
# par(op)
# summary.gam(bm_0.s)
# op <- par(mfrow=c(2,3), mar=c(4,4,1,1))
# plot(bm_0.s, all.terms = TRUE, rug=TRUE, shade= TRUE, shade.col = "lightblue", seWithMean = TRUE, residuals=TRUE)
# par(op)
# 
# bm_1.s = gam(bm.1 ~ s(year) +
#                s(WGSn) + 
#                s(WGSe) + 
#                s(elevation) +
#                s(NAO) +
#                s(river,bs="re"), 
#              data = fish[fish$species=="Salmo salar",],
#              family=Gamma(link="log"),
#              method="ML",
#              select=TRUE)
# 
# op <- par(mfrow=c(2,2), mar=c(4,4,1,1))
# gam.check(bm_1.s)
# par(op)
# summary.gam(bm_1.s)
# op <- par(mfrow=c(2,3), mar=c(4,4,1,1))
# plot(bm_1.s, all.terms = TRUE, rug=TRUE, shade= TRUE, shade.col = "lightblue", seWithMean = TRUE, residuals=TRUE)
# par(op)
# 
# ### 3.1.3 Trout ####
# bm_0.t = gam(bm.0 ~ s(year) +
#                s(WGSn) + 
#                s(WGSe) + 
#                s(elevation) +
#                s(NAO) +
#                s(river, bs="re"), 
#              data = fish[fish$species=="Salmo trutta fario",],
#              family=Gamma(link="log"),
#              method="ML",
#              select=TRUE)
# op <- par(mfrow=c(2,2), mar=c(4,4,1,1))
# gam.check(bm_0.t)
# par(op)
# summary.gam(bm_0.t)
# op <- par(mfrow=c(2,3), mar=c(4,4,1,1))
# plot(bm_0.t, all.terms = TRUE, rug=TRUE, shade= TRUE, shade.col = "lightblue", seWithMean = TRUE, residuals=TRUE)
# par(op)
# 
# bm_1.t = gam(bm.1 ~ s(year) +
#                s(WGSn) + 
#                s(WGSe) + 
#                s(elevation) +
#                s(NAO) +
#                s(river,bs="re"), 
#              data = fish[fish$species=="Salmo trutta fario",],
#              family=Gamma(link="log"),
#              method="ML",
#              select=TRUE)
# 
# op <- par(mfrow=c(2,2), mar=c(4,4,1,1))
# gam.check(bm_1.t)
# par(op)
# summary.gam(bm_1.t)
# op <- par(mfrow=c(2,3), mar=c(4,4,1,1))
# plot(bm_1.t, all.terms = TRUE, rug=TRUE, shade= TRUE, shade.col = "lightblue", seWithMean = TRUE, residuals=TRUE)
# par(op)

#### 3.2 without Ebro ####
# fish.nEbro$river <- as.factor(fish.nEbro$river)
# fish.nEbro$species <- as.factor(fish.nEbro$species)

### 3.2.1 combined ####
bm_0.nE = gam(bm.0 ~ s(year) +
                s(WGSn) + 
                s(WGSe) + 
                s(elevation) +
                s(NAO) +
                factor(species)+
                s(ratio.0) +
                s(river, bs="re"), 
              data = fish.nEbro,
              family=Gamma(link="log"),
              method="ML",
              select=TRUE)
op <- par(mfrow=c(2,2), mar=c(4,4,1,1))
gam.check(bm_0.nE)
par(op)
summary.gam(bm_0.nE)
op <- par(mfrow=c(3,3), mar=c(4,4,1,1))
plot(bm_0.nE, all.terms = TRUE, rug=TRUE, shade= TRUE, shade.col = "lightblue", seWithMean = TRUE, residuals=TRUE)
par(op)

bm_1.nE = gam(bm.1 ~ s(year) +
                s(WGSn) + 
                s(WGSe) + 
                s(elevation) +
                s(NAO) +
                factor(species)+
                s(ratio.1) +
                s(river,bs="re"), 
              data = fish.nEbro,
              family=Gamma(link="log"),
              method="ML",
              select=TRUE)

op <- par(mfrow=c(2,2), mar=c(4,4,1,1))
gam.check(bm_1.nE)
par(op)
summary.gam(bm_1.nE)
op <- par(mfrow=c(3,3), mar=c(4,4,1,1))
plot(bm_1.nE, all.terms = TRUE, rug=TRUE, shade= TRUE, shade.col = "lightblue", seWithMean = TRUE, residuals=TRUE)
par(op)

### 3.2.2 Salmon ####
bm_0.s.nE = gam(bm.0 ~ s(year) +
                  s(WGSn) + 
                  s(WGSe) + 
                  s(elevation) +
                  s(NAO) +
                  s(ratio.0) +
                  s(river, bs="re"), 
                data = fish.nEbro[fish.nEbro$species=="Salmo salar",],
                family=Gamma(link="log"),
                method="ML",
                select=TRUE)
op <- par(mfrow=c(2,2), mar=c(4,4,1,1))
gam.check(bm_0.s.nE)
par(op)
summary.gam(bm_0.s.nE)
op <- par(mfrow=c(3,3), mar=c(4,4,1,1))
plot(bm_0.s.nE, all.terms = TRUE, rug=TRUE, shade= TRUE, shade.col = "lightblue", seWithMean = TRUE, residuals=TRUE)
par(op)

bm_1.s.nE = gam(bm.1 ~ s(year) +
                  s(WGSn) + 
                  s(WGSe) + 
                  s(elevation) +
                  s(NAO) +
                  s(ratio.1) +
                  s(river,bs="re"), 
                data = fish.nEbro[fish.nEbro$species=="Salmo salar",],
                family=Gamma(link="log"),
                method="ML",
                select=TRUE)

op <- par(mfrow=c(2,2), mar=c(4,4,1,1))
gam.check(bm_1.s.nE)
par(op)
summary.gam(bm_1.s.nE)
op <- par(mfrow=c(3,3), mar=c(4,4,1,1))
plot(bm_1.s.nE, all.terms = TRUE, rug=TRUE, shade= TRUE, shade.col = "lightblue", seWithMean = TRUE, residuals=TRUE)
par(op)

### 3.2.3 Trout ####
bm_0.t.nE = gam(bm.0 ~ s(year) +
                  s(WGSn) + 
                  s(WGSe) + 
                  s(elevation) +
                  s(NAO) +
                  s(ratio.0, k=7) +
                  s(river, bs="re"), 
                data = fish.nEbro[fish.nEbro$species=="Salmo trutta fario",],
                family=Gamma(link="log"),
                method="ML",
                select=TRUE)
op <- par(mfrow=c(2,2), mar=c(4,4,1,1))
gam.check(bm_0.t.nE)
par(op)
summary.gam(bm_0.t.nE)
op <- par(mfrow=c(3,3), mar=c(4,4,1,1))
plot(bm_0.t.nE, all.terms = TRUE, rug=TRUE, shade= TRUE, shade.col = "lightblue", seWithMean = TRUE, residuals=TRUE)
par(op)

bm_1.t.nE = gam(bm.1 ~ s(year) +
                  s(WGSn) + 
                  s(WGSe) + 
                  s(elevation) +
                  s(NAO) +
                  s(ratio.1) +
                  s(river,bs="re"), 
                data = fish.nEbro[fish.nEbro$species=="Salmo trutta fario",],
                family=Gamma(link="log"),
                method="ML",
                select=TRUE)

op <- par(mfrow=c(2,2), mar=c(4,4,1,1))
gam.check(bm_1.t.nE)
par(op)
summary.gam(bm_1.t.nE)
op <- par(mfrow=c(3,3), mar=c(4,4,1,1))
plot(bm_1.t.nE, all.terms = TRUE, rug=TRUE, shade= TRUE, shade.col = "lightblue", seWithMean = TRUE, residuals=TRUE)
par(op)

##### 04. MODEL FITTING DENSITY #####
# #### 4.1 with Ebro ####
# ### 4.1.1 combined ####
# dens_0 = gam(dens.0 ~ s(year) +
#                s(WGSn) + 
#                s(WGSe) + 
#                s(elevation) +
#                s(NAO) +
#                factor(species)+
#                s(river, bs="re"), 
#              data = fish,
#              family=Gamma(link="log"),
#              method="ML",
#              select=TRUE)
# op <- par(mfrow=c(2,2), mar=c(4,4,1,1))
# gam.check(dens_0)
# par(op)
# summary.gam(dens_0)
# op <- par(mfrow=c(3,3), mar=c(4,4,1,1))
# plot(dens_0, all.terms = TRUE, rug=TRUE, shade= TRUE, shade.col = "lightblue", seWithMean = TRUE, residuals=TRUE)
# par(op)
# 
# dens_1 = gam(dens.1 ~ s(year) +
#                s(WGSn) + 
#                s(WGSe) + 
#                s(elevation) +
#                s(NAO) +
#                factor(species)+
#                s(river,bs="re"), 
#              data = fish,
#              family=Gamma(link="log"),
#              method="ML",
#              select=TRUE)
# 
# op <- par(mfrow=c(2,2), mar=c(4,4,1,1))
# gam.check(dens_1)
# par(op)
# summary.gam(dens_1)
# op <- par(mfrow=c(3,3), mar=c(4,4,1,1))
# plot(dens_1, all.terms = TRUE, rug=TRUE, shade= TRUE, shade.col = "lightblue", seWithMean = TRUE, residuals=TRUE)
# par(op)
# 
# ### 4.1.2 Salmon ####
# dens_0.s = gam(dens.0 ~ s(year) +
#                  s(WGSn) + 
#                  s(WGSe) + 
#                  s(elevation) +
#                  s(NAO) +
#                  s(river, bs="re"), 
#                data = fish[fish$species=="Salmo salar",],
#                family=Gamma(link="log"),
#                method="ML",
#                select=TRUE)
# op <- par(mfrow=c(2,2), mar=c(4,4,1,1))
# gam.check(dens_0.s)
# par(op)
# summary.gam(dens_0.s)
# op <- par(mfrow=c(2,3), mar=c(4,4,1,1))
# plot(dens_0.s, all.terms = TRUE, rug=TRUE, shade= TRUE, shade.col = "lightblue", seWithMean = TRUE, residuals=TRUE)
# par(op)
# 
# dens_1.s = gam(dens.1 ~ s(year) +
#                  s(WGSn) + 
#                  s(WGSe) + 
#                  s(elevation) +
#                  s(NAO) +
#                  s(river,bs="re"), 
#                data = fish[fish$species=="Salmo salar",],
#                family=Gamma(link="log"),
#                method="ML",
#                select=TRUE)
# 
# op <- par(mfrow=c(2,2), mar=c(4,4,1,1))
# gam.check(dens_1.s)
# par(op)
# summary.gam(dens_1.s)
# op <- par(mfrow=c(2,3), mar=c(4,4,1,1))
# plot(dens_1.s, all.terms = TRUE, rug=TRUE, shade= TRUE, shade.col = "lightblue", seWithMean = TRUE, residuals=TRUE)
# par(op)
# 
# ### 4.1.3 Trout ####
# dens_0.t = gam(dens.0 ~ s(year) +
#                  s(WGSn) + 
#                  s(WGSe) + 
#                  s(elevation) +
#                  s(NAO) +
#                  s(river, bs="re"), 
#                data = fish[fish$species=="Salmo trutta fario",],
#                family=Gamma(link="log"),
#                method="ML",
#                select=TRUE)
# op <- par(mfrow=c(2,2), mar=c(4,4,1,1))
# gam.check(dens_0.t)
# par(op)
# summary.gam(dens_0.t)
# op <- par(mfrow=c(2,3), mar=c(4,4,1,1))
# plot(dens_0.t, all.terms = TRUE, rug=TRUE, shade= TRUE, shade.col = "lightblue", seWithMean = TRUE, residuals=TRUE)
# par(op)
# 
# dens_1.t = gam(dens.1 ~ s(year) +
#                  s(WGSn) + 
#                  s(WGSe) + 
#                  s(elevation) +
#                  s(NAO) +
#                  s(river,bs="re"), 
#                data = fish[fish$species=="Salmo trutta fario",],
#                family=Gamma(link="log"),
#                method="ML",
#                select=TRUE)
# 
# op <- par(mfrow=c(2,2), mar=c(4,4,1,1))
# gam.check(dens_1.t)
# par(op)
# summary.gam(dens_1.t)
# op <- par(mfrow=c(2,3), mar=c(4,4,1,1))
# plot(dens_1.t, all.terms = TRUE, rug=TRUE, shade= TRUE, shade.col = "lightblue", seWithMean = TRUE, residuals=TRUE)
# par(op)

#### 4.2 without Ebro ####
# fish.nEbro$river <- as.factor(fish.nEbro$river)
# fish.nEbro$species <- as.factor(fish.nEbro$species)

### 4.2.1 combined ####
dens_0.nE = gam(dens.0 ~ s(year) +
                  s(WGSn) + 
                  s(WGSe) + 
                  s(elevation) +
                  s(NAO) +
                  factor(species)+
                  s(ratio.0) +
                  s(river, bs="re"), 
                data = fish.nEbro,
                family=Gamma(link="log"),
                method="ML",
                select=TRUE)
op <- par(mfrow=c(2,2), mar=c(4,4,1,1))
gam.check(dens_0.nE)
par(op)
summary.gam(dens_0.nE)
op <- par(mfrow=c(3,3), mar=c(4,4,1,1))
plot(dens_0.nE, all.terms = TRUE, rug=TRUE, shade= TRUE, shade.col = "lightblue", seWithMean = TRUE, residuals=TRUE)
par(op)

dens_1.nE = gam(dens.1 ~ s(year) +
                  s(WGSn) + 
                  s(WGSe) + 
                  s(elevation) +
                  s(NAO) +
                  factor(species)+
                  s(ratio.1) +
                  s(river,bs="re"), 
                data = fish.nEbro,
                family=Gamma(link="log"),
                method="ML",
                select=TRUE)

op <- par(mfrow=c(2,2), mar=c(4,4,1,1))
gam.check(dens_1.nE)
par(op)
summary.gam(dens_1.nE)
op <- par(mfrow=c(3,3), mar=c(4,4,1,1))
plot(dens_1.nE, all.terms = TRUE, rug=TRUE, shade= TRUE, shade.col = "lightblue", seWithMean = TRUE, residuals=TRUE)
par(op)

### 4.2.2 Salmon ####
dens_0.s.nE = gam(dens.0 ~ s(year) +
                    s(WGSn) + 
                    s(WGSe) + 
                    s(elevation) +
                    s(NAO) +
                    s(ratio.0) +
                    s(river, bs="re"), 
                  data = fish.nEbro[fish.nEbro$species=="Salmo salar",],
                  family=Gamma(link="log"),
                  method="ML",
                  select=TRUE)
op <- par(mfrow=c(2,2), mar=c(4,4,1,1))
gam.check(dens_0.s.nE)
par(op)
summary.gam(dens_0.s.nE)
op <- par(mfrow=c(3,3), mar=c(4,4,1,1))
plot(dens_0.s.nE, all.terms = TRUE, rug=TRUE, shade= TRUE, shade.col = "lightblue", seWithMean = TRUE, residuals=TRUE)
par(op)

dens_1.s.nE = gam(dens.1 ~ s(year) +
                    s(WGSn) + 
                    s(WGSe) + 
                    s(elevation) +
                    s(NAO) +
                    s(ratio.1) +
                    s(river,bs="re"), 
                  data = fish.nEbro[fish.nEbro$species=="Salmo salar",],
                  family=Gamma(link="log"),
                  method="ML",
                  select=TRUE)

op <- par(mfrow=c(2,2), mar=c(4,4,1,1))
gam.check(dens_1.s.nE)
par(op)
summary.gam(dens_1.s.nE)
op <- par(mfrow=c(3,3), mar=c(4,4,1,1))
plot(dens_1.s.nE, all.terms = TRUE, rug=TRUE, shade= TRUE, shade.col = "lightblue", seWithMean = TRUE, residuals=TRUE)
par(op)

### 4.2.3 Trout ####
dens_0.t.nE = gam(dens.0 ~ s(year) +
                    s(WGSn) + 
                    s(WGSe) + 
                    s(elevation) +
                    s(NAO) +
                    s(ratio.0, k=7) +
                    s(river, bs="re"), 
                  data = fish.nEbro[fish.nEbro$species=="Salmo trutta fario",],
                  family=Gamma(link="log"),
                  method="ML",
                  select=TRUE)
op <- par(mfrow=c(2,2), mar=c(4,4,1,1))
gam.check(dens_0.t.nE)
par(op)
summary.gam(dens_0.t.nE)
op <- par(mfrow=c(3,3), mar=c(4,4,1,1))
plot(dens_0.t.nE, all.terms = TRUE, rug=TRUE, shade= TRUE, shade.col = "lightblue", seWithMean = TRUE, residuals=TRUE)
par(op)

dens_1.t.nE = gam(dens.1 ~ s(year) +
                    s(WGSn) + 
                    s(WGSe) + 
                    s(elevation) +
                    s(NAO) +
                    s(ratio.1) +
                    s(river,bs="re"), 
                  data = fish.nEbro[fish.nEbro$species=="Salmo trutta fario",],
                  family=Gamma(link="log"),
                  method="ML",
                  select=TRUE)

op <- par(mfrow=c(2,2), mar=c(4,4,1,1))
gam.check(dens_1.t.nE)
par(op)
summary.gam(dens_1.t.nE)
op <- par(mfrow=c(3,3), mar=c(4,4,1,1))
plot(dens_1.t.nE, all.terms = TRUE, rug=TRUE, shade= TRUE, shade.col = "lightblue", seWithMean = TRUE, residuals=TRUE)
par(op)

##### 05. PLOTS #####
#### 5.1 functions ####
createDATA <- function(model0, model1, explanatory){
  T1 <- visreg(model0, scale='response', explanatory, line.par = list(col = 'black'), plot=FALSE)
  T2 <- visreg(model1, scale='response', explanatory, line.par = list(col = 'blue'), plot=FALSE)

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

#### 5.2. individuals combined ####
# ### 5.2.1 with Ebro ####
# fits_year <- createDATA(ind_0, ind_1, "year")
# fits_WGSn <- createDATA(ind_0, ind_1, "WGSn")
# fits_WGSe <- createDATA(ind_0, ind_1, "WGSe")
# fits_elev <- createDATA(ind_0, ind_1, "elevation")
# fits_NAO <- createDATA(ind_0, ind_1, "NAO")
# 
# svg("plot gam individuals year.svg", height=10, width=10)
# plotGAMyear(fits_year, "Individuals", "Age Class", "", "with Ebro")
# dev.off()
# 
# svg("plot gam individuals WGSn.svg", height=10, width=10)
# plotGAMnorthing(fits_WGSn, "Individuals", "Age Class", "", "with Ebro")
# dev.off()
# 
# svg("plot gam individuals WGSe.svg", height=10, width=10)
# plotGAMeasting(fits_WGSe, "Individuals", "Age Class", "", "with Ebro")
# dev.off()
# 
# svg("plot gam individuals elevation.svg", height=10, width=10)
# plotGAMelev(fits_elev, "Individuals", "Age Class", "", "with Ebro")
# dev.off()
# 
# svg("plot gam individuals NAO.svg", height=10, width=10)
# plotGAMnao(fits_NAO, "Individuals", "Age Class", "", "with Ebro")
# dev.off()

### 5.2.2 without Ebro ####
fits_year.nE <- createDATA(ind_0.nE, ind_1.nE, "year")
fits_WGSn.nE <- createDATA(ind_0.nE, ind_1.nE, "WGSn")
fits_WGSe.nE <- createDATA(ind_0.nE, ind_1.nE, "WGSe")
fits_elev.nE <- createDATA(ind_0.nE, ind_1.nE, "elevation")
fits_NAO.nE <- createDATA(ind_0.nE, ind_1.nE, "NAO")
fits_stocking.nE <- createDATAstocking(ind_0.nE, ind_1.nE)

svg("plot gam individuals year - no Ebro.svg", height=10, width=10)
plotGAMyear(fits_year.nE, "Individuals", "Age Class", "", "without Ebro")
dev.off()

svg("plot gam individuals WGSn - no Ebro.svg", height=10, width=10)
plotGAMnorthing(fits_WGSn.nE, "Individuals", "Age Class", "", "without Ebro")
dev.off()

svg("plot gam individuals WGSe - no Ebro.svg", height=10, width=10)
plotGAMeasting(fits_WGSe.nE, "Individuals", "Age Class", "", "without Ebro")
dev.off()

svg("plot gam individuals elevation - no Ebro.svg", height=10, width=10)
plotGAMelev(fits_elev.nE, "Individuals", "Age Class", "", "without Ebro")
dev.off()

svg("plot gam individuals NAO - no Ebro.svg", height=10, width=10)
plotGAMnao(fits_NAO.nE, "Individuals", "Age Class", "", "without Ebro")
dev.off()

svg("plot gam individuals stocking ratio - no Ebro.svg", height=10, width=10)
plotGAMstocking(fits_stocking.nE, "Individuals", "Age Class", "", "without Ebro")
dev.off()

#### 5.3 biomass combined ####
# ### 5.3.1 with Ebro ####
# fits_year <- createDATA(bm_0, bm_1, "year")
# fits_WGSn <- createDATA(bm_0, bm_1, "WGSn")
# fits_WGSe <- createDATA(bm_0, bm_1, "WGSe")
# fits_elev <- createDATA(bm_0, bm_1, "elevation")
# fits_NAO <- createDATA(bm_0, bm_1, "NAO")
# 
# svg("plot gam bm year.svg", height=10, width=10)
# plotGAMyear(fits_year, as.expression(bquote("Biomass [kg/" ~ m^2 ~ "]")), "Age Class", "", "with Ebro")
# dev.off()
# 
# svg("plot gam bm WGSn.svg", height=10, width=10)
# plotGAMnorthing(fits_WGSn, as.expression(bquote("Biomass [kg/" ~ m^2 ~ "]")), "Age Class", "", "with Ebro")
# dev.off()
# 
# svg("plot gam bm WGSe.svg", height=10, width=10)
# plotGAMeasting(fits_WGSe, as.expression(bquote("Biomass [kg/" ~ m^2 ~ "]")), "Age Class", "", "with Ebro")
# dev.off()
# 
# svg("plot gam bm elevation.svg", height=10, width=10)
# plotGAMelev(fits_elev, as.expression(bquote("Biomass [kg/" ~ m^2 ~ "]")), "Age Class", "", "with Ebro")
# dev.off()
# 
# svg("plot gam bm NAO.svg", height=10, width=10)
# plotGAMnao(fits_NAO, as.expression(bquote("Biomass [kg/" ~ m^2 ~ "]")), "Age Class", "", "with Ebro")
# dev.off()

### 5.3.2 without Ebro ####
fits_year.nE <- createDATA(bm_0.nE, bm_1.nE, "year")
fits_WGSn.nE <- createDATA(bm_0.nE, bm_1.nE, "WGSn")
fits_WGSe.nE <- createDATA(bm_0.nE, bm_1.nE, "WGSe")
fits_elev.nE <- createDATA(bm_0.nE, bm_1.nE, "elevation")
fits_NAO.nE <- createDATA(bm_0.nE, bm_1.nE, "NAO")
fits_stocking.nE <- createDATAstocking(bm_0.nE, bm_1.nE)

svg("plot gam bm year - no Ebro.svg", height=10, width=10)
plotGAMyear(fits_year.nE, as.expression(bquote("Biomass [kg/" ~ m^2 ~ "]")), "Age Class", "", "without Ebro")
dev.off()

svg("plot gam bm WGSn - no Ebro.svg", height=10, width=10)
plotGAMnorthing(fits_WGSn.nE, as.expression(bquote("Biomass [kg/" ~ m^2 ~ "]")), "Age Class", "", "without Ebro")
dev.off()

svg("plot gam bm WGSe - no Ebro.svg", height=10, width=10)
plotGAMeasting(fits_WGSe.nE, as.expression(bquote("Biomass [kg/" ~ m^2 ~ "]")), "Age Class", "", "without Ebro")
dev.off()

svg("plot gam bm elevation - no Ebro.svg", height=10, width=10)
plotGAMelev(fits_elev.nE, as.expression(bquote("Biomass [kg/" ~ m^2 ~ "]")), "Age Class", "", "without Ebro")
dev.off()

svg("plot gam bm NAO - no Ebro.svg", height=10, width=10)
plotGAMnao(fits_NAO.nE, as.expression(bquote("Biomass [kg/" ~ m^2 ~ "]")), "Age Class", "", "without Ebro")
dev.off()

svg("plot gam bm stocking ratio - no Ebro.svg", height=10, width=10)
plotGAMstocking(fits_stocking.nE, as.expression(bquote("Biomass [kg/" ~ m^2 ~ "]")), "Age Class", "", "without Ebro")
dev.off()

#### 5.4. density combined ####
# ### 5.4.1 with Ebro ####
# fits_year <- createDATA(dens_0, dens_1, "year")
# fits_WGSn <- createDATA(dens_0, dens_1, "WGSn")
# fits_WGSe <- createDATA(dens_0, dens_1, "WGSe")
# fits_elev <- createDATA(dens_0, dens_1, "elevation")
# fits_NAO <- createDATA(dens_0, dens_1, "NAO")
# 
# svg("plot gam density year.svg", height=10, width=10)
# plotGAMyear(fits_year, as.expression(bquote("Density [ind/" ~ m^2 ~ "]")), "Age Class", "", "with Ebro")
# dev.off()
# 
# svg("plot gam density WGSn.svg", height=10, width=10)
# plotGAMnorthing(fits_WGSn, as.expression(bquote("Density [ind/" ~ m^2 ~ "]")), "Age Class", "", "with Ebro")
# dev.off()
# 
# svg("plot gam density WGSe.svg", height=10, width=10)
# plotGAMeasting(fits_WGSe, as.expression(bquote("Density [ind/" ~ m^2 ~ "]")), "Age Class", "", "with Ebro")
# dev.off()
# 
# svg("plot gam density elevation.svg", height=10, width=10)
# plotGAMelev(fits_elev, as.expression(bquote("Density [ind/" ~ m^2 ~ "]")), "Age Class", "", "with Ebro")
# dev.off()
# 
# svg("plot gam density NAO.svg", height=10, width=10)
# plotGAMnao(fits_NAO, as.expression(bquote("Density [ind/" ~ m^2 ~ "]")), "Age Class", "", "with Ebro")
# dev.off()

### 5.4.2 without Ebro ####
fits_year.nE <- createDATA(dens_0.nE, dens_1.nE, "year")
fits_WGSn.nE <- createDATA(dens_0.nE, dens_1.nE, "WGSn")
fits_WGSe.nE <- createDATA(dens_0.nE, dens_1.nE, "WGSe")
fits_elev.nE <- createDATA(dens_0.nE, dens_1.nE, "elevation")
fits_NAO.nE <- createDATA(dens_0.nE, dens_1.nE, "NAO")
fits_stocking.nE <- createDATAstocking(dens_0.nE, dens_1.nE)

svg("plot gam density year - no Ebro.svg", height=10, width=10)
plotGAMyear(fits_year.nE, as.expression(bquote("Density [ind/" ~ m^2 ~ "]")), "Age Class", "", "without Ebro")
dev.off()

svg("plot gam density WGSn - no Ebro.svg", height=10, width=10)
plotGAMnorthing(fits_WGSn.nE, as.expression(bquote("Density [ind/" ~ m^2 ~ "]")), "Age Class", "", "without Ebro")
dev.off()

svg("plot gam density WGSe - no Ebro.svg", height=10, width=10)
plotGAMeasting(fits_WGSe.nE, as.expression(bquote("Density [ind/" ~ m^2 ~ "]")), "Age Class", "", "without Ebro")
dev.off()

svg("plot gam density elevation - no Ebro.svg", height=10, width=10)
plotGAMelev(fits_elev.nE, as.expression(bquote("Density [ind/" ~ m^2 ~ "]")), "Age Class", "", "without Ebro")
dev.off()

svg("plot gam density NAO - no Ebro.svg", height=10, width=10)
plotGAMnao(fits_NAO.nE, as.expression(bquote("Density [ind/" ~ m^2 ~ "]")), "Age Class", "", "without Ebro")
dev.off()

svg("plot gam density stocking ratio - no Ebro.svg", height=10, width=10)
plotGAMstocking(fits_stocking.nE, as.expression(bquote("Density [ind/" ~ m^2 ~ "]")), "Age Class", "", "without Ebro")
dev.off()

#### 5.5. individuals Salmon ####
# ### 5.5.1 with Ebro ####
# fits_year <- createDATA(ind_0.s, ind_1.s, "year")
# fits_WGSn <- createDATA(ind_0.s, ind_1.s, "WGSn")
# fits_WGSe <- createDATA(ind_0.s, ind_1.s, "WGSe")
# fits_elev <- createDATA(ind_0.s, ind_1.s, "elevation")
# fits_NAO <- createDATA(ind_0.s, ind_1.s, "NAO")
# 
# svg("plot gam individuals year - salmon.svg", height=10, width=10)
# plotGAMyear(fits_year, "Individuals", "Age Class", "", "Salmon - with Ebro")
# dev.off()
# 
# svg("plot gam individuals WGSn - salmon.svg", height=10, width=10)
# plotGAMnorthing(fits_WGSn, "Individuals", "Age Class", "", "Salmon - with Ebro")
# dev.off()
# 
# svg("plot gam individuals WGSe - salmon.svg", height=10, width=10)
# plotGAMeasting(fits_WGSe, "Individuals", "Age Class", "", "Salmon - with Ebro")
# dev.off()
# 
# svg("plot gam individuals elevation - salmon.svg", height=10, width=10)
# plotGAMelev(fits_elev, "Individuals", "Age Class", "", "Salmon - with Ebro")
# dev.off()
# 
# svg("plot gam individuals NAO - salmon.svg", height=10, width=10)
# plotGAMnao(fits_NAO, "Individuals", "Age Class", "", "Salmon - with Ebro")
# dev.off()

### 5.5.2 without Ebro ####
fits_year.nE <- createDATA(ind_0.s.nE, ind_1.s.nE, "year")
fits_WGSn.nE <- createDATA(ind_0.s.nE, ind_1.s.nE, "WGSn")
fits_WGSe.nE <- createDATA(ind_0.s.nE, ind_1.s.nE, "WGSe")
fits_elev.nE <- createDATA(ind_0.s.nE, ind_1.s.nE, "elevation")
fits_NAO.nE <- createDATA(ind_0.s.nE, ind_1.s.nE, "NAO")
fits_stocking.nE <- createDATAstocking(ind_0.s.nE, ind_1.s.nE)

svg("plot gam individuals year - no Ebro - salmon.svg", height=10, width=10)
plotGAMyear(fits_year.nE, "Individuals", "Age Class", "", "Salmon - without Ebro")
dev.off()

svg("plot gam individuals WGSn - no Ebro - salmon.svg", height=10, width=10)
plotGAMnorthing(fits_WGSn.nE, "Individuals", "Age Class", "", "Salmon - without Ebro")
dev.off()

svg("plot gam individuals WGSe - no Ebro - salmon.svg", height=10, width=10)
plotGAMeasting(fits_WGSe.nE, "Individuals", "Age Class", "", "Salmon - without Ebro")
dev.off()

svg("plot gam individuals elevation - no Ebro - salmon.svg", height=10, width=10)
plotGAMelev(fits_elev.nE, "Individuals", "Age Class", "", "Salmon - without Ebro")
dev.off()

svg("plot gam individuals NAO - no Ebro - salmon.svg", height=10, width=10)
plotGAMnao(fits_NAO.nE, "Individuals", "Age Class", "", "Salmon - without Ebro")
dev.off()

svg("plot gam individuals stocking ratio - no Ebro - salmon.svg", height=10, width=10)
plotGAMstocking(fits_stocking.nE, "Individuals", "Age Class", "", "Salmon - without Ebro")
dev.off()

#### 5.6 biomass Salmon ####
# ### 5.6.1 with Ebro ####
# fits_year <- createDATA(bm_0.s, bm_1.s, "year")
# fits_WGSn <- createDATA(bm_0.s, bm_1.s, "WGSn")
# fits_WGSe <- createDATA(bm_0.s, bm_1.s, "WGSe")
# fits_elev <- createDATA(bm_0.s, bm_1.s, "elevation")
# fits_NAO <- createDATA(bm_0.s, bm_1.s, "NAO")
# 
# svg("plot gam bm year - salmon.svg", height=10, width=10)
# plotGAMyear(fits_year, as.expression(bquote("Biomass [kg/" ~ m^2 ~ "]")), "Age Class", "", "Salmon - with Ebro")
# dev.off()
# 
# svg("plot gam bm WGSn - salmon.svg", height=10, width=10)
# plotGAMnorthing(fits_WGSn, as.expression(bquote("Biomass [kg/" ~ m^2 ~ "]")), "Age Class", "", "Salmon - with Ebro")
# dev.off()
# 
# svg("plot gam bm WGSe - salmon.svg", height=10, width=10)
# plotGAMeasting(fits_WGSe, as.expression(bquote("Biomass [kg/" ~ m^2 ~ "]")), "Age Class", "", "Salmon - with Ebro")
# dev.off()
# 
# svg("plot gam bm elevation - salmon.svg", height=10, width=10)
# plotGAMelev(fits_elev, as.expression(bquote("Biomass [kg/" ~ m^2 ~ "]")), "Age Class", "", "Salmon - with Ebro")
# dev.off()
# 
# svg("plot gam bm NAO - salmon.svg", height=10, width=10)
# plotGAMnao(fits_NAO, as.expression(bquote("Biomass [kg/" ~ m^2 ~ "]")), "Age Class", "", "Salmon - with Ebro")
# dev.off()

### 5.6.2 without Ebro ####
fits_year.nE <- createDATA(bm_0.s.nE, bm_1.s.nE, "year")
fits_WGSn.nE <- createDATA(bm_0.s.nE, bm_1.s.nE, "WGSn")
fits_WGSe.nE <- createDATA(bm_0.s.nE, bm_1.s.nE, "WGSe")
fits_elev.nE <- createDATA(bm_0.s.nE, bm_1.s.nE, "elevation")
fits_NAO.nE <- createDATA(bm_0.s.nE, bm_1.s.nE, "NAO")
fits_stocking.nE <- createDATAstocking(bm_0.s.nE, bm_1.s.nE)

svg("plot gam bm year - no Ebro - salmon.svg", height=10, width=10)
plotGAMyear(fits_year.nE, as.expression(bquote("Biomass [kg/" ~ m^2 ~ "]")), "Age Class", "", "Salmon - without Ebro")
dev.off()

svg("plot gam bm WGSn - no Ebro - salmon.svg", height=10, width=10)
plotGAMnorthing(fits_WGSn.nE, as.expression(bquote("Biomass [kg/" ~ m^2 ~ "]")), "Age Class", "", "Salmon - without Ebro")
dev.off()

svg("plot gam bm WGSe - no Ebro - salmon.svg", height=10, width=10)
plotGAMeasting(fits_WGSe.nE, as.expression(bquote("Biomass [kg/" ~ m^2 ~ "]")), "Age Class", "", "Salmon - without Ebro")
dev.off()

svg("plot gam bm elevation - no Ebro - salmon.svg", height=10, width=10)
plotGAMelev(fits_elev.nE, as.expression(bquote("Biomass [kg/" ~ m^2 ~ "]")), "Age Class", "", "Salmon - without Ebro")
dev.off()

svg("plot gam bm NAO - no Ebro - salmon.svg", height=10, width=10)
plotGAMnao(fits_NAO.nE, as.expression(bquote("Biomass [kg/" ~ m^2 ~ "]")), "Age Class", "", "Salmon - without Ebro")
dev.off()

svg("plot gam bm stocking ratio - no Ebro - salmon.svg", height=10, width=10)
plotGAMstocking(fits_stocking.nE, as.expression(bquote("Biomass [kg/" ~ m^2 ~ "]")), "Age Class", "", "Salmon - without Ebro")
dev.off()

#### 5.7. density Salmon ####
# ### 5.7.1 with Ebro ####
# fits_year <- createDATA(dens_0.s, dens_1.s, "year")
# fits_WGSn <- createDATA(dens_0.s, dens_1.s, "WGSn")
# fits_WGSe <- createDATA(dens_0.s, dens_1.s, "WGSe")
# fits_elev <- createDATA(dens_0.s, dens_1.s, "elevation")
# fits_NAO <- createDATA(dens_0.s, dens_1.s, "NAO")
# 
# svg("plot gam density year - salmon.svg", height=10, width=10)
# plotGAMyear(fits_year, as.expression(bquote("Density [ind/" ~ m^2 ~ "]")), "Age Class", "", "Salmon - with Ebro")
# dev.off()
# 
# svg("plot gam density WGSn - salmon.svg", height=10, width=10)
# plotGAMnorthing(fits_WGSn, as.expression(bquote("Density [ind/" ~ m^2 ~ "]")), "Age Class", "", "Salmon - with Ebro")
# dev.off()
# 
# svg("plot gam density WGSe - salmon.svg", height=10, width=10)
# plotGAMeasting(fits_WGSe, as.expression(bquote("Density [ind/" ~ m^2 ~ "]")), "Age Class", "", "Salmon - with Ebro")
# dev.off()
# 
# svg("plot gam density elevation - salmon.svg", height=10, width=10)
# plotGAMelev(fits_elev, as.expression(bquote("Density [ind/" ~ m^2 ~ "]")), "Age Class", "", "Salmon - with Ebro")
# dev.off()
# 
# svg("plot gam density NAO - salmon.svg", height=10, width=10)
# plotGAMnao(fits_NAO, as.expression(bquote("Density [ind/" ~ m^2 ~ "]")), "Age Class", "", "Salmon - with Ebro")
# dev.off()

### 5.7.2 without Ebro ####
fits_year.nE <- createDATA(dens_0.s.nE, dens_1.s.nE, "year")
fits_WGSn.nE <- createDATA(dens_0.s.nE, dens_1.s.nE, "WGSn")
fits_WGSe.nE <- createDATA(dens_0.s.nE, dens_1.s.nE, "WGSe")
fits_elev.nE <- createDATA(dens_0.s.nE, dens_1.s.nE, "elevation")
fits_NAO.nE <- createDATA(dens_0.s.nE, dens_1.s.nE, "NAO")
fits_stocking.nE <- createDATAstocking(dens_0.s.nE, dens_1.s.nE)

svg("plot gam density year - no Ebro - salmon.svg", height=10, width=10)
plotGAMyear(fits_year.nE, as.expression(bquote("Density [ind/" ~ m^2 ~ "]")), "Age Class", "", "Salmon - without Ebro")
dev.off()

svg("plot gam density WGSn - no Ebro - salmon.svg", height=10, width=10)
plotGAMnorthing(fits_WGSn.nE, as.expression(bquote("Density [ind/" ~ m^2 ~ "]")), "Age Class", "", "Salmon - without Ebro")
dev.off()

svg("plot gam density WGSe - no Ebro - salmon.svg", height=10, width=10)
plotGAMeasting(fits_WGSe.nE, as.expression(bquote("Density [ind/" ~ m^2 ~ "]")), "Age Class", "", "Salmon - without Ebro")
dev.off()

svg("plot gam density elevation - no Ebro - salmon.svg", height=10, width=10)
plotGAMelev(fits_elev.nE, as.expression(bquote("Density [ind/" ~ m^2 ~ "]")), "Age Class", "", "Salmon - without Ebro")
dev.off()

svg("plot gam density NAO - no Ebro - salmon.svg", height=10, width=10)
plotGAMnao(fits_NAO.nE, as.expression(bquote("Density [ind/" ~ m^2 ~ "]")), "Age Class", "", "Salmon - without Ebro")
dev.off()

svg("plot gam density stocking ratio - no Ebro - salmon.svg", height=10, width=10)
plotGAMstocking(fits_stocking.nE, as.expression(bquote("Density [ind/" ~ m^2 ~ "]")), "Age Class", "", "Salmon - without Ebro")
dev.off()

#### 5.8. individuals trout ####
# ### 5.8.1 with Ebro ####
# fits_year <- createDATA(ind_0.t, ind_1.t, "year")
# fits_WGSn <- createDATA(ind_0.t, ind_1.t, "WGSn")
# fits_WGSe <- createDATA(ind_0.t, ind_1.t, "WGSe")
# fits_elev <- createDATA(ind_0.t, ind_1.t, "elevation")
# fits_NAO <- createDATA(ind_0.t, ind_1.t, "NAO")
# 
# svg("plot gam individuals year - trout.svg", height=10, width=10)
# plotGAMyear(fits_year, "Individuals", "Age Class", "", "Trout - with Ebro")
# dev.off()
# 
# svg("plot gam individuals WGSn - trout.svg", height=10, width=10)
# plotGAMnorthing(fits_WGSn, "Individuals", "Age Class", "", "Trout - with Ebro")
# dev.off()
# 
# svg("plot gam individuals WGSe - trout.svg", height=10, width=10)
# plotGAMeasting(fits_WGSe, "Individuals", "Age Class", "", "Trout - with Ebro")
# dev.off()
# 
# svg("plot gam individuals elevation - trout.svg", height=10, width=10)
# plotGAMelev(fits_elev, "Individuals", "Age Class", "", "Trout - with Ebro")
# dev.off()
# 
# svg("plot gam individuals NAO - trout.svg", height=10, width=10)
# plotGAMnao(fits_NAO, "Individuals", "Age Class", "", "Trout - with Ebro")
# dev.off()

### 5.8.2 without Ebro ####
fits_year.nE <- createDATA(ind_0.t.nE, ind_1.t.nE, "year")
fits_WGSn.nE <- createDATA(ind_0.t.nE, ind_1.t.nE, "WGSn")
fits_WGSe.nE <- createDATA(ind_0.t.nE, ind_1.t.nE, "WGSe")
fits_elev.nE <- createDATA(ind_0.t.nE, ind_1.t.nE, "elevation")
fits_NAO.nE <- createDATA(ind_0.t.nE, ind_1.t.nE, "NAO")
fits_stocking.nE <- createDATAstocking(ind_0.t.nE, ind_1.t.nE)

svg("plot gam individuals year - no Ebro - trout.svg", height=10, width=10)
plotGAMyear(fits_year.nE, "Individuals", "Age Class", "", "Trout - without Ebro")
dev.off()

svg("plot gam individuals WGSn - no Ebro - trout.svg", height=10, width=10)
plotGAMnorthing(fits_WGSn.nE, "Individuals", "Age Class", "", "Trout - without Ebro")
dev.off()

svg("plot gam individuals WGSe - no Ebro - trout.svg", height=10, width=10)
plotGAMeasting(fits_WGSe.nE, "Individuals", "Age Class", "", "Trout - without Ebro")
dev.off()

svg("plot gam individuals elevation - no Ebro - trout.svg", height=10, width=10)
plotGAMelev(fits_elev.nE, "Individuals", "Age Class", "", "Trout - without Ebro")
dev.off()

svg("plot gam individuals NAO - no Ebro - trout.svg", height=10, width=10)
plotGAMnao(fits_NAO.nE, "Individuals", "Age Class", "", "Trout - without Ebro")
dev.off()

svg("plot gam individuals stocking ratio - no Ebro - trout.svg", height=10, width=10)
plotGAMstocking(fits_stocking.nE, "Individuals", "Age Class", "", "Trout - without Ebro")
dev.off()

#### 5.9 biomass trout ####
# ### 5.9.1 with Ebro ####
# fits_year <- createDATA(bm_0.t, bm_1.t, "year")
# fits_WGSn <- createDATA(bm_0.t, bm_1.t, "WGSn")
# fits_WGSe <- createDATA(bm_0.t, bm_1.t, "WGSe")
# fits_elev <- createDATA(bm_0.t, bm_1.t, "elevation")
# fits_NAO <- createDATA(bm_0.t, bm_1.t, "NAO")
# 
# svg("plot gam bm year - trout.svg", height=10, width=10)
# plotGAMyear(fits_year, as.expression(bquote("Biomass [kg/" ~ m^2 ~ "]")), "Age Class", "", "Trout - with Ebro")
# dev.off()
# 
# svg("plot gam bm WGSn - trout.svg", height=10, width=10)
# plotGAMnorthing(fits_WGSn, as.expression(bquote("Biomass [kg/" ~ m^2 ~ "]")), "Age Class", "", "Trout - with Ebro")
# dev.off()
# 
# svg("plot gam bm WGSe - trout.svg", height=10, width=10)
# plotGAMeasting(fits_WGSe, as.expression(bquote("Biomass [kg/" ~ m^2 ~ "]")), "Age Class", "", "Trout - with Ebro")
# dev.off()
# 
# svg("plot gam bm elevation - trout.svg", height=10, width=10)
# plotGAMelev(fits_elev, as.expression(bquote("Biomass [kg/" ~ m^2 ~ "]")), "Age Class", "", "Trout - with Ebro")
# dev.off()
# 
# svg("plot gam bm NAO - trout.svg", height=10, width=10)
# plotGAMnao(fits_NAO, as.expression(bquote("Biomass [kg/" ~ m^2 ~ "]")), "Age Class", "", "Trout - with Ebro")
# dev.off()

### 5.9.2 without Ebro ####
fits_year.nE <- createDATA(bm_0.t.nE, bm_1.t.nE, "year")
fits_WGSn.nE <- createDATA(bm_0.t.nE, bm_1.t.nE, "WGSn")
fits_WGSe.nE <- createDATA(bm_0.t.nE, bm_1.t.nE, "WGSe")
fits_elev.nE <- createDATA(bm_0.t.nE, bm_1.t.nE, "elevation")
fits_NAO.nE <- createDATA(bm_0.t.nE, bm_1.t.nE, "NAO")
fits_stocking.nE <- createDATAstocking(bm_0.t.nE, bm_1.t.nE)

svg("plot gam bm year - no Ebro - trout.svg", height=10, width=10)
plotGAMyear(fits_year.nE, as.expression(bquote("Biomass [kg/" ~ m^2 ~ "]")), "Age Class", "", "Trout - without Ebro")
dev.off()

svg("plot gam bm WGSn - no Ebro - trout.svg", height=10, width=10)
plotGAMnorthing(fits_WGSn.nE, as.expression(bquote("Biomass [kg/" ~ m^2 ~ "]")), "Age Class", "", "Trout - without Ebro")
dev.off()

svg("plot gam bm WGSe - no Ebro - trout.svg", height=10, width=10)
plotGAMeasting(fits_WGSe.nE, as.expression(bquote("Biomass [kg/" ~ m^2 ~ "]")), "Age Class", "", "Trout - without Ebro")
dev.off()

svg("plot gam bm elevation - no Ebro - trout.svg", height=10, width=10)
plotGAMelev(fits_elev.nE, as.expression(bquote("Biomass [kg/" ~ m^2 ~ "]")), "Age Class", "", "Trout - without Ebro")
dev.off()

svg("plot gam bm NAO - no Ebro - trout.svg", height=10, width=10)
plotGAMnao(fits_NAO.nE, as.expression(bquote("Biomass [kg/" ~ m^2 ~ "]")), "Age Class", "", "Trout - without Ebro")
dev.off()

svg("plot gam bm stocking ratio - no Ebro - trout.svg", height=10, width=10)
plotGAMstocking(fits_stocking.nE, as.expression(bquote("Biomass [kg/" ~ m^2 ~ "]")), "Age Class", "", "Trout - without Ebro")
dev.off()

#### 5.10. density trout ####
# ### 5.10.1 with Ebro ####
# fits_year <- createDATA(dens_0.t, dens_1.t, "year")
# fits_WGSn <- createDATA(dens_0.t, dens_1.t, "WGSn")
# fits_WGSe <- createDATA(dens_0.t, dens_1.t, "WGSe")
# fits_elev <- createDATA(dens_0.t, dens_1.t, "elevation")
# fits_NAO <- createDATA(dens_0.t, dens_1.t, "NAO")
# 
# svg("plot gam density year - trout.svg", height=10, width=10)
# plotGAMyear(fits_year, as.expression(bquote("Density [ind/" ~ m^2 ~ "]")), "Age Class", "", "Trout - with Ebro")
# dev.off()
# 
# svg("plot gam density WGSn - trout.svg", height=10, width=10)
# plotGAMnorthing(fits_WGSn, as.expression(bquote("Density [ind/" ~ m^2 ~ "]")), "Age Class", "", "Trout - with Ebro")
# dev.off()
# 
# svg("plot gam density WGSe - trout.svg", height=10, width=10)
# plotGAMeasting(fits_WGSe, as.expression(bquote("Density [ind/" ~ m^2 ~ "]")), "Age Class", "", "Trout - with Ebro")
# dev.off()
# 
# svg("plot gam density elevation - trout.svg", height=10, width=10)
# plotGAMelev(fits_elev, as.expression(bquote("Density [ind/" ~ m^2 ~ "]")), "Age Class", "", "Trout - with Ebro")
# dev.off()
# 
# svg("plot gam density NAO - trout.svg", height=10, width=10)
# plotGAMnao(fits_NAO, as.expression(bquote("Density [ind/" ~ m^2 ~ "]")), "Age Class", "", "Trout - with Ebro")
# dev.off()

### 5.10.2 without Ebro ####
fits_year.nE <- createDATA(dens_0.t.nE, dens_1.t.nE, "year")
fits_WGSn.nE <- createDATA(dens_0.t.nE, dens_1.t.nE, "WGSn")
fits_WGSe.nE <- createDATA(dens_0.t.nE, dens_1.t.nE, "WGSe")
fits_elev.nE <- createDATA(dens_0.t.nE, dens_1.t.nE, "elevation")
fits_NAO.nE <- createDATA(dens_0.t.nE, dens_1.t.nE, "NAO")
fits_stocking.nE <- createDATAstocking(dens_0.t.nE, dens_1.t.nE)

svg("plot gam density year - no Ebro - trout.svg", height=10, width=10)
plotGAMyear(fits_year.nE, as.expression(bquote("Density [ind/" ~ m^2 ~ "]")), "Age Class", "", "Trout - without Ebro")
dev.off()

svg("plot gam density WGSn - no Ebro - trout.svg", height=10, width=10)
plotGAMnorthing(fits_WGSn.nE, as.expression(bquote("Density [ind/" ~ m^2 ~ "]")), "Age Class", "", "Trout - without Ebro")
dev.off()

svg("plot gam density WGSe - no Ebro - trout.svg", height=10, width=10)
plotGAMeasting(fits_WGSe.nE, as.expression(bquote("Density [ind/" ~ m^2 ~ "]")), "Age Class", "", "Trout - without Ebro")
dev.off()

svg("plot gam density elevation - no Ebro - trout.svg", height=10, width=10)
plotGAMelev(fits_elev.nE, as.expression(bquote("Density [ind/" ~ m^2 ~ "]")), "Age Class", "", "Trout - without Ebro")
dev.off()

svg("plot gam density NAO - no Ebro - trout.svg", height=10, width=10)
plotGAMnao(fits_NAO.nE, as.expression(bquote("Density [ind/" ~ m^2 ~ "]")), "Age Class", "", "Trout - without Ebro")
dev.off()

svg("plot gam density stocking ratio - no Ebro - trout.svg", height=10, width=10)
plotGAMstocking(fits_stocking.nE, as.expression(bquote("Density [ind/" ~ m^2 ~ "]")), "Age Class", "", "Trout - without Ebro")
dev.off()

##### 06. SUMMARY #####
#### 6.1 individuals ####
# results_ind0 <- summary(ind_0)
# results_ind1 <- summary(ind_1)
# 
# results_ind0.s <- summary(ind_0.s)
# results_ind1.s <- summary(ind_1.s)
# 
# results_ind0.t <- summary(ind_0.t)
# results_ind1.t <- summary(ind_1.t)

results_ind0.nE <- summary(ind_0.nE)
results_ind1.nE <- summary(ind_1.nE)

results_ind0.s.nE <- summary(ind_0.s.nE)
results_ind1.s.nE <- summary(ind_1.s.nE)

results_ind0.t.nE <- summary(ind_0.t.nE)
results_ind1.t.nE <- summary(ind_1.t.nE)

#### 6.2 biomass ####
# results_bm0 <- summary(bm_0)
# results_bm1 <- summary(bm_1)
# 
# results_bm0.s <- summary(bm_0.s)
# results_bm1.s <- summary(bm_1.s)
# 
# results_bm0.t <- summary(bm_0.t)
# results_bm1.t <- summary(bm_1.t)

results_bm0.nE <- summary(bm_0.nE)
results_bm1.nE <- summary(bm_1.nE)

results_bm0.s.nE <- summary(bm_0.s.nE)
results_bm1.s.nE <- summary(bm_1.s.nE)

results_bm0.t.nE <- summary(bm_0.t.nE)
results_bm1.t.nE <- summary(bm_1.t.nE)

#### 6.3 density ####
# results_dens0 <- summary(dens_0)
# results_dens1 <- summary(dens_1)
# 
# results_dens0.s <- summary(dens_0.s)
# results_dens1.s <- summary(dens_1.s)
# 
# results_dens0.t <- summary(dens_0.t)
# results_dens1.t <- summary(dens_1.t)

results_dens0.nE <- summary(dens_0.nE)
results_dens1.nE <- summary(dens_1.nE)

results_dens0.s.nE <- summary(dens_0.s.nE)
results_dens1.s.nE <- summary(dens_1.s.nE)

results_dens0.t.nE <- summary(dens_0.t.nE)
results_dens1.t.nE <- summary(dens_1.t.nE)

#### 6.4 combine results ####
results <- read_excel("results_gam.xlsx", col_types = c(rep("text", 4), rep("numeric",32)))

### 6.4.1 species ####
# results[1,5] <- results_ind0$p.coeff[2]
# results[1,6] <- results_ind0$se[2]
# results[1,7] <- results_ind0$p.t[2]
# results[1,8] <- results_ind0$p.pv[2]
# 
# results[2,5] <- results_ind1$p.coeff[2]
# results[2,6] <- results_ind1$se[2]
# results[2,7] <- results_ind1$p.t[2]
# results[2,8] <- results_ind1$p.pv[2]

results[7,5] <- results_ind0.nE$p.coeff[2]
results[7,6] <- results_ind0.nE$se[2]
results[7,7] <- results_ind0.nE$p.t[2]
results[7,8] <- results_ind0.nE$p.pv[2]

results[8,5] <- results_ind1.nE$p.coeff[2]
results[8,6] <- results_ind1.nE$se[2]
results[8,7] <- results_ind1.nE$p.t[2]
results[8,8] <- results_ind1.nE$p.pv[2]


# results[13,5] <- results_bm0$p.coeff[2]
# results[13,6] <- results_bm0$se[2]
# results[13,7] <- results_bm0$p.t[2]
# results[13,8] <- results_bm0$p.pv[2]
# 
# results[14,5] <- results_bm1$p.coeff[2]
# results[14,6] <- results_bm1$se[2]
# results[14,7] <- results_bm1$p.t[2]
# results[14,8] <- results_bm1$p.pv[2]

results[19,5] <- results_bm0.nE$p.coeff[2]
results[19,6] <- results_bm0.nE$se[2]
results[19,7] <- results_bm0.nE$p.t[2]
results[19,8] <- results_bm0.nE$p.pv[2]

results[20,5] <- results_bm1.nE$p.coeff[2]
results[20,6] <- results_bm1.nE$se[2]
results[20,7] <- results_bm1.nE$p.t[2]
results[20,8] <- results_bm1.nE$p.pv[2]


# results[25,5] <- results_dens0$p.coeff[2]
# results[25,6] <- results_dens0$se[2]
# results[25,7] <- results_dens0$p.t[2]
# results[25,8] <- results_dens0$p.pv[2]
# 
# results[26,5] <- results_dens1$p.coeff[2]
# results[26,6] <- results_dens1$se[2]
# results[26,7] <- results_dens1$p.t[2]
# results[26,8] <- results_dens1$p.pv[2]

results[31,5] <- results_dens0.nE$p.coeff[2]
results[31,6] <- results_dens0.nE$se[2]
results[31,7] <- results_dens0.nE$p.t[2]
results[31,8] <- results_dens0.nE$p.pv[2]

results[32,5] <- results_dens1.nE$p.coeff[2]
results[32,6] <- results_dens1.nE$se[2]
results[32,7] <- results_dens1.nE$p.t[2]
results[32,8] <- results_dens1.nE$p.pv[2]

### 6.4.2 year ####
# #Ebro
# results[1,9:12] <- as.list(results_ind0$s.table[1,])
# results[2,9:12] <- as.list(results_ind1$s.table[1,])
# 
# results[3,9:12] <- as.list(results_ind0.s$s.table[1,])
# results[4,9:12] <- as.list(results_ind1.s$s.table[1,])
# 
# results[5,9:12] <- as.list(results_ind0.t$s.table[1,])
# results[6,9:12] <- as.list(results_ind1.t$s.table[1,])

#no Ebro
results[7,9:12] <- as.list(results_ind0.nE$s.table[1,])
results[8,9:12] <- as.list(results_ind1.nE$s.table[1,])

results[9,9:12] <- as.list(results_ind0.s.nE$s.table[1,])
results[10,9:12] <- as.list(results_ind1.s.nE$s.table[1,])

results[11,9:12] <- as.list(results_ind0.t.nE$s.table[1,])
results[12,9:12] <- as.list(results_ind1.t.nE$s.table[1,])


# #Ebro
# results[13,9:12] <- as.list(results_bm0$s.table[1,])
# results[14,9:12] <- as.list(results_bm1$s.table[1,])
# 
# results[15,9:12] <- as.list(results_bm0.s$s.table[1,])
# results[16,9:12] <- as.list(results_bm1.s$s.table[1,])
# 
# results[17,9:12] <- as.list(results_bm0.t$s.table[1,])
# results[18,9:12] <- as.list(results_bm1.t$s.table[1,])

#no Ebro
results[19,9:12] <- as.list(results_bm0.nE$s.table[1,])
results[20,9:12] <- as.list(results_bm1.nE$s.table[1,])

results[21,9:12] <- as.list(results_bm0.s.nE$s.table[1,])
results[22,9:12] <- as.list(results_bm1.s.nE$s.table[1,])

results[23,9:12] <- as.list(results_bm0.t.nE$s.table[1,])
results[24,9:12] <- as.list(results_bm1.t.nE$s.table[1,])


# #Ebro
# results[25,9:12] <- as.list(results_dens0$s.table[1,])
# results[26,9:12] <- as.list(results_dens1$s.table[1,])
# 
# results[27,9:12] <- as.list(results_dens0.s$s.table[1,])
# results[28,9:12] <- as.list(results_dens1.s$s.table[1,])
# 
# results[29,9:12] <- as.list(results_dens0.t$s.table[1,])
# results[30,9:12] <- as.list(results_dens1.t$s.table[1,])

#no Ebro
results[31,9:12] <- as.list(results_dens0.nE$s.table[1,])
results[32,9:12] <- as.list(results_dens1.nE$s.table[1,])

results[33,9:12] <- as.list(results_dens0.s.nE$s.table[1,])
results[34,9:12] <- as.list(results_dens1.s.nE$s.table[1,])

results[35,9:12] <- as.list(results_dens0.t.nE$s.table[1,])
results[36,9:12] <- as.list(results_dens1.t.nE$s.table[1,])

### 6.4.3 northing ####
# #Ebro
# results[1,13:16] <- as.list(results_ind0$s.table[2,])
# results[2,13:16] <- as.list(results_ind1$s.table[2,])
# 
# results[3,13:16] <- as.list(results_ind0.s$s.table[2,])
# results[4,13:16] <- as.list(results_ind1.s$s.table[2,])
# 
# results[5,13:16] <- as.list(results_ind0.t$s.table[2,])
# results[6,13:16] <- as.list(results_ind1.t$s.table[2,])

#no Ebro
results[7,13:16] <- as.list(results_ind0.nE$s.table[2,])
results[8,13:16] <- as.list(results_ind1.nE$s.table[2,])

results[9,13:16] <- as.list(results_ind0.s.nE$s.table[2,])
results[10,13:16] <- as.list(results_ind1.s.nE$s.table[2,])

results[11,13:16] <- as.list(results_ind0.t.nE$s.table[2,])
results[12,13:16] <- as.list(results_ind1.t.nE$s.table[2,])


# #Ebro
# results[13,13:16] <- as.list(results_bm0$s.table[2,])
# results[14,13:16] <- as.list(results_bm1$s.table[2,])
# 
# results[15,13:16] <- as.list(results_bm0.s$s.table[2,])
# results[16,13:16] <- as.list(results_bm1.s$s.table[2,])
# 
# results[17,13:16] <- as.list(results_bm0.t$s.table[2,])
# results[18,13:16] <- as.list(results_bm1.t$s.table[2,])

#no Ebro
results[19,13:16] <- as.list(results_bm0.nE$s.table[2,])
results[20,13:16] <- as.list(results_bm1.nE$s.table[2,])

results[21,13:16] <- as.list(results_bm0.s.nE$s.table[2,])
results[22,13:16] <- as.list(results_bm1.s.nE$s.table[2,])

results[23,13:16] <- as.list(results_bm0.t.nE$s.table[2,])
results[24,13:16] <- as.list(results_bm1.t.nE$s.table[2,])


# #Ebro
# results[25,13:16] <- as.list(results_dens0$s.table[2,])
# results[26,13:16] <- as.list(results_dens1$s.table[2,])
# 
# results[27,13:16] <- as.list(results_dens0.s$s.table[2,])
# results[28,13:16] <- as.list(results_dens1.s$s.table[2,])
# 
# results[29,13:16] <- as.list(results_dens0.t$s.table[2,])
# results[30,13:16] <- as.list(results_dens1.t$s.table[2,])

#no Ebro
results[31,13:16] <- as.list(results_dens0.nE$s.table[2,])
results[32,13:16] <- as.list(results_dens1.nE$s.table[2,])

results[33,13:16] <- as.list(results_dens0.s.nE$s.table[2,])
results[34,13:16] <- as.list(results_dens1.s.nE$s.table[2,])

results[35,13:16] <- as.list(results_dens0.t.nE$s.table[2,])
results[36,13:16] <- as.list(results_dens1.t.nE$s.table[2,])

### 6.4.3 easting ####
# #Ebro
# results[1,17:20] <- as.list(results_ind0$s.table[3,])
# results[2,17:20] <- as.list(results_ind1$s.table[3,])
# 
# results[3,17:20] <- as.list(results_ind0.s$s.table[3,])
# results[4,17:20] <- as.list(results_ind1.s$s.table[3,])
# 
# results[5,17:20] <- as.list(results_ind0.t$s.table[3,])
# results[6,17:20] <- as.list(results_ind1.t$s.table[3,])

#no Ebro
results[7,17:20] <- as.list(results_ind0.nE$s.table[3,])
results[8,17:20] <- as.list(results_ind1.nE$s.table[3,])

results[9,17:20] <- as.list(results_ind0.s.nE$s.table[3,])
results[10,17:20] <- as.list(results_ind1.s.nE$s.table[3,])

results[11,17:20] <- as.list(results_ind0.t.nE$s.table[3,])
results[12,17:20] <- as.list(results_ind1.t.nE$s.table[3,])


# #Ebro
# results[13,17:20] <- as.list(results_bm0$s.table[3,])
# results[14,17:20] <- as.list(results_bm1$s.table[3,])
# 
# results[15,17:20] <- as.list(results_bm0.s$s.table[3,])
# results[16,17:20] <- as.list(results_bm1.s$s.table[3,])
# 
# results[17,17:20] <- as.list(results_bm0.t$s.table[3,])
# results[18,17:20] <- as.list(results_bm1.t$s.table[3,])

#no Ebro
results[19,17:20] <- as.list(results_bm0.nE$s.table[3,])
results[20,17:20] <- as.list(results_bm1.nE$s.table[3,])

results[21,17:20] <- as.list(results_bm0.s.nE$s.table[3,])
results[22,17:20] <- as.list(results_bm1.s.nE$s.table[3,])

results[23,17:20] <- as.list(results_bm0.t.nE$s.table[3,])
results[24,17:20] <- as.list(results_bm1.t.nE$s.table[3,])


# #Ebro
# results[25,17:20] <- as.list(results_dens0$s.table[3,])
# results[26,17:20] <- as.list(results_dens1$s.table[3,])
# 
# results[27,17:20] <- as.list(results_dens0.s$s.table[3,])
# results[28,17:20] <- as.list(results_dens1.s$s.table[3,])
# 
# results[29,17:20] <- as.list(results_dens0.t$s.table[3,])
# results[30,17:20] <- as.list(results_dens1.t$s.table[3,])

#no Ebro
results[31,17:20] <- as.list(results_dens0.nE$s.table[3,])
results[32,17:20] <- as.list(results_dens1.nE$s.table[3,])

results[33,17:20] <- as.list(results_dens0.s.nE$s.table[3,])
results[34,17:20] <- as.list(results_dens1.s.nE$s.table[3,])

results[35,17:20] <- as.list(results_dens0.t.nE$s.table[3,])
results[36,17:20] <- as.list(results_dens1.t.nE$s.table[3,])

### 6.4.4 elevation ####
# #Ebro
# results[1,21:24] <- as.list(results_ind0$s.table[4,])
# results[2,21:24] <- as.list(results_ind1$s.table[4,])
# 
# results[3,21:24] <- as.list(results_ind0.s$s.table[4,])
# results[4,21:24] <- as.list(results_ind1.s$s.table[4,])
# 
# results[5,21:24] <- as.list(results_ind0.t$s.table[4,])
# results[6,21:24] <- as.list(results_ind1.t$s.table[4,])

#no Ebro
results[7,21:24] <- as.list(results_ind0.nE$s.table[4,])
results[8,21:24] <- as.list(results_ind1.nE$s.table[4,])

results[9,21:24] <- as.list(results_ind0.s.nE$s.table[4,])
results[10,21:24] <- as.list(results_ind1.s.nE$s.table[4,])

results[11,21:24] <- as.list(results_ind0.t.nE$s.table[4,])
results[12,21:24] <- as.list(results_ind1.t.nE$s.table[4,])


# #Ebro
# results[13,21:24] <- as.list(results_bm0$s.table[4,])
# results[14,21:24] <- as.list(results_bm1$s.table[4,])
# 
# results[15,21:24] <- as.list(results_bm0.s$s.table[4,])
# results[16,21:24] <- as.list(results_bm1.s$s.table[4,])
# 
# results[17,21:24] <- as.list(results_bm0.t$s.table[4,])
# results[18,21:24] <- as.list(results_bm1.t$s.table[4,])

#no Ebro
results[19,21:24] <- as.list(results_bm0.nE$s.table[4,])
results[20,21:24] <- as.list(results_bm1.nE$s.table[4,])

results[21,21:24] <- as.list(results_bm0.s.nE$s.table[4,])
results[22,21:24] <- as.list(results_bm1.s.nE$s.table[4,])

results[23,21:24] <- as.list(results_bm0.t.nE$s.table[4,])
results[24,21:24] <- as.list(results_bm1.t.nE$s.table[4,])


# #Ebro
# results[25,21:24] <- as.list(results_dens0$s.table[4,])
# results[26,21:24] <- as.list(results_dens1$s.table[4,])
# 
# results[27,21:24] <- as.list(results_dens0.s$s.table[4,])
# results[28,21:24] <- as.list(results_dens1.s$s.table[4,])
# 
# results[29,21:24] <- as.list(results_dens0.t$s.table[4,])
# results[30,21:24] <- as.list(results_dens1.t$s.table[4,])

#no Ebro
results[31,21:24] <- as.list(results_dens0.nE$s.table[4,])
results[32,21:24] <- as.list(results_dens1.nE$s.table[4,])

results[33,21:24] <- as.list(results_dens0.s.nE$s.table[4,])
results[34,21:24] <- as.list(results_dens1.s.nE$s.table[4,])

results[35,21:24] <- as.list(results_dens0.t.nE$s.table[4,])
results[36,21:24] <- as.list(results_dens1.t.nE$s.table[4,])

### 6.4.5 NAO ####
# #Ebro
# results[1,25:28] <- as.list(results_ind0$s.table[5,])
# results[2,25:28] <- as.list(results_ind1$s.table[5,])
# 
# results[3,25:28] <- as.list(results_ind0.s$s.table[5,])
# results[4,25:28] <- as.list(results_ind1.s$s.table[5,])
# 
# results[5,25:28] <- as.list(results_ind0.t$s.table[5,])
# results[6,25:28] <- as.list(results_ind1.t$s.table[5,])

#no Ebro
results[7,25:28] <- as.list(results_ind0.nE$s.table[5,])
results[8,25:28] <- as.list(results_ind1.nE$s.table[5,])

results[9,25:28] <- as.list(results_ind0.s.nE$s.table[5,])
results[10,25:28] <- as.list(results_ind1.s.nE$s.table[5,])

results[11,25:28] <- as.list(results_ind0.t.nE$s.table[5,])
results[12,25:28] <- as.list(results_ind1.t.nE$s.table[5,])


# #Ebro
# results[13,25:28] <- as.list(results_bm0$s.table[5,])
# results[14,25:28] <- as.list(results_bm1$s.table[5,])
# 
# results[15,25:28] <- as.list(results_bm0.s$s.table[5,])
# results[16,25:28] <- as.list(results_bm1.s$s.table[5,])
# 
# results[17,25:28] <- as.list(results_bm0.t$s.table[5,])
# results[18,25:28] <- as.list(results_bm1.t$s.table[5,])

#no Ebro
results[19,25:28] <- as.list(results_bm0.nE$s.table[5,])
results[20,25:28] <- as.list(results_bm1.nE$s.table[5,])

results[21,25:28] <- as.list(results_bm0.s.nE$s.table[5,])
results[22,25:28] <- as.list(results_bm1.s.nE$s.table[5,])

results[23,25:28] <- as.list(results_bm0.t.nE$s.table[5,])
results[24,25:28] <- as.list(results_bm1.t.nE$s.table[5,])


# #Ebro
# results[25,25:28] <- as.list(results_dens0$s.table[5,])
# results[26,25:28] <- as.list(results_dens1$s.table[5,])
# 
# results[27,25:28] <- as.list(results_dens0.s$s.table[5,])
# results[28,25:28] <- as.list(results_dens1.s$s.table[5,])
# 
# results[29,25:28] <- as.list(results_dens0.t$s.table[5,])
# results[30,25:28] <- as.list(results_dens1.t$s.table[5,])

#no Ebro
results[31,25:28] <- as.list(results_dens0.nE$s.table[5,])
results[32,25:28] <- as.list(results_dens1.nE$s.table[5,])

results[33,25:28] <- as.list(results_dens0.s.nE$s.table[5,])
results[34,25:28] <- as.list(results_dens1.s.nE$s.table[5,])

results[35,25:28] <- as.list(results_dens0.t.nE$s.table[5,])
results[36,25:28] <- as.list(results_dens1.t.nE$s.table[5,])


### 6.4.6 stocking ratio ####
# #Ebro
# results[1,29:32] <- as.list(results_ind0$s.table[7,])
# results[2,29:32] <- as.list(results_ind1$s.table[7,])
# 
# results[3,29:32] <- as.list(results_ind0.s$s.table[7,])
# results[4,29:32] <- as.list(results_ind1.s$s.table[7,])
# 
# results[5,29:32] <- as.list(results_ind0.t$s.table[7,])
# results[6,29:32] <- as.list(results_ind1.t$s.table[7,])

#no Ebro
results[7,29:32] <- as.list(results_ind0.nE$s.table[6,])
results[8,29:32] <- as.list(results_ind1.nE$s.table[6,])

results[9,29:32] <- as.list(results_ind0.s.nE$s.table[6,])
results[10,29:32] <- as.list(results_ind1.s.nE$s.table[6,])

results[11,29:32] <- as.list(results_ind0.t.nE$s.table[6,])
results[12,29:32] <- as.list(results_ind1.t.nE$s.table[6,])


# #Ebro
# results[13,29:32] <- as.list(results_bm0$s.table[6,])
# results[14,29:32] <- as.list(results_bm1$s.table[6,])
# 
# results[15,29:32] <- as.list(results_bm0.s$s.table[6,])
# results[16,29:32] <- as.list(results_bm1.s$s.table[6,])
# 
# results[17,29:32] <- as.list(results_bm0.t$s.table[6,])
# results[18,29:32] <- as.list(results_bm1.t$s.table[6,])

#no Ebro
results[19,29:32] <- as.list(results_bm0.nE$s.table[6,])
results[20,29:32] <- as.list(results_bm1.nE$s.table[6,])

results[21,29:32] <- as.list(results_bm0.s.nE$s.table[6,])
results[22,29:32] <- as.list(results_bm1.s.nE$s.table[6,])

results[23,29:32] <- as.list(results_bm0.t.nE$s.table[6,])
results[24,29:32] <- as.list(results_bm1.t.nE$s.table[6,])


# #Ebro
# results[25,29:32] <- as.list(results_dens0$s.table[6,])
# results[26,29:32] <- as.list(results_dens1$s.table[6,])
# 
# results[27,29:32] <- as.list(results_dens0.s$s.table[6,])
# results[28,29:32] <- as.list(results_dens1.s$s.table[6,])
# 
# results[29,29:32] <- as.list(results_dens0.t$s.table[6,])
# results[30,29:32] <- as.list(results_dens1.t$s.table[6,])

#no Ebro
results[31,29:32] <- as.list(results_dens0.nE$s.table[6,])
results[32,29:32] <- as.list(results_dens1.nE$s.table[6,])

results[33,29:32] <- as.list(results_dens0.s.nE$s.table[6,])
results[34,29:32] <- as.list(results_dens1.s.nE$s.table[6,])

results[35,29:32] <- as.list(results_dens0.t.nE$s.table[6,])
results[36,29:32] <- as.list(results_dens1.t.nE$s.table[6,])

### 6.4.7 river ####
# #Ebro
# results[1,33:36] <- as.list(results_ind0$s.table[7,])
# results[2,33:36] <- as.list(results_ind1$s.table[7,])
# 
# results[3,33:36] <- as.list(results_ind0.s$s.table[7,])
# results[4,33:36] <- as.list(results_ind1.s$s.table[7,])
# 
# results[5,33:36] <- as.list(results_ind0.t$s.table[7,])
# results[6,33:36] <- as.list(results_ind1.t$s.table[7,])

#no Ebro
results[7,33:36] <- as.list(results_ind0.nE$s.table[7,])
results[8,33:36] <- as.list(results_ind1.nE$s.table[7,])

results[9,33:36] <- as.list(results_ind0.s.nE$s.table[7,])
results[10,33:36] <- as.list(results_ind1.s.nE$s.table[7,])

results[11,33:36] <- as.list(results_ind0.t.nE$s.table[7,])
results[12,33:36] <- as.list(results_ind1.t.nE$s.table[7,])


# #Ebro
# results[13,33:36] <- as.list(results_bm0$s.table[7,])
# results[14,33:36] <- as.list(results_bm1$s.table[7,])
# 
# results[15,33:36] <- as.list(results_bm0.s$s.table[7,])
# results[16,33:36] <- as.list(results_bm1.s$s.table[7,])
# 
# results[17,33:36] <- as.list(results_bm0.t$s.table[7,])
# results[18,33:36] <- as.list(results_bm1.t$s.table[7,])

#no Ebro
results[19,33:36] <- as.list(results_bm0.nE$s.table[7,])
results[20,33:36] <- as.list(results_bm1.nE$s.table[7,])

results[21,33:36] <- as.list(results_bm0.s.nE$s.table[7,])
results[22,33:36] <- as.list(results_bm1.s.nE$s.table[7,])

results[23,33:36] <- as.list(results_bm0.t.nE$s.table[7,])
results[24,33:36] <- as.list(results_bm1.t.nE$s.table[7,])


# #Ebro
# results[25,33:36] <- as.list(results_dens0$s.table[7,])
# results[26,33:36] <- as.list(results_dens1$s.table[7,])
# 
# results[27,33:36] <- as.list(results_dens0.s$s.table[7,])
# results[28,33:36] <- as.list(results_dens1.s$s.table[7,])
# 
# results[29,33:36] <- as.list(results_dens0.t$s.table[7,])
# results[30,33:36] <- as.list(results_dens1.t$s.table[7,])

#no Ebro
results[31,33:36] <- as.list(results_dens0.nE$s.table[7,])
results[32,33:36] <- as.list(results_dens1.nE$s.table[7,])

results[33,33:36] <- as.list(results_dens0.s.nE$s.table[7,])
results[34,33:36] <- as.list(results_dens1.s.nE$s.table[7,])

results[35,33:36] <- as.list(results_dens0.t.nE$s.table[7,])
results[36,33:36] <- as.list(results_dens1.t.nE$s.table[7,])

#### 6.5 round results ####
results[,c(5:7, 9:11, 13:15, 17:19, 21:23, 25:27, 29:31, 33:35)] <- round(results[,c(5:7, 9:11, 13:15, 17:19, 21:23, 25:27, 29:31, 33:35)], 4)
write.xlsx(results, "results_gam_filled.xlsx")

#### 6.6 change p-values ####
# final_results <- read_excel("results_gam_filled_no Ebro.xlsx")
final_results <- results[results$Ebro=="n",]
final_results <- final_results[,-4]
final_results[,4:35] <- round(final_results[,4:35],2)

final_results$species_p[final_results$species_p<0.001] <- "< 0.001"
final_results$species_p[final_results$species_p<0.01 & final_results$species_p>=0.001] <- "< 0.01"
final_results$species_p[final_results$species_p<0.05 & final_results$species_p>=0.01] <- "< 0.05"

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

write.xlsx(final_results, "results gam final.xlsx")
