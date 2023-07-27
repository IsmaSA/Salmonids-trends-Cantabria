##### 01. SETUP #########
# subsetting and preparing Breitenbach datasets for the analysis#
#package  installation
pckg <- installed.packages()
req.pkg <- c("readxl", "openxlsx","ggplot2")
inst.pkg <- req.pkg[!(req.pkg %in% pckg[,1])]
install.packages(inst.pkg)
lapply(req.pkg, library, character.only=T)
rm(inst.pkg,req.pkg, pckg)

##### 02. DATA IMPORT, CLEANUP AND PREPARATION #########
fish <- read_excel("raw data.xlsx", col_types = c("skip", "skip", "date", "numeric", "numeric", "numeric", "text",
                                                  "text", "numeric", "numeric", "numeric", "numeric", "numeric",
                                                  "numeric", "numeric", "numeric", "numeric", "numeric", "numeric",
                                                  "numeric", "numeric", "numeric", "numeric", "numeric", "numeric"))

# replace -1 in environmental data with NA
fish[,19:23][fish[,19:23] == -1] <- NA

# rename columns to easier names
colnames(fish) <- c("date", "year", "UTM.X", "UTM.Y", "river", "species", "ind", "dens", "bm", "ind.0", "dens.0", "bm.0",
                    "ind.1", "dens.1", "bm.1", "ind.adult", "dens.adult", "bm.adult", "Twater", "pH", "conductivity", "pc.O2", "O2")

# fix obvious errors
summary(fish)
which.max(fish$Twater) #value of 43511
fish$Twater[801] <-NA

which.max(fish$conductivity) #value of 3520
fish$conductivity[235] <-NA

which.max(fish$O2) #value of 43593
fish$O2[799] <-NA

#### 2.1 store fixed data #####
write.xlsx(fish, "fixed data.xlsx")

##### 03. PLOT #########
#### 3.1 density ####
dens <- ggplot(fish, aes(year, dens, colour=species))
dens +
  geom_point() +
  geom_smooth(method="loess", aes(fill=species), alpha=0.1) +
  scale_x_continuous(breaks=seq(2010,2019,by=1)) +
  labs(x="Year", y="Density [ind/100m]", title = "Total fish density per year")

dens0 <- ggplot(fish, aes(year, dens.0, colour=species))
dens0 +
  geom_point() +
  geom_smooth(method="loess", aes(fill=species), alpha=0.1) +
  scale_x_continuous(breaks=seq(2010,2019,by=1)) +
  labs(x="Year", y="Density [ind/100m]", title = "Fish density of age class 0+ per year")

dens1 <- ggplot(fish, aes(year, dens.1, colour=species))
dens1 +
  geom_point() +
  geom_smooth(method="loess", aes(fill=species), alpha=0.1) +
  scale_x_continuous(breaks=seq(2010,2019,by=1)) +
  # ylim(c(0,1500)) +
  labs(x="Year", y="Density [ind/100m]", title = "Fish density of age class 1+ per year")

densad <- ggplot(fish, aes(year, dens.adult, colour=species))
densad +
  geom_point() +
  geom_smooth(method="loess", aes(fill=species), alpha=0.1) +
  scale_x_continuous(breaks=seq(2010,2019,by=1)) +
  # ylim(c(0,1500)) +
  labs(x="Year", y="Density [ind/100m]", title = "Fish density of adult fish per year")

#### 3.2 biomass ####
bm <- ggplot(fish, aes(year, bm, colour=species))
bm +
  geom_point() +
  geom_smooth(method="lm", aes(fill=species), alpha=0.1) +
  scale_x_continuous(breaks=seq(2010,2019,by=1)) +
  labs(x="Year", y="Biomass [kg/100m]", title = "Total fish biomass per year")

bm0 <- ggplot(fish, aes(year, bm.0, colour=species))
bm0 +
  geom_point() +
  geom_smooth(method="lm", aes(fill=species), alpha=0.1) +
  scale_x_continuous(breaks=seq(2010,2019,by=1)) +
  labs(x="Year", y="Biomass [kg/100m]", title = "Fish biomass of age class 0+ per year")

bm1 <- ggplot(fish, aes(year, bm.1, colour=species))
bm1 +
  geom_point() +
  geom_smooth(method="lm", aes(fill=species), alpha=0.1) +
  scale_x_continuous(breaks=seq(2010,2019,by=1)) +
  labs(x="Year", y="Biomass [kg/100m]", title = "Fish biomass of age class 1+ per year")

bmad <- ggplot(fish, aes(year, bm.adult, colour=species))
bmad +
  geom_point() +
  geom_smooth(method="lm", aes(fill=species), alpha=0.1) +
  scale_x_continuous(breaks=seq(2010,2019,by=1)) +
  labs(x="Year", y="Biomass [kg/100m]", title = "Fish biomass of adult fish per year")

#### 3.3 environmental data ######
temp <- ggplot(fish, aes(year, Twater))
temp +
  geom_point() +
  geom_smooth(method="lm", alpha=0.1) +
  scale_x_continuous(breaks=seq(2010,2019,by=1)) +
  labs(x="Year", y="Water temperature [°C]")

pH <- ggplot(fish, aes(year, pH))
pH +
  geom_point() +
  geom_smooth(method="lm", alpha=0.1) +
  scale_x_continuous(breaks=seq(2010,2019,by=1)) +
  labs(x="Year", y="pH")
 
cond <- ggplot(fish, aes(year, conductivity))
cond +
  geom_point() +
  geom_smooth(method="lm", alpha=0.1) +
  scale_x_continuous(breaks=seq(2010,2019,by=1)) +
  labs(x="Year", y="conductivity")

pcO <- ggplot(fish, aes(year, pc.O2))
pcO +
  geom_point() +
  geom_smooth(method="lm", alpha=0.1) +
  scale_x_continuous(breaks=seq(2010,2019,by=1)) +
  labs(x="Year", y="% O2")

O2 <- ggplot(fish, aes(year, O2))
O2 +
  geom_point() +
  geom_smooth(method="lm", alpha=0.1) +
  scale_x_continuous(breaks=seq(2010,2019,by=1)) +
  labs(x="Year", y="O2")
