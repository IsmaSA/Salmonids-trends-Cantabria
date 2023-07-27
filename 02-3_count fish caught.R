library(dplyr)
years <- levels(as.factor(fish$year))
years <- as.numeric(rep(years, each=2*4))
spec <- rep(c("Salmo salar", "Salmo trutta fario"), each=4, 10)
age <- rep(c("0+", "1+", "adult", "total"), 2*10)
fish.summary <- data.frame(cbind(years, spec, age))

##0+
fish.summary$total <- NA
fish.summary$total[fish.summary$age=="0+"] <- mapply(function(spec, year) {
  sum(fish$ind.0[fish$year==year & fish$species==spec], na.rm=T)},
  spec=fish.summary$spec[fish.summary$age=="0+"], year=fish.summary$years[fish.summary$age=="0+"])

fish.summary$stocked <- NA
fish.summary$stocked[fish.summary$age=="0+"] <- mapply(function(spec, year) {
  sum(fish$adip.0[fish$year==year & fish$species==spec], na.rm=T)},
  spec=fish.summary$spec[fish.summary$age=="0+"], year=fish.summary$years[fish.summary$age=="0+"])

##1+
fish.summary$total[fish.summary$age=="1+"] <- mapply(function(spec, year) {
  sum(fish$ind.1[fish$year==year & fish$species==spec], na.rm=T)},
  spec=fish.summary$spec[fish.summary$age=="1+"], year=fish.summary$years[fish.summary$age=="1+"])

fish.summary$stocked[fish.summary$age=="1+"] <- mapply(function(spec, year) {
  sum(fish$adip.1[fish$year==year & fish$species==spec], na.rm=T)},
  spec=fish.summary$spec[fish.summary$age=="1+"], year=fish.summary$years[fish.summary$age=="1+"])

##adult
fish.summary$total[fish.summary$age=="adult"] <- mapply(function(spec, year) {
  sum(fish$ind.adult[fish$year==year & fish$species==spec], na.rm=T)},
  spec=fish.summary$spec[fish.summary$age=="adult"], year=fish.summary$years[fish.summary$age=="adult"])

fish.summary$stocked[fish.summary$age=="adult"] <- mapply(function(spec, year) {
  sum(fish$adip.adult[fish$year==year & fish$species==spec], na.rm=T)},
  spec=fish.summary$spec[fish.summary$age=="adult"], year=fish.summary$years[fish.summary$age=="adult"])

##total
fish.summary$total[fish.summary$age=="total"] <- mapply(function(spec, year) {
  sum(fish$ind[fish$year==year & fish$species==spec], na.rm=T)},
  spec=fish.summary$spec[fish.summary$age=="total"], year=fish.summary$years[fish.summary$age=="total"])

fish.summary$stocked[fish.summary$age=="total"] <- mapply(function(spec, year) {
  sum(fish$adip[fish$year==year & fish$species==spec], na.rm=T)},
  spec=fish.summary$spec[fish.summary$age=="total"], year=fish.summary$years[fish.summary$age=="total"])

### native
fish.summary$native <- fish.summary$total - fish.summary$stocked

### percentages
fish.summary$stocked.p <- round(fish.summary$stocked / fish.summary$total * 100, 2)
fish.summary$native.p <- round(fish.summary$native / fish.summary$total * 100, 2)

library(openxlsx)
write.xlsx(fish.summary, "number of individuals per year.xlsx")


#### overall
spec <- rep(c("Salmo salar", "Salmo trutta fario"), each=4)
age <- rep(c("0+", "1+", "adult", "total"), 2)
fish.summary <- data.frame(cbind(spec, age))

##0+
fish.summary$total <- NA
fish.summary$total[fish.summary$age=="0+"] <- mapply(function(spec) {
  sum(fish$ind.0[fish$species==spec], na.rm=T)},
  spec=fish.summary$spec[fish.summary$age=="0+"])

fish.summary$stocked <- NA
fish.summary$stocked[fish.summary$age=="0+"] <- mapply(function(spec) {
  sum(fish$adip.0[fish$species==spec], na.rm=T)},
  spec=fish.summary$spec[fish.summary$age=="0+"])

##1+
fish.summary$total[fish.summary$age=="1+"] <- mapply(function(spec) {
  sum(fish$ind.1[fish$species==spec], na.rm=T)},
  spec=fish.summary$spec[fish.summary$age=="1+"])

fish.summary$stocked[fish.summary$age=="1+"] <- mapply(function(spec) {
  sum(fish$adip.1[fish$species==spec], na.rm=T)},
  spec=fish.summary$spec[fish.summary$age=="1+"])

##adult
fish.summary$total[fish.summary$age=="adult"] <- mapply(function(spec) {
  sum(fish$ind.adult[fish$species==spec], na.rm=T)},
  spec=fish.summary$spec[fish.summary$age=="adult"])

fish.summary$stocked[fish.summary$age=="adult"] <- mapply(function(spec) {
  sum(fish$adip.adult[fish$species==spec], na.rm=T)},
  spec=fish.summary$spec[fish.summary$age=="adult"])

##total
fish.summary$total[fish.summary$age=="total"] <- mapply(function(spec) {
  sum(fish$ind[fish$species==spec], na.rm=T)},
  spec=fish.summary$spec[fish.summary$age=="total"])

fish.summary$stocked[fish.summary$age=="total"] <- mapply(function(spec) {
  sum(fish$adip[fish$species==spec], na.rm=T)},
  spec=fish.summary$spec[fish.summary$age=="total"])

### native
fish.summary$native <- fish.summary$total - fish.summary$stocked

### percentages
fish.summary$stocked.p <- round(fish.summary$stocked / fish.summary$total * 100, 2)
fish.summary$native.p <- round(fish.summary$native / fish.summary$total * 100, 2)


write.xlsx(fish.summary, "number of individuals.xlsx")
