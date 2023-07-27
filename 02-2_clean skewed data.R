## data is very skewed ##
## throw out any sites that were only sampled in 2010, 2018, 2019 or a combination of these ##

#package  installation
pckg <- installed.packages()
req.pkg <- c("readxl", "dplyr", "ggplot2", "openxlsx")
inst.pkg <- req.pkg[!(req.pkg %in% pckg[,1])]
install.packages(inst.pkg)
lapply(req.pkg, library, character.only=T)
rm(inst.pkg,req.pkg, pckg)

###
`%!in%` = Negate(`%in%`)
###
fish <- read_excel("data from captures.xlsx")
colnames(fish) <- c("date", "year", "species", "siteID", "effort", "no.years", "ID", "month", "river", "length", "width", "area", "WGSn", "WGSe", "elevation",
                    "Tair", "Twater", "pH", "conductivity", "pc.O2", "O2", "NAO", "adip.0", "adip.1", "adip.adult", "adip",
                    "field_weight.0", "field_weight.1", "field_weight.adult", "field.weight", "weight.0", "weight.1", "weight.adult",
                    "weight", "ind.0", "ind.1", "ind.adult", "ind", "ratio.0", "ratio.1", "ratio.adult", "ratio", "bm.0", "bm.1",
                    "bm.adult", "bm", "dens.0", "dens.1", "dens.adult", "dens")

fish$year <- as.numeric(fish$year)
sites <- fish %>% group_by(year, siteID, ID, river, WGSn, WGSe, elevation) %>% summarise()
sites <- sites[order(sites$year, sites$siteID),]

sites.unique <- unique(sites[,2])
sites.unique <- data.frame(sites.unique[order(sites.unique$siteID),])
sites.unique$WGSn <- lapply(sites.unique$siteID, function(i) round(mean(sites$WGSn[sites$siteID==i], na.rm=T),6))
sites.unique$WGSe <- lapply(sites.unique$siteID, function(i) round(mean(sites$WGSe[sites$siteID==i], na.rm=T),6))
sites.unique$elevation <- lapply(sites.unique$siteID, function(i) round(mean(sites$elevation[sites$siteID==i], na.rm=T),6))
sites.unique$years <- lapply(sites.unique$siteID, function(i) sites$year[sites$siteID==i])
sites.unique$no.years <- lapply(sites.unique$siteID, function(i) length(sites$year[sites$siteID==i]))

## drop single 2010, 2018 and 2019
sites.cleaned <- sites.unique[sites.unique$no.years>=2 | sites.unique$year %in% c(2011,2012,2013,2014,2015,2016,2017),]

## drop 2010 & 2018 / 2010 & 2019 / 2018 & 2019 / 2010,2018,2019
wa.sites <- sites.cleaned
wa.sites$y1 <- sapply(wa.sites$years, function(i) unlist(i)[1])
wa.sites$y2 <- sapply(wa.sites$years, function(i) unlist(i)[2])
#2010,2018 & 2010,2018,2019
wa.sites$check <- wa.sites$y1==2010 & wa.sites$y2==2018
wa.sites <- wa.sites[!wa.sites$check,]
#2018,2019
wa.sites$check <- wa.sites$y1==2018 & wa.sites$y2==2019
wa.sites <- wa.sites[!wa.sites$check,]

sites.cleaned <- sites.cleaned[sites.cleaned$siteID %in% wa.sites$siteID,]
samplings.cleaned <- sites[sites$siteID %in% wa.sites$siteID,]
rm(wa.sites)

sites.years <- ggplot(data=sites, aes(year))
sitesy.all <- sites.years +
  geom_bar() +
  scale_x_continuous(breaks=seq(2010,2019,by=1)) +
  stat_count(geom = "text", colour = "white", size = 3.5,aes(label = ..count..),position=position_stack(vjust=0.5)) +
  theme_bw()+
  labs(x="Year", y="Count", title = "Number of Sites sampled per Year")
sitesy.all

sites.years <- ggplot(data=samplings.cleaned, aes(year))
sitesy <- sites.years +
  geom_bar() +
  scale_x_continuous(breaks=seq(2010,2019,by=1)) +
  stat_count(geom = "text", colour = "white", size = 3.5,aes(label = ..count..),position=position_stack(vjust=0.5)) +
  theme_bw()+
  labs(x="Year", y="Count", title = "Number of Sites sampled per Year")
sitesy

sites.years <- ggplot(data=samplings.cleaned[samplings.cleaned$siteID %!in% c(18,24,29,34,44,48,55,64,87,95,126,127,128,143,144,145,146,148,155,157,164),], aes(year))
sitesy.res <- sites.years +
  geom_bar() +
  scale_x_continuous(breaks=seq(2010,2019,by=1)) +
  stat_count(geom = "text", colour = "white", size = 3.5,aes(label = ..count..),position=position_stack(vjust=0.5)) +
  theme_bw()+
  labs(x="Year", y="Count", title = "Number of Sites sampled per Year")
sitesy.res

fish.restricted <- fish[fish$siteID %in% sites.cleaned$siteID,]
fish.restricted.more <- fish.restricted[fish.restricted$siteID %!in% c(18,24,29,34,44,48,55,64,87,95,126,127,128,143,144,145,146,148,155,157,164),] #drop combinations of 2010,2011,2012,2018,2019

## update & fix effort ##
fish.restricted$effort <- sapply(fish.restricted$year, function(i) length(unique(fish.restricted$siteID[fish.restricted$year==i])))
fish.restricted.more$effort <- sapply(fish.restricted.more$year, function(i) length(unique(fish.restricted.more$siteID[fish.restricted.more$year==i])))

## export files ##
write.xlsx(fish.restricted, "data from captures_restricted.xlsx")
write.xlsx(fish.restricted.more, "data from captures_restricted more.xlsx")

write.xlsx(sites.cleaned, "sites_restricted.xlsx")
write.xlsx(samplings.cleaned, "samplings_restricted.xlsx")

sites.cleaned.more <- sites.cleaned[sites.cleaned$siteID %!in% c(18,24,29,34,44,48,55,64,87,95,126,127,128,143,144,145,146,148,155,157,164),]
samplings.cleaned.more <- samplings.cleaned[samplings.cleaned$siteID %!in% c(18,24,29,34,44,48,55,64,87,95,126,127,128,143,144,145,146,148,155,157,164),]

write.xlsx(sites.cleaned.more, "sites_restricted more.xlsx")
write.xlsx(samplings.cleaned.more, "samplings_restricted more.xlsx")
