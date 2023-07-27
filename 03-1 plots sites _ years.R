library(ggplot2)

sites.years <- ggplot(data=samplings, aes(year))
sitesy <- sites.years +
  geom_bar() +
  scale_x_continuous(breaks=seq(2010,2019,by=1)) +
  stat_count(geom = "text", colour = "white", size = 3.5,aes(label = ..count..),position=position_stack(vjust=0.5)) +
  theme_bw()+
  theme(axis.text.x = element_text(angle=45, hjust=1)) +
  labs(x="Year", y="Count", title = "Number of Sites sampled per Year")

sites.times <- ggplot(data=sites, aes(no.years))
sitests <- sites.times +
  geom_bar() +
  scale_x_continuous(breaks=seq(1,10,by=1)) +
  stat_count(geom = "text", colour = "white", size = 3.5,aes(label = ..count..),position=position_stack(vjust=0.5)) +
  theme_bw()+
  labs(x="Number of Years Sampled", y="Count", title = "Number of Sites sampled for x Years")

northing.years <- ggplot(samplings, aes(year, WGSn, group=year))
northy <- northing.years +
  geom_boxplot() +
  geom_point(color="grey") +
  geom_smooth(method = "lm", color="red", aes(group=1)) +
  scale_x_continuous(breaks=seq(2010,2019,by=1)) +
  theme_bw()+
  theme(axis.text.x = element_text(angle=45, hjust=1)) +
  labs(x="Year", y="WGS84 Northing", title = "WGS84 Northing across the Years")

easting.years <- ggplot(samplings, aes(year, WGSe, group=year))
easty <- easting.years +
  geom_boxplot() +
  geom_point(color="grey") +
  geom_smooth(method = "lm", color="red", aes(group=1)) +
  scale_x_continuous(breaks=seq(2010,2019,by=1)) +
  theme_bw()+
  theme(axis.text.x = element_text(angle=45, hjust=1)) +
  labs(x="Year", y="WGS84  Easting", title = "WGS84 Easting across the Years")

elev.years <- ggplot(samplings, aes(year, elevation, group=year))
elevy <- elev.years +
  geom_boxplot() +
  geom_point(color="grey") +
  geom_smooth(method = "lm", color="red", aes(group=1)) +
  scale_x_continuous(breaks=seq(2010,2019,by=1)) +
  theme_bw()+
  theme(axis.text.x = element_text(angle=45, hjust=1)) +
  labs(x="Year", y="Elevation", title = "Elevation across the Years")

library(gridExtra)
svg(filename="plots sites.svg")
grid.arrange(sitesy, sitests, northy, easty, elevy, nrow=3)
dev.off()

northing.ts <- ggplot(sites, aes(no.years, WGSn, group=no.years))
northy <- northing.ts +
  geom_boxplot() +
  geom_point(color="grey") +
  geom_smooth(method = "lm", color="red", aes(group=1)) +
  scale_x_continuous(breaks=seq(1,10,by=1)) +
  theme_bw()+
  labs(x="Number of Years Sampled", y="WGS84 Northing", title = "WGS84 Northing across the Timeseries")

easting.ts <- ggplot(sites, aes(no.years, WGSe, group=no.years))
easty <- easting.ts +
  geom_boxplot() +
  geom_point(color="grey") +
  geom_smooth(method = "lm", color="red", aes(group=1)) +
  scale_x_continuous(breaks=seq(1,10,by=1)) +
  theme_bw()+
  labs(x="Year", y="WGS84  Easting", title = "WGS84 Easting across the Timeseries")

elev.ts <- ggplot(sites, aes(no.years, elevation, group=no.years))
elevy <- elev.ts +
  geom_boxplot() +
  geom_point(color="grey") +
  geom_smooth(method = "lm", color="red", aes(group=1)) +
  scale_x_continuous(breaks=seq(1,10,by=1)) +
  theme_bw()+
  labs(x="Year", y="Elevation", title = "Elevation across the Timeseries")

grid.arrange(northy, easty, elevy, nrow=3)
