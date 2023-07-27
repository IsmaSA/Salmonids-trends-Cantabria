#### year #######
library(ggplot2)
year.ss0 <- ggplot(fish[fish$species=="Salmo salar",], aes(year, ratio.0))
salmon0year <- year.ss0 +
  theme_bw() +
  geom_point() +
  geom_smooth(method="gam") +
  scale_x_continuous(breaks=seq(2010,2019,by=1)) +
  scale_y_continuous(breaks=seq(0,1,by=0.25)) +
  labs(x="Year", y="Stocking Ratio", title = "Salmo salar 0+")

year.ss1 <- ggplot(fish[fish$species=="Salmo salar",], aes(year, ratio.1))
salmon1year <- year.ss1 +
  theme_bw() +
  geom_point() +
  geom_smooth(method="gam") +
  scale_x_continuous(breaks=seq(2010,2019,by=1)) +
  scale_y_continuous(breaks=seq(0,1,by=0.25)) +
  labs(x="Year", y="Stocking Ratio", title = "Salmo salar 1+")

year.stf0 <- ggplot(fish[fish$species=="Salmo trutta fario",], aes(year, ratio.0))
trout0year <- year.stf0 +
  theme_bw() +
  geom_point() +
  geom_smooth(method="gam") +
  scale_x_continuous(breaks=seq(2010,2019,by=1)) +
  scale_y_continuous(breaks=seq(0,1,by=0.25)) +
  labs(x="Year", y="Stocking Ratio", title = "Salmo trutta fario 0+")

year.stf1 <- ggplot(fish[fish$species=="Salmo trutta fario",], aes(year, ratio.1))
trout1year <- year.stf1 +
  theme_bw() +
  geom_point() +
  geom_smooth(method="gam") +
  scale_x_continuous(breaks=seq(2010,2019,by=1)) +
  scale_y_continuous(breaks=seq(0,1,by=0.25)) +
  labs(x="Year", y="Stocking Ratio", title = "Salmo trutta fario 1+")

year.stfad <- ggplot(fish[fish$species=="Salmo trutta fario",], aes(year, ratio.adult))
troutadyear <- year.stfad +
  theme_bw() +
  geom_point() +
  geom_smooth(method="gam") +
  scale_x_continuous(breaks=seq(2010,2019,by=1)) +
  scale_y_continuous(breaks=seq(0,1,by=0.25)) +
  labs(x="Year", y="Stocking Ratio", title = "Salmo trutta fario Adult")

library(gridExtra)
svg(filename="trends stocking ratio year.svg")
grid.arrange(salmon0year, trout0year, salmon1year, trout1year, nrow=2)
dev.off()

#### north-south #######
ns.ss0 <- ggplot(fish[fish$species=="Salmo salar",], aes(WGSn, ratio.0))
salmon0ns <- ns.ss0 +
  theme_bw() +
  geom_point() +
  geom_smooth(method="gam") +
  scale_x_continuous(breaks=seq(42.8,43.5,by=0.1)) +
  scale_y_continuous(breaks=seq(0,1,by=0.25)) +
  labs(x="WGS84 Northing", y="Stocking Ratio", title = "Salmo salar 0+")

ns.ss1 <- ggplot(fish[fish$species=="Salmo salar",], aes(WGSn, ratio.1))
salmon1ns <- ns.ss1 +
  theme_bw() +
  geom_point() +
  geom_smooth(method="gam") +
  scale_x_continuous(breaks=seq(42.8,43.5,by=0.1)) +
  scale_y_continuous(breaks=seq(0,1,by=0.25)) +
  labs(x="WGS84 Northing", y="Stocking Ratio", title = "Salmo salar 1+")

ns.stf0 <- ggplot(fish[fish$species=="Salmo trutta fario",], aes(WGSn, ratio.0))
trout0ns <- ns.stf0 +
  theme_bw() +
  geom_point() +
  geom_smooth(method="gam") +
  scale_x_continuous(breaks=seq(42.8,43.5,by=0.1)) +
  scale_y_continuous(breaks=seq(0,1,by=0.25)) +
  labs(x="WGS84 Northing", y="Stocking Ratio", title = "Salmo trutta fario 0+")

ns.stf1 <- ggplot(fish[fish$species=="Salmo trutta fario",], aes(WGSn, ratio.1))
trout1ns <- ns.stf1 +
  theme_bw() +
  geom_point() +
  geom_smooth(method="gam") +
  scale_x_continuous(breaks=seq(42.8,43.5,by=0.1)) +
  scale_y_continuous(breaks=seq(0,1,by=0.25)) +
  labs(x="WGS84 Northing", y="Stocking Ratio", title = "Salmo trutta fario 1+")

ns.stfad <- ggplot(fish[fish$species=="Salmo trutta fario",], aes(WGSn, ratio.adult))
troutadns <- ns.stfad +
  theme_bw() +
  geom_point() +
  geom_smooth(method="gam") +
  scale_x_continuous(breaks=seq(42.8,43.5,by=0.1)) +
  scale_y_continuous(breaks=seq(0,1,by=0.25)) +
  labs(x="WGS84 Northing", y="Stocking Ratio", title = "Salmo trutta fario Adults")

svg(filename="trends stocking ratio north-south.svg")
grid.arrange(salmon0ns, trout0ns, salmon1ns, trout1ns, nrow=2)
dev.off()

########### isma longitude (Easting) #####

ns.ss0 <- ggplot(fish[fish$species=="Salmo salar",], aes(WGSe, ratio.0))
salmon0ns <- ns.ss0 +
  theme_bw() +
  geom_point() +
  geom_smooth(method="gam") +
 
  scale_y_continuous(breaks=seq(0,1,by=0.25)) +
  labs(x="Longitude", y="Stocking Ratio", title = "Salmo salar 0+")

ns.ss1 <- ggplot(fish[fish$species=="Salmo salar",], aes(WGSe, ratio.1))
salmon1ns <- ns.ss1 +
  theme_bw() +
  geom_point() +
  geom_smooth(method="gam") +

  scale_y_continuous(breaks=seq(0,1,by=0.25)) +
  labs(x="Longitude", y="Stocking Ratio", title = "Salmo salar 1+")

ns.stf0 <- ggplot(fish[fish$species=="Salmo trutta fario",], aes(WGSe, ratio.0))
trout0ns <- ns.stf0 +
  theme_bw() +
  geom_point() +
  geom_smooth(method="gam") +

  scale_y_continuous(breaks=seq(0,1,by=0.25)) +
  labs(x="Longitude", y="Stocking Ratio", title = "Salmo trutta fario 0+")

ns.stf1 <- ggplot(fish[fish$species=="Salmo trutta fario",], aes(WGSe, ratio.1))
trout1ns <- ns.stf1 +
  theme_bw() +
  geom_point() +
  geom_smooth(method="gam") +

  scale_y_continuous(breaks=seq(0,1,by=0.25)) +
  labs(x="Longitude", y="Stocking Ratio", title = "Salmo trutta fario 1+")

svg(filename="trends stocking ratio north-south.svg")
grid.arrange(salmon0ns, trout0ns, salmon1ns, trout1ns, nrow=2)
dev.off()

#### elevation #######
elev.ss0 <- ggplot(fish[fish$species=="Salmo salar",], aes(elevation, ratio.0))
salmon0elev <- elev.ss0 +
  theme_bw() +
  geom_point() +
  geom_smooth(method="gam") +
  scale_x_continuous(breaks=seq(100,1300,100)) +
  scale_y_continuous(breaks=seq(0,1,by=0.25)) +
  labs(x="Elevation", y="Stocking Ratio", title = "Salmo salar 0+")

elev.ss1 <- ggplot(fish[fish$species=="Salmo salar",], aes(elevation, ratio.1))
salmon1elev <- elev.ss1 +
  theme_bw() +
  geom_point() +
  geom_smooth(method="gam") +
  scale_x_continuous(breaks=seq(100,1300,100)) +
  scale_y_continuous(breaks=seq(0,1,by=0.25)) +
  labs(x="Elevation", y="Stocking Ratio", title = "Salmo salar 1+")

elev.stf0 <- ggplot(fish[fish$species=="Salmo trutta fario",], aes(elevation, ratio.0))
trout0elev <- elev.stf0 +
  theme_bw() +
  geom_point() +
  geom_smooth(method="gam") +
  scale_x_continuous(breaks=seq(100,1300,100)) +
  scale_y_continuous(breaks=seq(0,1,by=0.25)) +
  labs(x="Elevation", y="Stocking Ratio", title = "Salmo trutta fario 0+")

elev.stf1 <- ggplot(fish[fish$species=="Salmo trutta fario",], aes(elevation, ratio.1))
trout1elev <- elev.stf1 +
  theme_bw() +
  geom_point() +
  geom_smooth(method="gam") +
  scale_x_continuous(breaks=seq(100,1300,100)) +
  scale_y_continuous(breaks=seq(0,1,by=0.25)) +
  labs(x="Elevation", y="Stocking Ratio", title = "Salmo trutta fario 1+")

elev.stfad <- ggplot(fish[fish$species=="Salmo trutta fario",], aes(elevation, ratio.adult))
troutadelev <- elev.stfad +
  theme_bw() +
  geom_point() +
  geom_smooth(method="gam") +
  scale_x_continuous(breaks=seq(100,1300,100)) +
  scale_y_continuous(breaks=seq(0,1,by=0.25)) +
  labs(x="Elevation", y="Stocking Ratio", title = "Salmo trutta fario Adult")

svg(filename="trends stocking ratio elevation.svg")
grid.arrange(salmon0elev, trout0elev, salmon1elev, trout1elev, nrow=2)
dev.off()

svg(filename="trends stocking ratio adult trouts.svg")
grid.arrange(troutadyear, troutadns, troutadelev, nrow=2)
dev.off()



##### Stocked density

year.ss0 <- ggplot(fish[fish$species=="Salmo salar",], aes(year, dens.0))
salmon0year <- year.ss0 +
  theme_classic2()+theme_cleveland() +
  geom_point() +
  geom_smooth(method="gam") +
  scale_x_continuous(breaks=seq(2010,2019,by=1)) +
  scale_y_continuous(breaks=seq(0,1,by=0.25)) +
  labs(x="Year", y="Stocking Ratio", title = "Salmo salar 0+")

year.ss1 <- ggplot(fish[fish$species=="Salmo salar",], aes(year, dens.1))
salmon1year <- year.ss1 +
   theme_classic2()+theme_cleveland() +
  geom_point() +
  geom_smooth(method="gam") +
  scale_x_continuous(breaks=seq(2010,2019,by=1)) +
  scale_y_continuous(breaks=seq(0,1,by=0.25)) +
  labs(x="Year", y="Stocking Ratio", title = "Salmo salar 1+")




year.stf0 <- ggplot(fish[fish$species=="Salmo trutta fario",], aes(year, dens.0))
trout0year <- year.stf0 +
   theme_classic2()+theme_cleveland() +
  geom_point() +
  geom_smooth(method="gam") +
  scale_x_continuous(breaks=seq(2010,2019,by=1)) +
  scale_y_continuous(breaks=seq(0,1,by=0.25)) +
  labs(x="Year", y="Stocking Ratio", title = "Salmo trutta fario 0+")

year.stf1 <- ggplot(fish[fish$species=="Salmo trutta fario",], aes(year, dens.1))
trout1year <- year.stf1 +
  theme_classic2()+theme_cleveland() +
  geom_point() +
  geom_smooth(method="gam") +
  scale_x_continuous(breaks=seq(2010,2019,by=1)) +
  scale_y_continuous(breaks=seq(0,1,by=0.25)) +
  labs(x="Year", y="Stocking Ratio", title = "Salmo trutta fario 1+")


grid.arrange(salmon0year, salmon1year, trout0year, trout1year, nrow=2)
