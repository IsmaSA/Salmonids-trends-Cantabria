library(gridExtra)
library(ggplot2)
plotIND0 <- function(data, ylim, ylab, title){
  ind0 <- ggplot(data, aes(year, ind.0))
  ind0 +
    theme_bw() +
    geom_point() +
    geom_smooth(method="gam") +
    scale_x_continuous(breaks=seq(2010,2019,by=1)) +
    ylim(ylim)+
    labs(x="Year", y=ylab, title = title)
}

plotIND1 <- function(data, ylim, ylab, title){
  ind1 <- ggplot(data, aes(year, ind.1))
  ind1 +
    theme_bw()+
    geom_point() +
    geom_smooth(method="gam") +
    scale_x_continuous(breaks=seq(2010,2019,by=1)) +
    ylim(ylim)+
    labs(x="Year", y=ylab, title = title)
}

plotINDad <- function(data, ylim, ylab, title){
  indadult <- ggplot(data, aes(year, ind.adult))
  indadult +
    theme_bw() +
    geom_point() +
    geom_smooth(method="gam") +
    scale_x_continuous(breaks=seq(2010,2019,by=1)) +
    ylim(ylim)+
    labs(x="Year", y=ylab, title = title)
}

plotDENS0 <- function(data, ylim, ylab, title){
  dens0 <- ggplot(data, aes(year, dens.0))
  dens0 +
    theme_bw() +
    geom_point() +
    geom_smooth(method="gam") +
    scale_x_continuous(breaks=seq(2010,2019,by=1)) +
    ylim(ylim)+
    labs(x="Year", y=ylab, title = title)
}

plotDENS1 <- function(data, ylim, ylab, title){
  dens1 <- ggplot(data, aes(year, dens.1))
  dens1 +
    theme_bw()+
    geom_point() +
    geom_smooth(method="gam") +
    scale_x_continuous(breaks=seq(2010,2019,by=1)) +
    ylim(ylim)+
    labs(x="Year", y=ylab, title = title)
}

plotDENSad <- function(data, ylim, ylab, title){
  densadult <- ggplot(data, aes(year, dens.adult))
  densadult +
    theme_bw() +
    geom_point() +
    geom_smooth(method="gam") +
    scale_x_continuous(breaks=seq(2010,2019,by=1)) +
    ylim(ylim)+
    labs(x="Year", y=ylab, title = title)
}

plotBM0 <- function(data, ylim, ylab, title){
  bm0 <- ggplot(data, aes(year, bm.0))
  bm0 +
    theme_bw() +
    geom_point() +
    geom_smooth(method="gam") +
    scale_x_continuous(breaks=seq(2010,2019,by=1)) +
    ylim(ylim)+
    labs(x="Year", y=ylab, title = title)
}

plotBM1 <- function(data, ylim, ylab, title){
  bm1 <- ggplot(data, aes(year, bm.1))
  bm1 +
    theme_bw()+
    geom_point() +
    geom_smooth(method="gam") +
    scale_x_continuous(breaks=seq(2010,2019,by=1)) +
    ylim(ylim)+
    labs(x="Year", y=ylab, title = title)
}

plotBMad <- function(data, ylim, ylab, title){
  bmadult <- ggplot(data, aes(year, bm.adult))
  bmadult +
    theme_bw()+
    geom_point() +
    geom_smooth(method="gam") +
    scale_x_continuous(breaks=seq(2010,2019,by=1)) +
    ylim(ylim)+
    labs(x="Year", y=ylab, title = title)
}

# age class 0+
salmon0ind <- plotIND0(fish[fish$species=="Salmo salar",], ylim = c(0,300), "Individuals Age Class 0+", "Salmo salar")
trout0ind <- plotIND0(fish[fish$species=="Salmo trutta fario",], ylim = c(0,300), "Individuals Age Class 0+", "Salmo trutta fario")

salmon0dens <- plotDENS0(fish[fish$species=="Salmo salar",], ylim = c(0,1), as.expression(bquote("Density Age Class 0+ [ind/" ~ m^2 ~ "]")), "Salmo salar")
trout0dens <- plotDENS0(fish[fish$species=="Salmo trutta fario",], ylim = c(0,1), as.expression(bquote("Density Age Class 0+ [ind/" ~ m^2 ~ "]")), "Salmo trutta fario")

salmon0bm <- plotBM0(fish[fish$species=="Salmo salar",], ylim = c(0,5), as.expression(bquote("Biomass Age Class 0+ [kg/" ~ m^2 ~ "]")), "Salmo salar")
trout0bm <- plotBM0(fish[fish$species=="Salmo trutta fario",], ylim = c(0,5), as.expression(bquote("Biomass Age Class 0+ [kg/" ~ m^2 ~ "]")), "Salmo trutta fario")

# age class 1+
salmon1ind <- plotIND1(fish[fish$species=="Salmo salar",], ylim = c(0,200), "Individuals Age Class 1+", "Salmo salar")
trout1ind <- plotIND1(fish[fish$species=="Salmo trutta fario",], ylim = c(0,200), "Individuals Age Class 1+", "Salmo trutta fario")

salmon1dens <- plotDENS1(fish[fish$species=="Salmo salar",], ylim = c(0,1), as.expression(bquote("Density Age Class 1+ [ind/" ~ m^2 ~ "]")), "Salmo salar")
trout1dens <- plotDENS1(fish[fish$species=="Salmo trutta fario",], ylim = c(0,1), as.expression(bquote("Density Age Class 1+ [ind/" ~ m^2 ~ "]")), "Salmo trutta fario")

salmon1bm <- plotBM1(fish[fish$species=="Salmo salar",], ylim = c(0,25), as.expression(bquote("Biomass Age Class 1+ [kg/" ~ m^2 ~ "]")), "Salmo salar")
trout1bm <- plotBM1(fish[fish$species=="Salmo trutta fario",], ylim = c(0,25), as.expression(bquote("Biomass Age Class 1+ [kg/" ~ m^2 ~ "]")), "Salmo trutta fario")

# adults
salmonADind <- plotINDad(fish[fish$species=="Salmo salar",], ylim = c(0,NA), "Adult Individuals", "Salmo salar")
troutADind <- plotINDad(fish[fish$species=="Salmo trutta fario",], ylim = c(0,NA), "Adult Individuals", "Salmo trutta fario")

salmonADdens <- plotDENSad(fish[fish$species=="Salmo salar",], ylim = c(0,0.2), as.expression(bquote("Adult Density [ind/" ~ m^2 ~ "]")), "Salmo salar")
troutADdens <- plotDENSad(fish[fish$species=="Salmo trutta fario",], ylim = c(0,0.2), as.expression(bquote("Adults Density [ind/" ~ m^2 ~ "]")), "Salmo trutta fario")

salmonADbm <- plotBMad(fish[fish$species=="Salmo salar",], ylim = c(0,35), as.expression(bquote("Adult Biomass [kg/" ~ m^2 ~ "]")), "Salmo salar")
troutADbm <- plotBMad(fish[fish$species=="Salmo trutta fario",], ylim = c(0,35), as.expression(bquote("Adult Biomass [kg/" ~ m^2 ~ "]")), "Salmo trutta fario")

svg(filename="trends individuals.svg")
grid.arrange(salmon0ind, trout0ind, salmon1ind, trout1ind, nrow=2)
dev.off()

svg(filename="trends density.svg")
grid.arrange(salmon0dens, trout0dens, salmon1dens, trout1dens, nrow=2)
dev.off()

svg(filename="trends biomass.svg")
grid.arrange(salmon0bm, trout0bm, salmon1bm, trout1bm, nrow=2)
dev.off()

svg(filename="trends adults.svg")
grid.arrange(salmonADind, troutADind, salmonADdens, troutADdens, salmonADbm, troutADbm, nrow=3)
dev.off()
