##### 01. SETUP #########
#package  installation
pckg <- installed.packages()
req.pkg <- c("readxl", "MESS", "KOGMWU", "lattice", "dplyr")

inst.pkg <- req.pkg[!(req.pkg %in% pckg[,1])]
install.packages(inst.pkg)
lapply(req.pkg, library, character.only=T)
rm(inst.pkg,req.pkg, pckg)

Sys.setenv(LANG = "en")
Sys.setlocale("LC_TIME", "C")

##### 02. DATA EXPLORATION #####
#### 2.1 outliers ####
op <- par(mfrow=c(3, 1), mar=c(3,3,3,1))

dotchart(fish$ind.0, main = "Ind age class 0+", groups = as.factor(fish$species))
dotchart(fish$ind.1, main = "Ind age class 1+", groups = as.factor(fish$species))
dotchart(fish$ind.adult, main = "Ind age class adult", groups = as.factor(fish$species))

dotchart(fish$dens.0, main = "Dens age class 0+", groups = as.factor(fish$species))
dotchart(fish$dens.1, main = "Dens age class 1+", groups = as.factor(fish$species))
dotchart(fish$dens.adult, main = "Dens age class adult", groups = as.factor(fish$species))

dotchart(fish$bm.0, main = "BM age class 0+", groups = as.factor(fish$species))
dotchart(fish$bm.1, main = "BM age class 1+", groups = as.factor(fish$species))
dotchart(fish$bm.adult, main = "BM age class adult", groups = as.factor(fish$species))

par(op)
#### 2.2 collinearity ####
#function to calculate spearman's correlation
panel.cor <- function(x, y, digits = 2, cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  # correlation coefficient
  r <- cor.test(x, y, method="spearman")$estimate
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  text(0.5, 0.5, txt, cex=cex.cor*abs(r))
  
  # # p-value calculation
  # p <- cor.test(x, y, method="spearman")$p.value
  # txt2 <- format(c(p, 0.123456789), digits = digits)[1]
  # txt2 <- paste("p= ", txt2, sep = "")
  # if(p<0.01) txt2 <- paste("p= ", "<0.01", sep = "")
  # text(0.5, 0.4, txt2)
}

fish$river.nr <- as.numeric(as.factor(fish$river))
pairs(fish[,c(35:37,43:45,47:49,39:41,2,4,5,12:15,51)], lower.panel = panel.smooth, upper.panel = panel.cor, diag.panel = panel.hist, cex.cor=1.5)

# #### 3. temporal trend elevation #####
# elev <- lm(fish$elevation ~ fish$year)
# summary(elev)
# 
# elev.plot <- ggplot(fish, aes(year, elevation))
# elev.plot +
#   theme_bw()+
#   geom_point() +
#   geom_smooth(method="lm") +
#   scale_x_continuous(breaks=seq(2010,2019,by=1)) +
#   labs(x="Year", y="Elevation")
# 
# elev.box <- ggplot(fish, aes(as.factor(year), elevation))
# elev.box +
#   theme_bw()+
#   geom_boxplot() +
#   labs(x="Year", y="Elevation")
# 
