library(maptools)
library(sp)
library(shapefiles)
library(sf)
library(terra)
library(foreign)
library(gpclib)
library(rgeos)

fdc<-readShapePoly("Cartographie/geom/Tunisie_snuts4")
summary(fdc)

donnees <- read.csv("./Marriages2020-2021.csv", header = TRUE, sep = ";",  dec = ",", encoding = "latin1")
dim(donnees)

pt <- cbind(fdc@data[, "id"], as.data.frame(coordinates(fdc)))
colnames(pt) <- c("id", "x", "y")
i=match(pt[, "id"], donnees[, "del"])
pt <- data.frame(pt, donnees[i, ])
pt$var <- pt$Marriages2021

x1 <- bbox(fdc)[1]
y1 <- bbox(fdc)[2]
x2 <- bbox(fdc)[3]
y2 <- bbox(fdc)[4]


sfdc <- (x2 - x1) * (y2 - y1)


sc <- sum(pt$var, na.rm = TRUE)


k <- 0.2  
pt$size <- sqrt((pt$var * k * sfdc/sc)/pi)

# Tracer le fond 
plot(fdc, border = "white", col = "grey")
# affichage des cercles proportionnels
symbols(pt[, c("x", "y")], circles = pt$size, add = TRUE, bg = "red", inches = FALSE)

#############

# Titre
title(main = "Marriages, 2021",  cex.sub = 0.7)
# échelle
xscale <- x2
yscale <- y1
sizescale <- 50000
labelscale <- "50km"
SpatialPolygonsRescale(layout.scale.bar(), offset = c(xscale, yscale), scale = sizescale, fill = c("black"), plot.grid = F)
text(xscale + sizescale/2, yscale, paste(labelscale, "\n\n", sep = ""), cex = 0.7)
# fleche nord
xarrow <- x1
yarrow <- y2 - (y2 - y1)/10
SpatialPolygonsRescale(layout.north.arrow(2), offset = c(xarrow, yarrow), scale = 50000, plot.grid = F)

## Titre de la légende.
LegTitle <- "Nombre \nde marriages\n"
## Tracer des cercles dont la taille indique les valeurs de la variable population.
rLeg <- quantile(pt$size, c(1, 0.9, 0.25, 0), type = 1, na.rm = TRUE);rLeg
rVal <- quantile(pt$var, c(1, 0.9, 0.25, 0), type = 1, na.rm = TRUE);rVal
l <- data.frame(x = x1, y = y1);head(l)
xinit <- l$x + rLeg[1];xinit
ypos <- l$y + rLeg;ypos
symbols(x = rep(xinit, 4), y = ypos, circles = rLeg, add = TRUE, bg = "red", 
inches = FALSE)
text(x = rep(xinit, 4) + rLeg[1] * 1.2, y = (l$y + (2 * rLeg)), rVal, cex = 0.3, 
srt = 0, adj = 0)
for (i in 1:4) {
segments(xinit, (l$y + (2 * rLeg[i])), xinit + rLeg[1] * 1.1, (l$y + (2 * 
rLeg[i])))
}
text(x = xinit - rLeg[1], y = (l$y + (2 * rLeg[1])), LegTitle, adj = c(0, 0), 
cex = 0.7)



i=match(fdc@data[, "id"], donnees[, "del"])
fdc@data <- data.frame(fdc@data, donnees[i,])
fdc@data$var <- fdc@data$Marriages2021
var <- as.vector(na.omit(fdc@data$var))

nbclass <- 8


library(classInt)
distr <- classIntervals(var, nbclass, style = "quantile")$brks

library(RColorBrewer)
colours <- brewer.pal(nbclass, "YlOrRd")
colMap <- colours[(findInterval(fdc$var, distr, all.inside = TRUE))]

plot(fdc, col = colMap, border = "black", lwd = 1)

# legende
legend(x = "topright", legend = leglabs(round(distr, 2), over = "plus de", under = "moins de"), fill = colours, bty = "n", pt.cex = 1, cex = 0.7, title = "indice 0-1")
# Titre
title(main = "Marriages 2021", cex.sub = 0.7)


#####################################

tun_df <- fortify(fdc,region = "id")
dt=data.frame(fdc@data[,c("id","Difference","Region")])
tun_df <- merge(tun_df,dt, by="id")
colnames(tun_df)[8]="Difference"

splits <- 7
mid.point<-0
pos.vals <- tun_df$Difference[tun_df$Difference > mid.point]
neg.vals <- tun_df$Difference[tun_df$Difference < mid.point]
pos.quants <- quantile(c(mid.point, pos.vals), 0:((splits - 1) / 2) / ((splits - 1) / 2), names=F)
neg.quants <- quantile(c(mid.point, neg.vals), 0:((splits - 1) / 2) / ((splits - 1) / 2), names=F)
quants <- c(neg.quants, pos.quants[-1])  

get_col <- colorRamp(c("red", "white", "green"))  # make fun to interpolate colors
colours <- rgb(get_col(0:(splits - 1)/(splits - 1)), max=255)       # 7 evenly interpolated colors 
val.remap <- (quants - min(quants)) / +   diff(range(quants))
quants2=as.integer(c(quants[1],mid.point,quants[7]))


p<-ggplot(tun_df, aes(x=long, y=lat, group=group)) +   geom_polygon(aes(fill=Difference),color = "grey50")+   scale_fill_gradientn(colours=colours,     values=val.remap,    breaks=quants2,name="Marriages Difference\n(2021 - 2022)")++   labs(x="",y="")+ theme_bw()++   coord_fixed()
p<-p+theme(axis.line=element_blank(),+       axis.text.x=element_blank(),+       axis.text.y=element_blank(),+       axis.ticks=element_blank(),+       axis.title.x=element_blank(),+       axis.title.y=element_blank(),panel.grid.major = element_blank(),+     panel.grid.minor = element_blank(),+     panel.border = element_blank(),+     panel.background = element_blank())
p<-p+ theme(legend.position="right")
p