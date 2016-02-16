require(ggplot2)
require(PBSmapping)
library(rgdal)
require(sp)
require(maptools)
require(plyr)
require(alphahull)
require(rgeos)


# set working directory 
setwd('..');setwd('..')



##########################################################################################
##########################################################################################
# load data


# load backscatter data
nasc <- read.csv("Acoustics/Echoview/Exports/Sv raw pings T2/IntegratedByCells.csv", header=T, 
                       stringsAsFactors = FALSE, row.names = 1)




##########################################################################################
##########################################################################################
# Regional Polygon 


# projection string
pj <- CRS("+proj=aea +lat_1=38 +lat_2=58 +lat_45=0 +lon_0=-126 +x_0=1000000.0 +y_0=0 +ellps=GRS80 +datum=NAD83")


data(nepacLL)
world <- nepacLL
land <- clipPolys(world, xlim = c(-130, -120), ylim =c (47, 50)) #clip to NPO
landT<- thinPolys(land, tol = 1, filter = 20) #remove small polys
sp.world <- PolySet2SpatialPolygons(landT)
proj.world <- spTransform(sp.world, CRS=pj)
m.world <- fortify(proj.world)


#map
ggplot(data = NULL) +
  geom_path(data = m.world, aes(x=long, y=lat, group = group)) +
  coord_fixed() +
  theme_bw()



##########################################################################################
##########################################################################################
# NASC data

# add replciate
nasc$Transect <-  sub(".*/", "", nasc$EV_filename) # remove everything before /
nasc$Replicate <-  gsub("[A-Za-z ]", "", nasc$Transect)
nasc$Replicate <-  as.numeric(sub("[.].*", "", nasc$Replicate))


#group by interval
nasc <- ddply(nasc, .(Lat_S, Lon_S, Interval, Transect, Replicate),
                   summarise,
                   NASC = sum(NASC),
                   Date = head(Date_S,1),
                   Time = head(Time_S,1))

# remove bad data
nasc <- nasc[!nasc$Lat_S == 999,]


# --  repeat script once for each survey or replicate combo
int <- nasc[nasc$Replicate == "1", ]


# transform into spatial dataframe and project
forhull <- int
coordinates(int) <- c("Lon_S", "Lat_S")
proj4string(int) <- CRS("+proj=longlat")
proj.int <- spTransform(int, CRS=pj)
track <- as.data.frame(proj.int)


# map
ggplot(data = NULL) +
  geom_path(data = m.world, aes(x=long, y=lat, group = group)) +
  geom_path(data = track, aes(x=Lon_S, y=Lat_S, group = Transect), colour= "dodgerblue") +
  coord_fixed() +
  theme_bw()


##########################################################################################
##########################################################################################
# alpha hull

# remove duplicates 
forhull <- forhull[!duplicated(forhull[c("Lon_S", "Lat_S")]),]

## Create polygon surrounding track points
hull <- ahull(x= forhull$Lon_S, y= forhull$Lat_S, alpha = 1.5)
plot(hull, asp=1)
bb.pts <- forhull[hull$arcs[,"end1"],c("Lon_S", "Lat_S")]  # extract the boundary points from XY
bb.pts <- rbind(bb.pts,bb.pts[1,])                  # add the closing point
track.poly <- SpatialPolygons(list(Polygons(list(Polygon(bb.pts)),ID="s1"))) # create the SpatialPolygons
proj4string(track.poly) <- CRS("+proj=longlat")
proj.boundary <- spTransform(track.poly, CRS=pj) #project hull
track.boundary <- fortify(proj.boundary)

# map
ggplot(data = NULL) +
  geom_path(data = m.world, aes(x=long, y=lat, group = group)) +
  geom_path(data = track, aes(x=Lon_S, y=Lat_S, group = Transect), colour= "dodgerblue", size =1) +
  geom_path(data = track.boundary, aes(x=long, y=lat, group = group), colour= "red", size =1) +
  coord_fixed() +
  theme_bw()



##########################################################################################
##########################################################################################
# Area

# clip track bondary with land polygon

buff.world <- gBuffer(proj.world, width = 2000)

plot(buff.world, col= "blue")
plot(proj.world, col= "yellow", add=T)

erase <- gDifference(proj.boundary, buff.world)
area.hull <- gBuffer(erase, width = 1000)

# map
plot(area.hull, col = "orange")
plot(proj.int, col = "dodgerblue", add=T, pch = 1, cex = .1)


# area in square nautical miles
harea <- gArea(area.hull)
harea * (2.915533496 * 10^-7)

# rep 1 = 1326
# rep 2 = 1246



