require(PBSmapping)
require(ggplot2)
require(grid)
require(mapproj)
require(plyr)
require(chron)
require(classInt)


basePlot <- function(dataset, city, survey, day, hour, shiplog, analysis, species, ctds, trawl, project, xlims, ylims, isob, bathy, pointsize, linesize, maxsize){

#ocean colour  
bg <- "white"  #'#bddfeb'

#smoother variable for land and contours in KM
smooth <- 0

# filter variable for removal of small land polygons and contours (by # of vertices)
vert <- 15

###############################
#-----  Regional Polygon  ----#

    data(nepacLLhigh)
    land <- nepacLLhigh


#thin Polygon
landT<- thinPolys(land, tol = smooth, filter = vert)
ld<-data.frame(landT)


############################
#------ Bathymetry --------#

if (dataset == "Pacific") {
  load("Rscripts/Map/Layers/NPObathy.rda")
  isobath <- NPObathy
  isobath$z <- isobath$z * -1 
} else{
  data(bcBathymetry)
  isobath <- bcBathymetry
}

#create contours
isobs = as.numeric(strsplit(isob, " ")[[1]])
con <- contourLines(isobath, levels = isobs)
contour <- convCP(con, projection = "LL")
thincon <- thinPolys(contour$PolySet, tol = smooth, filter = vert)
by <- merge(data.frame(thincon),data.frame(contour$PolyData), by=c("PID","SID"), all.x=T)
by$level <- as.factor(by$level)
by$ID <- as.integer(interaction(by$PID, by$SID)) # create unique ID for each contour line

#add rows to create bathy polygons
pby <- by
pby$level <- as.numeric(as.character(pby$level))
sumRows <- function(x) {
    PID = min(x$PID)
    SID = min(x$SID)
    POS = max(x$POS) + 1
      X = min(x$X)
      Y = min(x$Y)
  level = min(x$level)
  return(c(PID,SID,POS,X,Y,level))
}
pby <- ddply(pby, .(ID), sumRows)
colnames(pby) <- c("ID","PID","SID","POS","X","Y","level")
pby <- data.frame(rbind(by, data.frame(PID = pby$PID, SID = pby$SID, POS = pby$POS, X =pby$X, Y = pby$Y, level = factor(pby$level),ID = pby$ID)))
pby <- pby[order(pby$SID),]
pby <- pby[order(pby$PID),]
pby$level <- as.numeric(as.character(pby$level))


#contour labels
clip <- clipLines(thincon, xlim = xlims, ylim = ylims) 
bylab <- merge(data.frame(clip),data.frame(contour$PolyData), by=c("PID","SID"), all.x=T)
bylab$level <- as.factor(bylab$level)
#sort by lat and long
bylab <- bylab[order(bylab$X, decreasing = F),]
bylab <- bylab[order(bylab$Y, decreasing = F),]
bylab <- bylab[order(bylab$level, decreasing = F),]
#get one lat and long combination for each isobath
bylabs <- ddply(bylab, .(level), head, 50)
bylabs <- ddply(bylabs, .(level), tail, 1)


# isobath colours
numbathy <- length(unique(by$level))
blues <- colorRampPalette(c("#9ecae1", "#64a2ea", "#2B7BF3", "#0D65E7" , "#0B54C1", "#09439A" ,"#073274", "#04224D"))(numbathy)
blues <- data.frame(blues, level = sort(unique(by$level)))
by <- merge(by, blues, by = "level")
bylabs <- merge(bylabs, blues, by = "level")

#poly colours
ord <- data.frame(level = sort(unique(pby$level)))
ord$fac <- as.factor(letters[1:nrow(ord)])
pby <- merge(pby, ord, by = "level")
ocean.cols <- rev(c("#08306b","#08519c", "#2171b5", "#4292c6", "#6baed6" , "#9ecae1", "#c6dbef"))
ocean <- colorRampPalette(ocean.cols)(length(isobs))


############################
#-------- Citites ---------#
cities <- read.csv("Rscripts/Map/Layers/Cities.csv", header=T)
cities <- as.EventData(cities, projection="LL")
ct <- data.frame(cities)


#################################
#-------- CRUISE TRACK ---------#

#Load the slim cruise track file. If it doesn't exist, create one.
if (file.exists("Acoustics/Echoview/Exports/GPSTrack/SlimCruiseTrack.csv")) {
  track <- read.csv("Acoustics/Echoview/Exports/GPSTrack/SlimCruiseTrack.csv", header=T, row.names=1, stringsAsFactors = FALSE)
 } else if (file.exists("Acoustics/Echoview/Exports/GPSTrack/CruiseTrack.csv")) {
   tracks <- read.csv("Acoustics/Echoview/Exports/GPSTrack/CruiseTrack.csv", header=T, row.names=1,stringsAsFactors = FALSE)[,-7]
   track <-tracks[(seq(1,to=nrow(tracks),by=100)),] #keep 1 in every 100 records to slim dataset
   write.csv(track, "Acoustics/Echoview/Exports/GPSTrack/SlimCruiseTrack.csv")
 } else {
  stop("No CruiseTrack.csv found in '", file.path(getwd(), "Acoustics/Echoview/Exports/GPSTrack"), "' directory")
 }

#dates by day
date <- track[!duplicated(track$GPS_date),]
date$GPS_date <- dates(date$GPS_date, format = "y-m-d")
date$month_day <- paste(months(date$GPS_date),days(date$GPS_date),sep = "-")

#times by the hour
track$Time <- times(track$GPS_time)
track$Hours <- hours(track$Time)
time <- ddply(track, .(GPS_date), function(x) x[!duplicated(x$Hours),])
time <- time[-1,]
time <-time[(seq(1,to=nrow(time),by=4)),]
time$Hours <- paste(time$Hours, ":00", sep="")




#################################
#-------- TRANSECTS ---------#

# integration by cell data location
cellfile <- "Acoustics/Echoview/Exports/Sv raw pings T2/IntegratedByCells.csv"

#Load the integration file. If it doesn't exist, give error
if (file.exists(cellfile)) {
  int_cells <- read.csv(cellfile, header=T, row.names=1, stringsAsFactors = FALSE)
} else {
  stop(file.path(getwd(),cellfile), " not found")
}

int_cells <- int_cells[!int_cells$Lat_S == 999,]


  
#################################
#-------- CRUISE LOG ---------#

cruiselog <- read.csv("Acoustics/Echoview/Exports/Log/CruiseLog.csv", header=T, stringsAsFactors = FALSE)
Log <- cruiselog[cruiselog$Region_type == " Marker",]


#transects 
start <- Log[grep(" ST", Log$Region_name, ignore.case=TRUE), c("Region_class", "Date_s","Time_s","Lat_s","Lon_s","Region_name","File")]
start <- start[order(start$Date_s, start$Time_s),]

end <- Log[grep(" ET", Log$Region_name, ignore.case=TRUE), c("Region_class", "Date_s","Time_s","Lat_s","Lon_s","Region_name","File")]
end <- end[order(end$Date_s, end$Time_s),]

if (nrow(start) == nrow(end)){
  transects <- data.frame(start=start, end=end)
} else {
  stop("Different number of start (ST = ", nrow(start), ") and end (ET = ", nrow(end), ") transects in 'Region_name' column of CruiseLog.csv")
}


#trawling
trawls <- Log[grep("SD", Log$Region_name, ignore.case=TRUE), c("Region_class", "Date_s","Time_s","Lat_s","Lon_s","Region_name")]


#analysis
region <- cruiselog[cruiselog$Region_type == " Analysis", c("Region_class", "Date_s","Time_s","Lat_s","Lon_s","Lat_e","Lon_e","Region_name")]

#set colour scaling for analysis regions
numregion <- length(unique(region$Region_class))
multicols <- c("#e41a1c", "#ff7f00", "#FCDF00", "#4daf4a", "#00DDFF", "#f781bf", "#984ea3", "#a65628")
leg <- ifelse(numregion > length(multicols), length(multicols), numregion)
set1 <- colorRampPalette(multicols[1:leg])(numregion)
set1 <- data.frame(set1, Region_class = unique(region$Region_class))
region <- merge(region, set1, by = "Region_class")
region$set1 <- as.character(region$set1)
region$alpha <- 0

#analysis region labels
regionlabs <- region[!duplicated(region$set1),]


#ctd
ctd <- cruiselog[grep("ctd", cruiselog$Region_name, ignore.case=TRUE), c("Region_class", "Date_s","Time_s","Lat_s","Lon_s","Region_name")]



#################################
#------------ NASC ------------#

intfile <- "Acoustics/Echoview/Exports/Sv raw pings T2/IntegratedByRegionsByCells.csv"

#Load the integration file. If it doesn't exist, give error
if (file.exists(intfile)) {
  int <- read.csv(intfile, header=T, row.names=1, stringsAsFactors = FALSE)
} else {
  stop(file.path(getwd(),intfile), " not found")
}

#set colours
nasc.df <- merge(int, set1, by = "Region_class")
nasc.df$set1 <- as.character(nasc.df$set1)
nasc.df$alpha <- 0


#which analysis regions to show
for (i in species){
  region$alpha[region$Region_class == i] <- 1
  nasc.df$alpha[nasc.df$Region_class == i] <- 1
}

#set breaks
breaks <- signif(classIntervals(nasc.df$PRC_NASC, 5, style = "kmeans")$brks,1)[-1]


######################################################
######################################################
# PLOT

base <- ggplot(data = NULL)

#polygon
if(bathy == "poly")  {
  for (i in unique(pby$level)) {
    base <- base + geom_polygon(data = pby[pby$level == i,], 
                                aes_string(x = "X", y = "Y", group="ID", fill="fac"))
    base <- base + scale_fill_manual(values = ocean, name="Depth", guide = "legend", 
                                     labels=ord$level)
  }
}

#contours
if(bathy == "cont")  {
  base <- base + geom_path(data = by, aes_string(x = "X", y = "Y", group="ID", 
                                                 colour="blues"), size=.2, show_guide  = F)
  base <- base + geom_text(data = bylabs, aes_string(x = "X", y = "Y", label="level"),
                           colour=bg, size = pointsize, angle = 0, fontface = 2)
  base <- base + geom_text(data = bylabs, aes_string(x = "X", y = "Y", label="level", 
                          colour="blues"), size = pointsize, angle = 0, fontface = 1,
                          show_guide  = F)
}

#base
base <- base + geom_polygon(data = ld, aes_string(x = "X", y = "Y", group="PID"), 
                            fill = "gray80", colour = "gray60", size = .1)
base <- base + labs(x = "Longitude", y = "Latitude")
base <- base + coord_map(xlim=xlims, ylim=ylims, projection = project)
base <- base + scale_colour_identity("Regions",labels =  regionlabs$Region_class, 
                                     breaks = regionlabs$set1, guide = "legend")


#themes
base <- base + theme(panel.border = element_rect(fill=NA, colour="black"),
    panel.background = element_rect(fill=bg,colour="black"),
    axis.ticks = element_line(colour="black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.ticks.length = unit(0.1,"cm"),
    axis.text = element_text(size=10, colour = "black"),
    axis.title = element_text(size=11),
    legend.text = element_text(size=11),
    legend.title = element_text(size=12, face="plain"),
    legend.background = element_blank(), legend.key = element_blank(),
    legend.key.height = unit(.5,"cm"), legend.key.width = unit(.4,"cm"),
    legend.justification = c(1,1), legend.position = "right", 
    plot.margin = unit(c(.5,.5,.5,.5), "lines")) # top, right, bottom, and left 


#guides
base <- base + guides(colour = guide_legend(override.aes = list(size=4)))

#city
if(city == TRUE)  {
  base <- base + geom_point(data = ct, aes_string(x = "X", y = "Y"), pch=20, 
                          colour = "black", size = pointsize) +
                 geom_text(data = ct, aes_string(x = "X", y = "Y", label="label"), 
                          colour = "black", size = pointsize, hjust = -.2)
}

#cruise track
if(survey == TRUE) 
  base <- base + geom_path(data = track, aes_string(x = "Longitude", y = "Latitude"),
                           linetype = "dotted", size = .5)

#cruise day
if(day == TRUE) 
  base <- base + geom_text(data = date, aes_string(x = "Longitude", y = "Latitude", 
                           label = "month_day"), colour="black", size=pointsize,  fontface = 2)

#cruise times
if(hour == TRUE) 
  base <- base + geom_text(data = time, aes_string(x = "Longitude", y = "Latitude", 
                           label = "Hours"), colour="black", size=pointsize,  fontface = 2)

#transects
if(shiplog == TRUE) {
  base <- base + geom_path(data = int_cells, aes_string(x = "Lon_S", y = "Lat_S", group = "EV_filename"), 
                              size = linesize) +
                geom_text(data = transects, aes_string(x = "start.Lon_s", 
                          y = "start.Lat_s", label="start.Region_name"), size = pointsize, 
                          vjust = 1.2, fontface = 2)
}


#regions
if(analysis == "Regions") {
  base <- base + geom_point(data = nasc.df, aes_string(x = "Lon_S", y = "Lat_S", 
                              colour="set1", alpha = "alpha"), 
                              pch = 20, size = linesize) +
                 scale_alpha_identity()
}

#nasc
if(analysis == "NASC") {
  base <- base + geom_point(data = nasc.df, aes_string(x = "Lon_S", y = "Lat_S",
                            colour="set1", alpha = "alpha", size = "PRC_NASC"), pch = 20, 
                            max_size = maxsize) +
                 scale_size_area(max_size = maxsize, name = "NASC", breaks = breaks) +
                 scale_alpha_identity()
}

#trawls
if(trawl == TRUE)  {
  base <- base + geom_point(data = trawls, aes_string(x = "Lon_s", y = "Lat_s"), 
                            pch=43, colour = "#BC00D9", size = pointsize) +
                 geom_text(data = trawls, aes_string(x = "Lon_s", y = "Lat_s", 
                            label="Region_name"), colour = "#BC00D9", 
                            size = pointsize, vjust = -.2, fontface = 2)
}

#ctds
if(ctds == TRUE)  {
  base <- base + geom_point(data = ctd, aes_string(x = "Lon_s", y = "Lat_s"), pch=8, 
                            size = pointsize)
}


######
return(base)
}




##########################################################
# x and y lims for initial plot

# integration by cell data location
cellfile <- "Acoustics/Echoview/Exports/Sv raw pings T2/IntegratedByCells.csv"

#Load the integration file. If it doesn't exist, give error
if (file.exists(cellfile)) {
  int_cells <- read.csv(cellfile, header=T, row.names=1, stringsAsFactors = FALSE)
} else {
  stop(file.path(getwd(),cellfile), " not found")
}

int_cells <- int_cells[!int_cells$Lat_S == 999,]


y1 <- min(int_cells$Lat_S)-.5
y2 <- max(int_cells$Lat_S)+.5

x1 <- min(int_cells$Lon_S)-.5
x2 <- max(int_cells$Lon_S)+.5


