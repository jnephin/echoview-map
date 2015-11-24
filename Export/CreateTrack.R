require(RDCOMClient)
require(PBSmapping)
require(ggplot2)
require(grid)
require(chron)
source("1_Paths.R")




###############################################
#           Load in file names                #
###############################################
# list .raw files
RAWfile.list <- list.files(file.path(getwd(), RAWdir), pattern="*.raw$")
RAWfileNames <- file.path(getwd(), RAWdir, RAWfile.list)



###############################################
#              RUN in Echoview                #
###############################################

# create COM connection between R and Echoview
EVApp <- COMCreate("EchoviewCom.EvApplication")

#create new EV file
EVfile <- EVApp$NewFile()

#set fileset object
filesetObj <- EVfile[["Filesets"]]$Item(0)

#add data
for (i in RAWfileNames){
  filesetObj[["DataFiles"]]$Add(i)
}


#set EvVariable object
evVarObj <- EVfile[["Variables"]]
evVarObj$FindByName("Sv raw pings T2") # force echoview to wait for raw files to upload

#get the gps fixes variable
gpsfix <- evVarObj$FindByName("position GPS fixes")

#export cruise track
gpsfix$ExportData(file.path(getwd(), GPSdir, "CruiseTrack.csv"))

#close EV file
EVApp$CloseFile(EVfile)


##########################################
#quit echoview
EVApp$Quit()







###############################################
#             Survey Track Map                #
###############################################



## Load cruise track
if (file.exists("Acoustics/Echoview/Exports/GPSTrack/SlimCruiseTrack.csv")) {
  track <- read.csv("Acoustics/Echoview/Exports/GPSTrack/SlimCruiseTrack.csv", header=T, row.names=1, stringsAsFactors = FALSE)
} else if (file.exists("Acoustics/Echoview/Exports/GPSTrack/CruiseTrack.csv")) {
  tracks <- read.csv("Acoustics/Echoview/Exports/GPSTrack/CruiseTrack.csv", header=T, row.names=1,stringsAsFactors = FALSE)[,-7]
  track <-tracks[(seq(1,to=nrow(tracks),by=100)),] #keep 1 in every 100 records to slim dataset
  write.csv(track, "Acoustics/Echoview/Exports/GPSTrack/SlimCruiseTrack.csv")
} else {
  stop("No CruiseTrack.csv found in '", file.path(getwd(), "Acoustics/Echoview/Exports/GPSTrack"), "' directory")
}

# GPS status
track <- track[!track$GPS_status == 1,]
  
#dates by day
date <- track[!duplicated(track$GPS_date),]
date$GPS_date <- dates(date$GPS_date, format = "y-m-d")
date$month_day <- paste(months(date$GPS_date),days(date$GPS_date),sep = "-")

## set limits
xlims <- c(min(track$Longitude)-.2, max(track$Longitude)+.2)
ylims <- c(min(track$Latitude)-.2, max(track$Latitude)+.2)

##  Regional Polygon 
data(nepacLLhigh)
land <- nepacLLhigh
landT<- thinPolys(land, tol = 0, filter = 15)
ld<-data.frame(landT)


## Contours
data(bcBathymetry)
isobath <- bcBathymetry
con <- contourLines(isobath, levels = c(50,100,200,500,1000,1500))
contour <- convCP(con, projection = "LL")
thincon <- thinPolys(contour$PolySet, tol = 0, filter = 15)
by <- merge(data.frame(thincon),data.frame(contour$PolyData), by=c("PID","SID"), all.x=T)
by$level <- as.factor(by$level)
by$ID <- as.integer(interaction(by$PID, by$SID))
blues <- colorRampPalette(c("#9ecae1", "#64a2ea", "#2B7BF3", "#0D65E7" , "#0B54C1", "#09439A" ,"#073274", "#04224D"))(6)
blues <- data.frame(blues, level = sort(unique(by$level)))
by <- merge(by, blues, by = "level")


## MAP
map <- ggplot(data = NULL) +
        geom_path(data = by, aes(x = X, y = Y, group=ID, colour=blues), size=.1) +
        geom_polygon(data = ld, aes(x = X, y = Y, group=PID), 
                            fill = "gray80", colour = "gray60", size = .1) +
        labs(x = "Longitude", y = "Latitude") +
        coord_map(xlim=xlims, ylim=ylims) +
        scale_colour_identity() +
        theme(panel.border = element_rect(fill=NA, colour="black"),
                     panel.background = element_rect(fill="white",colour="black"),
                     axis.ticks = element_line(colour="black"),
                     panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(),
                     axis.ticks.length = unit(0.1,"cm"),
                     axis.text = element_text(size=10, colour = "black"),
                     axis.title = element_text(size=11),
                     plot.margin = unit(c(.5,.5,.5,.5), "lines")) + 
#cruise track
      geom_path(data = track, aes(x = Longitude, y = Latitude),
                           linetype = "solid", size = .8) +
#cruise day
       geom_text(data = date, aes(x = Longitude, y = Latitude, label = month_day), 
                 colour="black", size=2,  fontface = 2, vjust = -.5)


#print pdf
pdf("Other data/Figures/Trackline_Map.pdf", width=12, height=12)
map
dev.off()
