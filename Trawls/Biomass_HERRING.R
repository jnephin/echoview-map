require(ggplot2)
require(plyr)

# set working directory 
setwd('..');setwd('..')


# load mean lengths and weight
summ <- read.csv("Other data/Fishing/morpho_summary.csv", header=T, stringsAsFactors = FALSE, row.names = 1)

# load backscatter data
nasc <- read.csv("Acoustics/Echoview/Exports/Sv raw pings T2/IntegratedByRegionsByCells.csv", header=T, stringsAsFactors = FALSE, row.names = 1)
nasc_cells <- read.csv("Acoustics/Echoview/Exports/Sv raw pings T2/IntegratedByCells.csv", header=T, stringsAsFactors = FALSE, row.names = 1)

# load echoview log
log <- read.csv("Other data/Log/Cruiselog.csv", header=T, stringsAsFactors = FALSE, row.names=1)

# load target strength coefficients
coeff <- read.csv("EchoviewR/Trawls/TS_coefficients.csv", header=T, stringsAsFactors = FALSE)



##########################################################################################
##########################################################################################
# species of interest

# subset morpho summary to only include species from analysis regions
summ <- summ[summ$SPECIES_DESC %in% unique(log$Region_class),]

# analysis regions classes
unique(log$Region_class[log$Region_type == " Analysis"])



##########################################################################################
##########################################################################################
# prep NASC

# sum nasc by region by cell by interval (across depth bins)
int_regions <- ddply(nasc, .(Region_class, Region_ID, Region_name, Lat_S, Lon_S, Interval, EV_filename), 
             summarise,
             NASC = sum(PRC_NASC),
             Date = head(Date_S,1),
             Time = head(Time_S,1))

# sum nasc by cell by interval (across depth bins)
int_cells <- ddply(nasc_cells, .(Lat_S, Lon_S, Interval, EV_filename),
                   summarise,
             NASC = sum(NASC),
             Date = head(Date_S,1),
             Time = head(Time_S,1))

# remove bad data
int_cells <- int_cells[!int_cells$Lat_S == 999,]


#plot regions
ggplot(data = int_regions, aes(x = Lon_S, y = Lat_S, colour = Region_class))+
  geom_point()+
  theme_bw()
  
# check for off transect intervals
# plot cells
ggplot(data = int_cells, aes(x = Lon_S, y = Lat_S))+
  #geom_point()+
  geom_text(aes(label = Interval), size = 3)+
  theme_bw()



# add extra columns to cell nasc
int_cells$Region_class <- "None"
int_cells$Region_ID <- 0
int_cells$Region_name <- "None"

# remove records from cell nasc that match region intervals
int_cells <- int_cells[!(int_cells$Interval %in% int_regions$Interval),]

# bind region nasc and 'empty' cell nasc dataframes
int <- rbind(int_regions, int_cells)
int$Source <- c(rep("region",1080),rep("cell",452))
  
# plot all cells
ggplot(data = int, aes(x = Lon_S, y = Lat_S, colour = Source))+
  geom_point()+
  #geom_text(aes(label = Interval), size = 1)+
  theme_bw()

# change file name into transect
int$Transect <- substr(int$EV_filename, 63, 67)
int <- int[,c( "Date", "Time", "Interval","Lat_S","Lon_S","Region_class","Region_ID","Region_name",   
               "NASC","Source","Transect")]

##########################################################################################
##########################################################################################
# total distance travelled in survey

# order
int <- int[order(int$Interval),]

# distance between lat long points in nmi
earth.dist <- function (long1, lat1, long2, lat2) {
  rad <- pi/180
  a1 <- lat1 * rad
  a2 <- long1 * rad
  b1 <- lat2 * rad
  b2 <- long2 * rad
  dlon <- b2 - a2
  dlat <- b1 - a1
  a <- (sin(dlat/2))^2 + cos(a1) * cos(b1) * (sin(dlon/2))^2
  c <- 2 * atan2(sqrt(a), sqrt(1 - a))
  R <- 6378.145
  d <- R * c * 0.53996
  return(d)
}

# loop through intervals in order
for (j in 1:nrow(int)) {
int$dist[j] <-  earth.dist(int$Lon_S[j], int$Lat_S[j], int$Lon_S[j+1], int$Lat_S[j+1])
}

# change last NA to zero
int[nrow(int),c("dist")] <- 0

# remove distance between transects
rem <- NULL
for (d in (1:nrow(int)-1)) {
ind <- int$Transect[d] != int$Transect[d+1]
rem[d] <- ind
}

int$dist[rem == TRUE] <- 0


# total distance covered on survey (in nautical miles)
sum(int$dist)




##########################################################################################
##########################################################################################
# biomass calculation

sp <- NULL
data <- NULL
for (s in summ$SPECIES_DESC){
  
dn <- int[int$Region_class == s,]
ds <- summ[summ$SPECIES_DESC == s,]
dc <- coeff[coeff$Region_class == s,]

name <- gsub( "\\s", "" , s)
sp[s] <- name

# Target strength by mean length (in cm)
TS <- dc$m * log10(ds$weigted.mean.length/10) + dc$b

# Backscattering cross-section 
sigma_bs <- 10 ^(TS/10)

# Denisty (#/nmi^2)
dn$density <- dn$NASC/ (4*pi*sigma_bs)

# Biomass (kg/nmi^2)
dn$biomass <- dn$density * (ds$mean.weight/1000)

# bind species data together
assign(name, dn)

}

# summaries
for (i in sp){
print(i)
print(summary(get(i)))
}



##########################################################################################
##########################################################################################
# Average biomass (kg per nmi^2) per species over the survey area 


# calculate the total distance (area) covered by regions for each species
# loop through intervals in order
for (j in 1:nrow(int)) {
  int$dist[j] <-  earth.dist(k$Lon_S[j], k$Lat_S[j], k$Lon_S[j+1], k$Lat_S[j+1])
}


avg <- ddply(data, .(Region_class), summarise, 
             Density = mean(density),
             Biomass = mean(biomass))
avg







