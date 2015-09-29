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


## FOR REGION EXPORTS

# sum nasc by region by cell by interval (across depth bins)
int_regions <- ddply(nasc, .(Region_class, Region_ID, Region_name, Lat_S, Lon_S, Interval, EV_filename), 
             summarise,
             NASC = sum(PRC_NASC),
             Date = head(Date_S,1),
             Time = head(Time_S,1))

#plot regions
ggplot(data = int_regions, aes(x = Lon_S, y = Lat_S, colour = Region_class))+
  geom_point()+
  theme_bw()

# change file name into transect
int_regions$Transect <- substr(int_regions$EV_filename, 63, 67)
int_regions <- int_regions[,c( "Date", "Time", "Interval","Lat_S","Lon_S", "Region_class", "Region_ID", "Region_name","NASC", "Transect")]



## FOR CELL EXPORTS

# sum nasc by cell by interval (across depth bins)
int_cells <- ddply(nasc_cells, .(Lat_S, Lon_S, Interval, EV_filename),
                   summarise,
             NASC = sum(NASC),
             Date = head(Date_S,1),
             Time = head(Time_S,1))

# remove bad data
int_cells <- int_cells[!int_cells$Lat_S == 999,]

# check for off transect intervals
# plot cells
ggplot(data = int_cells, aes(x = Lon_S, y = Lat_S))+
  geom_point()+
  #geom_text(aes(label = Interval), size = 3)+
  theme_bw()

# change file name into transect
int_cells$Transect <- substr(int_cells$EV_filename, 63, 67)
int_cells <- int_cells[,c( "Date", "Time", "Interval","Lat_S","Lon_S", "NASC","Transect")]





##########################################################################################
##########################################################################################
# total distance travelled in survey

# order
int_cells <- int_cells[order(int_cells$Interval),]

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
for (j in 1:nrow(int_cells)) {
  int_cells$dist[j] <-  earth.dist(int_cells$Lon_S[j], int_cells$Lat_S[j], 
                                   int_cells$Lon_S[j+1], int_cells$Lat_S[j+1])
}

# change last NA to zero
int_cells[nrow(int_cells),c("dist")] <- 0

# remove distance between transects
rem <- NULL
for (d in (1:nrow(int_cells)-1)) {
ind <- int_cells$Transect[d] != int_cells$Transect[d+1]
rem[d] <- ind
}

int_cells$dist[rem == TRUE] <- 0

# remove distance where intervals are not sequential (e.g. between BT to RT)
rem <- NULL
for (d in (1:nrow(int_cells)-1)) {
  ind <- int_cells$Interval[d] != (int_cells$Interval[d+1] - 1)
  rem[d] <- ind
}

int_cells$dist[rem == TRUE] <- 0

# check -- should all be close to .5 nmi
summary(int_cells)

# total distance covered on survey (in nautical miles)
total <- sum(int_cells$dist)
total



##########################################################################################
##########################################################################################
# biomass calculation

sp <- NULL
data_a <- NULL
data_b <- NULL

for (s in summ$SPECIES_DESC){
  
dn <- int_regions[int_regions$Region_class == s,]
ds <- summ[summ$SPECIES_DESC == s,]
dc <- coeff[coeff$Region_class == s,]

name <- gsub( "\\s", "" , s)
sp[s] <- name

  for (k in c("a","b")){
    # Target strength by mean length (in cm)
      TS <- dc$m[dc$choice == k] * log10(ds$weigted.mean.length/10) + dc$b[dc$choice == k]

    # Backscattering cross-section 
      sigma_bs <- 10 ^(TS/10)

    # Denisty (#/nmi^2)
      dn$density <- dn$NASC/ (4*pi*sigma_bs)

    # Biomass (kg/nmi^2)
      dn$biomass <- dn$density * (ds$mean.weight/1000)

    # bind biomass estimates
      assign(k,dn)
  }

# bind species data together (one dataframe for each biomass estimate)
data_a <- rbind(data_a, a)
data_b <- rbind(data_b, b)
}

summary(data_a)
summary(data_b)


##########################################################################################
##########################################################################################
# Average biomass 


# get analysis regions from log
analysis <- log[log$Region_type == " Analysis",]

# calculate the total distance (linear transect area) covered by regions for each species
dat <- NULL
regions <- NULL
for (p in unique(analysis$Region_class)){
  da <- analysis[analysis$Region_class == p,]
         # loop through intervals in order
           for (l in 1:nrow(da)) {
               da$dist[l] <-  earth.dist(da$Lon_s[l], da$Lat_s[l], da$Lon_e[l], da$Lat_e[l])
           }
  dat <- data.frame(Region_class = p, distance = sum(da$dist))
  regions <- rbind(regions, dat)
}

# proportion of total survey that regions covered
regions$proportion <- regions$distance/total
regions



# average biomass (kg per nmi^2) per species over their regions
avg_a <- ddply(data_a, .(Region_class), summarise, 
             Density = mean(density),
             Biomass = mean(biomass))

avg_b <- ddply(data_b, .(Region_class), summarise, 
               Density = mean(density),
               Biomass = mean(biomass))

# merge with proportion of survey data
avg <- merge(avg_a, avg_b, by = "Region_class")
avg <- merge(avg, regions, by = "Region_class")


# average biomass (kg per nmi^2) per species over the survey area 
avg$Density.x <- avg$Density.x * avg$proportion
avg$Biomass.x <- avg$Biomass.x * avg$proportion
avg$Density.y <- avg$Density.y * avg$proportion
avg$Biomass.y <- avg$Biomass.y * avg$proportion


# biomass difference
avg$Biomass_diff <- abs(avg$Biomass.x - avg$Biomass.y)

# biomass (kg per nmi) estimates
avg

