require(ggplot2)
require(plyr)

# set working directory 
setwd('..');setwd('..')



##########################################################################################
##########################################################################################
# load data

# load mean lengths and weight
summ <- read.csv("Other data/Fishing/morpho_summary.csv", header=T, stringsAsFactors = FALSE, row.names = 1)

# load backscatter data
nasc <- read.csv("Acoustics/Echoview/Exports/Sv raw pings T2/IntegratedByRegionsByCells.csv", header=T, 
                 stringsAsFactors = FALSE, row.names = 1)
nasc_cells <- read.csv("Acoustics/Echoview/Exports/Sv raw pings T2/IntegratedByCells.csv", header=T, 
                       stringsAsFactors = FALSE, row.names = 1)

# load echoview log
log <- read.csv("Other data/Log/Cruiselog.csv", header=T, stringsAsFactors = FALSE, row.names=1)
colnames(log)[1] <- "Region_ID"

# load target strength coefficients
coeff <- read.csv("EchoviewR/Trawls/TS_coefficients.csv", header=T, stringsAsFactors = FALSE)

# load catch data 
catch <- read.csv("Other data/Fishing/catch_summary.csv", header=T, stringsAsFactors = FALSE, row.names = 1)




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





##########################################################################################
##########################################################################################
# Update NASC for mixed analysis regions

# analysis regions classes
reg <- unique(log$Region_class[log$Region_type == " Analysis"])
reg



### ----------------------------------------------------------------------------- ###
### ----------------------------------------------------------------------------- ###
#                         ADAPT REGIONS AND MATCHING SETS                           #
### ----------------------------------------------------------------------------- ###
### ----------------------------------------------------------------------------- ###

# mixed regions
mixed <- c(" Herring-Rockfish mix", " Hake mix")

# add sets
matched <- log[log$Region_class %in% mixed,c("Region_ID","Region_name","Region_class","File")]
matched$File <- substring(matched$File, 1,5)
colnames(matched)[4] <- "Transect"
matched$sets <- c(14,29,29,29,29)

# catch data for mixed sets
cats <- catch[catch$SET %in% matched$sets,]

# change species names to reflect analysis region names
cats$SPECIES_DESC[grep("rockfish", cats$SPECIES_DESC, ignore.case=T)] <- " Rockfish"
cats$SPECIES_DESC[grep("herring", cats$SPECIES_DESC, ignore.case=T)] <- " Herring"
cats$SPECIES_DESC[grep("hake", cats$SPECIES_DESC, ignore.case=T)] <- " Hake"


### ----------------------------------------------------------------------------- ###
### ----------------------------------------------------------------------------- ###
### ----------------------------------------------------------------------------- ###



# only include region species
cats <- cats[cats$SPECIES_DESC %in% reg,]

# group species
cats <-ddply(cats, .(SET, SPECIES_DESC), summarise, PERCENT = sum(PERCENT))

# for each region class, region id and transect create a seperate table
int_mix <- int_regions[int_regions$Region_class %in% mixed,]

# merge matched sets with int_mix
int_sets <- merge(int_mix, matched, by=c("Region_ID", "Region_name", "Region_class", "Transect"))

# check for the same length
dim(int_mix)
dim(int_sets)


# calculate percent of nasc attributed to each species in catch
mix_data <- NULL
for (y in unique(int_sets$sets)){
  # partiton data by reference set
    per <- cats[cats$SET == y,]
    int_per <- int_sets[int_sets$sets == y,]
          # create table for each species of mix
          for (z in per$SPECIES_DESC){
            pdat <- int_per
            pdat$Region_class <- z
            pdat$NASC <- pdat$NASC * (per$PERCENT[per$SPECIES_DESC == z]/100)
            # bind species tables together
            mix_data <- rbind(mix_data, pdat)
          }
}

# check - percent of NASC which remains
100 - (sum(int_mix$NASC) - sum(mix_data$NASC))/sum(int_mix$NASC) *100

# should be close to percent of catch
sum(cats$PERCENT)/(length(unique(cats$SET))*100) *100



## remove mixed regions from int_regions data
int_regions <- int_regions[!int_regions$Region_class %in% mixed,]

## add partitoned regions back into int_regions data
int_regions <- rbind(int_regions, mix_data[names(int_regions)])




##########################################################################################
##########################################################################################
# Update mixed anlaysis regions in log

# get analysis regions from log
analysis <- log[log$Region_type == " Analysis",]

# change file to transect
analysis$File <- substring(analysis$File, 1,5)
colnames(analysis)[16] <- "Transect"

# merge matched sets with analysis
man <- merge(analysis, matched, by=c("Region_ID", "Region_name", "Region_class", "Transect"))

# add new classes for mixed regions
mix_reg <- NULL
for (y in unique(man$sets)){
  # partiton data by reference set
  sp_sets <- cats[cats$SET == y,]
  man_sets <- man[man$sets == y,]
  # create table for each species of mix
  for (z in sp_sets$SPECIES_DESC){
    pdat <- man_sets
    pdat$Region_class <- z
    # bind species tables together
    mix_reg <- rbind(mix_reg, pdat)
  }
}

## remove mixed regions from analysis
analysis <- analysis[!analysis$Region_class %in% mixed,]

## add partitoned regions back into  analysis
analysis <- rbind(analysis, mix_reg[names(analysis)])





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
# total distance travelled in regions


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





##########################################################################################
##########################################################################################
# biomass calculation

sp <- NULL
data <- NULL

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

    # estimate a or b?
      dn$estimate <- k
      
    # assign table
      assign(k,dn)
  }

# bind species data together
data <- rbind(data, a, b)

}

summary(data)



##########################################################################################
##########################################################################################
# Average biomass 


# average biomass (kg per nmi^2) per species over their regions
tmp <- ddply(data, .(Region_class, estimate), summarise, 
             Density = mean(density),
             Biomass = mean(biomass))


# merge with proportion of survey data
mrg <- merge(tmp, regions, by = "Region_class")


# average biomass (kg per nmi^2) per species over the survey area 
avg <- data.frame(Region_class = mrg$Region_class, 
                  Estimate = mrg$estimate,
                  Density = mrg$Density * mrg$proportion,
                  Biomass = mrg$Biomass * mrg$proportion)
avg


# standard deviation of biomass estimates
SD <- aggregate(Biomass ~ Region_class, sd, data = avg)
SD

