require(ggplot2)
require(plyr)

# set working directory 
setwd('..');setwd('..')



##########################################################################################
##########################################################################################
# load data

# load mean lengths and weight per species
summ <- read.csv("Other data/Fishing/morpho_summary.csv", header=T, stringsAsFactors = FALSE, row.names = 1)

# load mean lengths, weights and counts per set per species
morph <- read.csv("Other data/Fishing/morpho_counts.csv", header=T, stringsAsFactors = FALSE, row.names = 1)

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


#### Choose target strength - length regression constants (a or b estimtes)
con <- "a"




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
tempf <- unlist(strsplit(int_cells$EV_filename, "[.]EV"))
tempt <- unlist(strsplit(tempf, "/"))
steps <- length(tempt)/length(tempf)
int_cells$Transect <- tempt[(seq(steps,to=length(tempt),by=steps))]
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
tempf <- unlist(strsplit(int_regions$EV_filename, "[.]EV"))
tempt <- unlist(strsplit(tempf, "/"))
steps <- length(tempt)/length(tempf)
int_regions$Transect <- tempt[(seq(steps,to=length(tempt),by=steps))]
int_regions <- int_regions[,c( "Date", "Time", "Interval","Lat_S","Lon_S", "Region_class", "Region_ID", "Region_name","NASC", "Transect")]

# remove leading space
int_regions$Region_class <- sub( "\\s", "" , int_regions$Region_class)



##########################################################################################
##########################################################################################
# species of interest

# analysis regions classes
log$Region_class <- sub( "\\s", "" , log$Region_class)
reg <- unique(log$Region_class[log$Region_type == " Analysis"])
reg


# get analysis regions from log
analysis <- log[log$Region_type == " Analysis",]

# change file to transect
analysis$File <- strsplit(analysis$File, "[.]csv")
colnames(analysis)[16] <- "Transect"



##########################################################################################
##########################################################################################
# Update NASC for mixed regions based on TS and catch


## -- skip if there are no mixed regions


### Calculate nasc ratio
                      
# mixed regions
mixed <- c("Herring-Rockfish mix", "Hake mix")

# mixed regions from log
matched <- log[log$Region_class %in% mixed,c("Region_ID","Region_name","Region_class","File")]
matched$File <- unlist(strsplit(matched$File, "[.]csv"))
colnames(matched)[4] <- "Transect"

# add matching sets
matched$sets <- sub(".*set","",matched$Region_name) # remove everything before "set"
matched$sets <- sub("[A-z ]*","",matched$sets) # remove leading letters and spaces
matched$sets <- sub("-.*","",matched$sets) # remove everything after dash
matched$sets <- sub("*.A","",matched$sets) # remove everything before A
matched

# mean weight, length and count data for mixed sets
mat <- morph[morph$SET %in% matched$sets,]

# merge coeff data with mat
mat <- merge(mat, coeff[coeff$choice == con,c(1,2,3,7)], by.x = "SPECIES_DESC", by.y = "Region_class")

# calculate target strength to partition nasc
mat$TS <- mat$m * log10((mat$mean.length.mm*mat$convert)/10) + mat$b

# calculate backscatter cross-section to partition nasc
mat$sigma_bs <- 10 ^(mat$TS/10)

# calculate  sigma_bs  * N
mat$bsn <- mat$Estimated_N * mat$sigma_bs 

# calculate the ratio to partition nasc
mat <- ddply(mat, .(SET), transform, sum_bsn = sum(bsn))
mat$R <- mat$bsn/ mat$sum_bsn
mat

## check - sums should equal to 1 for each set
ddply(mat, .(SET), summarise, sums = sum(R))



### calculate NASC based on ratio

# get nasc data for mixed regions only
int_mix <- int_regions[int_regions$Region_class %in% mixed,]

# add set number to int_mix
int_sets <- merge(int_mix, matched, by=c("Region_ID", "Region_name", "Region_class", "Transect"))

# check for the same length
dim(int_mix)
dim(int_sets)


# calculate percent of nasc attributed to each species in catch
mix_data <- NULL
for (y in unique(int_sets$sets)){
  # partiton data by reference set
    rat <- mat[mat$SET == y,]
    int_s <- int_sets[int_sets$sets == y,]
          # create table for each species of mix
          for (z in rat$SPECIES_DESC){
            pdat <- int_s
            pdat$Region_class <- z
            pdat$NASC <- pdat$NASC * (rat$R[rat$SPECIES_DESC == z])
            # bind species tables together
            mix_data <- rbind(mix_data, pdat)
          }
}

## check - percent of NASC which remains - should be 100%
100 - (sum(int_mix$NASC) - sum(mix_data$NASC))/sum(int_mix$NASC) *100




### append int_region data with partitioned regions

# remove mixed regions from int_regions data
int_regions <- int_regions[!int_regions$Region_class %in% mixed,]

# add partitoned regions back into int_regions data
int_regions <- rbind(int_regions, mix_data[names(int_regions)])




##########################################################################################
##########################################################################################
# Update mixed anlaysis regions in log


## -- skip if there are no mixed regions


# merge matched sets with analysis
man <- merge(analysis, matched, by=c("Region_ID", "Region_name", "Region_class", "Transect"))

# add new classes for mixed regions
mix_reg <- NULL
for (y in unique(man$sets)){
  # partiton data by reference set
  sp_sets <- mat[mat$SET == y,]
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
summary(int_cells$dist)

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

# only species in updated analysis regions
summ <- summ[summ$SPECIES_DESC %in% regions$Region_class,]

# merge nasc and morpho data
im <- merge(int_regions, summ, by.x = c("Region_class"), by.y = c("SPECIES_DESC"))

## check - the same length unless region aren't in fishing data -> e.g. CPS
table(int_regions$Region_class)
dim(int_regions)
dim(im)

# merge coeff data with him
imc <- merge(im, coeff[coeff$choice == con,c(1,2,3,7)], by = "Region_class")


# biomass calculations

# Target strength by mean length (in cm)
imc$TS <- imc$m * log10((imc$weigted.mean.length.mm*imc$convert)/10) + imc$b

# Backscattering cross-section 
imc$sigma_bs <- 10 ^(imc$TS/10)

# Denisty (#/nmi^2)
imc$density <- imc$NASC/ (4*pi*imc$sigma_bs)

# Biomass (kg/nmi^2)
imc$biomass <- imc$density * (imc$mean.weight.kg)



##########################################################################################
##########################################################################################
# Average biomass 


# average biomass (kg per nmi^2) per species over their regions
tmp <- ddply(imc, .(Region_class), summarise, 
             Density = mean(density),
             Biomass = mean(biomass))


# merge with proportion of survey data
mrg <- merge(tmp, regions, by = "Region_class")


# average biomass (kg per nmi^2) per species over the survey area 
assign(paste("avg", con, sep="_"), data.frame(Region_class = mrg$Region_class,
                                    Estimate = con,
                                    Density.sqnmi = mrg$Density * mrg$proportion,
                                    Biomass.kg.sqnmi = mrg$Biomass * mrg$proportion))
get(paste("avg", con, sep="_"))





# ---------------------------------------------------------------------#
# RERUN script with other estimates for TS-length regression constants

# bind density and biomass from difference estimates
avg <- rbind(avg_a,avg_b)
avg <- avg[order(avg$Region_class),]

# scale up biomass to area of interest
# area of survey (spatial extent) -> for Laperouse 2015 -> 2300 sq nmi
avg$Total.Biomass.kg <- round(avg$Biomass.kg.sqnmi * 2300)
avg

# exports
write.csv(avg, file = "Other data/Fishing/Biomass.csv")


# standard deviation of biomass estimates
SD <- aggregate(Biomass.kg.sqnmi ~ Region_class, sd, data = avg)
SD

