require(ggplot2)
require(grid)
require(scales)
require(plyr)

# set working directory 
setwd('..');setwd('..')



##########################################################################################
##########################################################################################
# load data

# load mean lengths and weight per species
summall <- read.csv("Other data/Fishing/morpho_summary.csv", header=T, stringsAsFactors = FALSE, row.names = 1)

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

#### Choose replicate
reps <- 2



##########################################################################################
##########################################################################################
# prep NASC

## FOR CELL EXPORTS

# sum nasc by cell by interval (across depth bins)
int_cells <- ddply(nasc_cells, .(Lat_S, Lon_S, Lat_E, Lon_E, Interval, EV_filename),
                   summarise,
             NASC = sum(NASC),
             Date = head(Date_S,1),
             Time = head(Time_S,1))

# remove bad data
int_cells <- int_cells[!int_cells$Lat_S == 999,]
int_cells <- int_cells[!int_cells$Lat_E == 999,]


# check for off transect intervals
# plot cells
ggplot(data = int_cells, aes(x = Lon_S, y = Lat_S))+
  geom_point()+
  #geom_text(aes(label = Interval), size = 3)+
  theme_bw()

# add transect
int_cells$Transect <-  sub(".*/", "", int_cells$EV_filename)  # remove everything before /
int_cells$Transect <-  sub(".EV", "", int_cells$Transect) # removes '.EV'
int_cells <- int_cells[,c( "Date", "Time", "Interval","Lat_S","Lon_S","Lat_E","Lon_E", "NASC","Transect")]



## FOR REGION EXPORTS

# sum nasc by region by cell by interval (across depth bins)
int_regions <- ddply(nasc, .(Region_class, Region_ID, Region_name, 
                             Lat_S, Lon_S, Lat_E, Lon_E, Interval, EV_filename), 
                     summarise,
                     NASC = sum(PRC_NASC),
                     Date = head(Date_S,1),
                     Time = head(Time_S,1))

#plot regions
ggplot(data = int_regions, aes(x = Lon_S, y = Lat_S, colour = Region_class))+
  geom_point()+
  theme_bw()

# check for bad data
int_regions[int_regions$Lat_S == 999,]
int_regions[int_regions$Lat_E == 999,]

# change file name into transect
int_regions$Transect <-  sub(".*/", "", int_regions$EV_filename) # remove everything before /
int_regions$Transect <-  sub(".EV", "", int_regions$Transect)  # removes '.EV'
int_regions <- int_regions[,c( "Date", "Time", "Interval","Lat_S","Lon_S", "Lat_E","Lon_E","Region_class", "Region_ID", "Region_name","NASC", "Transect")]

# remove leading space
int_regions$Region_class <- sub( "\\s", "" , int_regions$Region_class)




###############################
## ADD Transects and Replicates

log$Transect <- sub(".csv", "", log$File)
log$Replicate <- sub("T", "", log$Transect)
log$Replicate <- as.numeric(sub("[.].*", "", log$Replicate))

int_regions$Replicate <-  sub("T", "", int_regions$Transect)
int_regions$Replicate <-  as.numeric(sub("[.].*", "", int_regions$Replicate))

int_cells$Replicate <-  sub("T", "", int_cells$Transect)
int_cells$Replicate <-  as.numeric(sub("[.].*", "", int_cells$Replicate))





##### -------------------------------------------------------------------------------####  
##### -------------------------------------------------------------------------------####  


## repeat script once for each survey or replicate combo
log <- log[log$Replicate == reps,]
int_regions <- int_regions[int_regions$Replicate == reps,] 
int_cells <- int_cells[int_cells$Replicate == reps,] 
morph <- morph[morph$Replicate == reps,] 
summ <- summall[summall$Replicate == reps,] 


#plot regions
ggplot(data = int_regions, aes(x = Lon_S, y = Lat_S, colour = Region_class))+
  geom_point()+
  theme_bw()




##########################################################################################
##########################################################################################
# species of interest

# analysis regions classes
log$Region_class <- sub( "\\s", "" , log$Region_class)
reg <- unique(log$Region_class[log$Region_type == " Analysis"])
reg


# get analysis regions from log
analysis <- log[log$Region_type == " Analysis",]






##########################################################################################
##########################################################################################
# Update NASC for mixed regions based on TS and catch


## -- skip if there are no mixed regions


### Calculate nasc ratio
                      
# mixed regions
mixed <- c("Herring-Rockfish mix")

# mixed regions from log
matched <- log[log$Region_class %in% mixed,c("Region_ID","Region_name","Region_class","File","Transect")]


# add matching sets
matched$sets <- sub(".*set","",matched$Region_name) # remove everything before "set"
matched$sets <- sub("[A-z ]*","",matched$sets) # remove leading letters and spaces
matched$sets <- sub("-.*","",matched$sets) # remove everything after dash
matched$sets <- sub(".*A","",matched$sets) # remove everything before A
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





### append mixed anlaysis regions in log with partitioned regions


# merge matched sets with analysis
man <- merge(analysis, matched, by=c("Region_ID", "Region_name", "Region_class", "Transect", "File"))

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
# total distance travelled of survey and regions


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


# calculate the total distance (linear transect area) covered by each cell
int_cells$dist <-  earth.dist(int_cells$Lon_S, int_cells$Lat_S, int_cells$Lon_E, int_cells$Lat_E)
total <- sum(int_cells$dist)
total



# calculate the total distance (linear transect area) covered by regions for each species
dat <- NULL
regions <- NULL
for (p in unique(int_regions$Region_class)){
  da <- int_regions[int_regions$Region_class == p,]
  # loop through intervals in order
  for (l in 1:nrow(da)) {
    da$dist[l] <-  earth.dist(da$Lon_S[l], da$Lat_S[l], da$Lon_E[l], da$Lat_E[l])
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

# check if all species in int data are present in catch data
summ$SPECIES_DESC
regions$Region_class

# only species in updated analysis regions
summ <- summ[summ$SPECIES_DESC %in% regions$Region_class,]

# get missing species from summall if necessary
missing <- regions$Region_class[!(regions$Region_class %in% summ$SPECIES_DESC)]
summ_miss <- summall[summall$SPECIES_DESC %in% missing,]
summ <- rbind(summ, summ_miss)


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
assign(paste("avg", con, reps, sep="_"), data.frame(Region_class = mrg$Region_class,
                                    Estimate = con,
                                    Replicate = reps,
                                    Density.sqnmi = mrg$Density * mrg$proportion,
                                    Biomass.kg.sqnmi = mrg$Biomass * mrg$proportion))
get(paste("avg", con,  reps, sep="_"))





# ---------------------------------------------------------------------#
# RERUN script with other estimates for TS-length regression constants

# bind density and biomass from difference estimates
avg <- rbind(avg_a_1,avg_a_2, avg_b_1, avg_b_2)
avg <- avg[order(avg$Region_class),]

# scale up biomass to area of interest
# area of survey (spatial extent) -> for Laperouse 2015 - > 1900 sq nmi
avg$Total.Biomass.kg <- round(avg$Biomass.kg.sqnmi * 1900)
avg

# exports
write.csv(avg, file = "Other data/Fishing/Biomass.csv")





###################################################################################
# plot biomass comparison

comp_est <- ggplot(data = avg[avg$Replicate == 1,]) + 
  geom_bar(aes(x = Region_class, y = Total.Biomass.kg/1000, group = Estimate, fill = Region_class),
           stat= "identity", position = "dodge", colour = "black", size = .1) +
  geom_text(aes(x=Region_class, y = Total.Biomass.kg/1000, group = Estimate, label = Estimate), 
            position = position_dodge(width=1), size = 3, vjust = -.3) +
  labs(x="", y=" Total Biomass (mT)") +
  scale_fill_brewer(palette = "Set1", guide="none") +
  scale_y_continuous(label=comma, expand =c(0,0), limit= c(0,380000))+
  # themes
  theme(panel.border = element_rect(fill=NA, colour="black", size = .1),
        panel.background = element_rect(fill="white",colour="white"),
        strip.background = element_blank(),
        axis.ticks = element_line(colour="black"),
        axis.ticks.length = unit(0.1,"cm"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size=10, colour = "black"),
        axis.title = element_text(size=11, colour = "black"),
        plot.margin = unit(c(.2,.2,.2,.2), "lines")) # top, right, bottom, and left
pdf("Other data/Figures/Biomass_Coefficients.pdf", width = 8, height = 5)
comp_est
dev.off()



comp_rep <- ggplot(data = avg[avg$Estimate == "a",]) + 
        geom_bar(aes(x = Region_class, y = Total.Biomass.kg/1000, group = Replicate, fill = Region_class),
                 stat= "identity", position = "dodge", colour = "black", size = .1) +
        geom_text(aes(x=Region_class, y = Total.Biomass.kg/1000, group = Replicate, label = Replicate), 
                  position = position_dodge(width=1), size = 3, vjust = -.3) +
        labs(x="", y=" Total Biomass (mT)") +
        scale_fill_brewer(palette = "Set1", guide="none") +
        scale_y_continuous(label=comma, expand =c(0,0), limit= c(0,130000))+
      # themes
        theme(panel.border = element_rect(fill=NA, colour="black", size = .1),
        panel.background = element_rect(fill="white",colour="white"),
        strip.background = element_blank(),
        axis.ticks = element_line(colour="black"),
        axis.ticks.length = unit(0.1,"cm"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size=10, colour = "black"),
        axis.title = element_text(size=11, colour = "black"),
        plot.margin = unit(c(.2,.2,.2,.2), "lines")) # top, right, bottom, and left
pdf("Other data/Figures/Biomass_Replicates.pdf", width = 8, height = 5)
comp_rep
dev.off()




