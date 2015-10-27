require(ggplot2)
require(grid)
require(scales)
require(plyr)
require(PBSmapping)

# set working directory 
setwd('..');setwd('..')



##########################################################################################
##########################################################################################
# load data

# load mean lengths and weight per species
summs <- read.csv("Other data/Catch data/morpho_summary.csv", header=T, stringsAsFactors = FALSE, 
                  row.names = 1)

# load mean lengths, weights and counts per set per species
morph <- read.csv("Other data/Catch data/morpho_counts.csv", header=T, stringsAsFactors = FALSE, 
                  row.names = 1)

# load backscatter data
nasc <- read.csv("Acoustics/Echoview/Exports/Sv raw pings T2/IntegratedByRegionsByCells.csv", header=T, 
                 stringsAsFactors = FALSE, row.names = 1)
nasc_cells <- read.csv("Acoustics/Echoview/Exports/Sv raw pings T2/IntegratedByCells.csv", header=T, 
                       stringsAsFactors = FALSE, row.names = 1)

# load echoview log
logs <- read.csv("Acoustics/Echoview/Exports/Log/Cruiselog.csv", header=T, stringsAsFactors = FALSE, row.names=1)
colnames(logs)[1] <- "Region_ID"

# load target strength coefficients
coeff <- read.csv("Rscripts/Trawls/TS_coefficients.csv", header=T, stringsAsFactors = FALSE)



##########################################################################################
##########################################################################################
# NASC DATAFRAMES


## FOR CELL EXPORTS

# sum nasc by cell by interval (across depth bins)
int_cell <- ddply(nasc_cells, .(Lat_S, Lon_S, Lat_E, Lon_E, Interval, EV_filename),
                  summarise,
                  NASC = sum(NASC),
                  Date = head(Date_S,1),
                  Time = head(Time_S,1))

# remove bad data
int_cell <- int_cell[!int_cell$Lat_S == 999,]
int_cell <- int_cell[!int_cell$Lat_E == 999,]


# check for off transect intervals
# plot cells
ggplot(data = int_cell, aes(x = Lon_S, y = Lat_S))+
  geom_point()+
  #geom_text(aes(label = Interval), size = 3)+
  theme_bw()

# add transect
int_cell$Transect <-  sub(".*/", "", int_cell$EV_filename)  # remove everything before /
int_cell$Transect <-  sub(".EV", "", int_cell$Transect) # removes '.EV'
int_cell <- int_cell[,c( "Date", "Time", "Interval","Lat_S","Lon_S","Lat_E","Lon_E", "NASC","Transect")]



## FOR REGION EXPORTS

# sum nasc by region by cell by interval (across depth bins)
int_region <- ddply(nasc, .(Region_class, Region_ID, Region_name, 
                            Lat_S, Lon_S, Lat_E, Lon_E, Interval, EV_filename), 
                    summarise,
                    NASC = sum(PRC_NASC),
                    Date = head(Date_S,1),
                    Time = head(Time_S,1))


# check for bad data
int_region[int_region$Lat_S == 999,]
int_region[int_region$Lat_E == 999,]

#plot regions
ggplot(data = int_region, aes(x = Lon_S, y = Lat_S, colour = Region_class))+
  geom_point()+
  theme_bw()


# change file name into transect
int_region$Transect <-  sub(".*/", "", int_region$EV_filename) # remove everything before /
int_region$Transect <-  sub(".EV", "", int_region$Transect)  # removes '.EV'
int_region <- int_region[,c( "Date", "Time", "Interval","Lat_S","Lon_S", "Lat_E","Lon_E","Region_class", "Region_ID", "Region_name","NASC", "Transect")]

# remove leading space
int_region$Region_class <- sub( "\\s", "" , int_region$Region_class)



############################################################################
############################################################################
## ADD Transects and Replicates

logs$Transect <- sub(".csv", "", logs$File)
logs$Replicate <- sub("T", "", logs$Transect)
logs$Replicate <- as.numeric(sub("[.].*", "", logs$Replicate))

int_region$Replicate <-  sub("T", "", int_region$Transect)
int_region$Replicate <-  as.numeric(sub("[.].*", "", int_region$Replicate))

int_cell$Replicate <-  sub("T", "", int_cell$Transect)
int_cell$Replicate <-  as.numeric(sub("[.].*", "", int_cell$Replicate))




############################################################################
############################################################################
## LOOP THROUGH SURVEYS, AREAS OR REPLICATES and ESIMTATES

biomass.df <- NULL
distance.df <- NULL
options <- c("Replicate1","Replicate2")
con <- c("a", "b") # target strength - length regression constants

for (i in options){
  
  for (e in con){
    
    
    # 1) for replicate 1
    if(i == "Replicate1")  {
      log <- logs[logs$Replicate == 1,]
      int_regions <- int_region[int_region$Replicate == 1,]
      int_cells <- int_cell[int_cell$Replicate == 1, ] 
      summ <- summs[summs$Replicate == 1, ]
    }
    
    # 2) for replicate 2
    if(i == "Replicate2")  {
      log <- logs[logs$Replicate == 2,]
      int_regions <- int_region[int_region$Replicate == 2,]
      int_cells <- int_cell[int_cell$Replicate == 2, ] 
      summ <- summs[summs$Replicate == 2, ]
    }
    
    
    
    
    
    
    ##########################################################################################
    ##########################################################################################
    # SPECIES 
    
    # analysis regions classes
    log$Region_class <- sub( "\\s", "" , log$Region_class)
    reg <- unique(log$Region_class[log$Region_type == " Analysis"])
    reg
    
    # get analysis regions from log
    analysis <- log[log$Region_type == " Analysis",]
    
    
    
    
    
    ##########################################################################################
    ##########################################################################################
    # MIXED REGIONS
    
    
    ##### 1) Calculate ratio
    
    # mixed regions
    mixed <- c("Herring-Rockfish mix")
    
    # mixed regions from log
    matched <- log[log$Region_class %in% mixed,c("Region_ID","Region_name","Region_class","Transect")]
    
    # add matching sets
    matched$SET <- sub(".*set","",matched$Region_name) # remove everything before "set"
    matched$SET <- sub("[A-z ]*","",matched$SET) # remove leading letters and spaces
    matched$SET <- sub("-.*","",matched$SET) # remove everything after dash
    matched$SET <- sub(".*A","",matched$SET) # remove everything before A
    
    # mean weight, length and count data for mixed sets
    mat <- morph[morph$SET %in% matched$SET,]
    
    # merge coeff data with mat
    mat <- merge(mat, coeff[coeff$choice == e,c(1,2,3,7)], by.x = "SPECIES_DESC", by.y = "Region_class")
    
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
    
    
    
    ##### 2) Calculate NASC based on ratio
    
    # get nasc data for mixed regions only
    int_mix <- int_regions[int_regions$Region_class %in% mixed,]
    
    # add set number to int_mix
    int_sets <- merge(int_mix, matched, by=c("Region_ID", "Region_name", "Region_class", "Transect"))
    
    # check for the same length
    dim(int_mix)
    dim(int_sets)
    
    # partition NASC
    mix_data <- merge(int_sets, mat[,!names(mat) %in% "Replicate"], by="SET")
    mix_data$Region_class <- mix_data$SPECIES_DESC
    mix_data$NASC <- mix_data$NASC * mix_data$R
    
    ## check - is 100% of NASC remaining?
    round(sum(int_mix$NASC)) == round(sum(mix_data$NASC))
    
    
    
    ##### 3) Update int_regions with new NASC 
    
    # remove mixed regions from int_regions data
    int_regions <- int_regions[!int_regions$Region_class %in% mixed,]
    
    # add partitoned regions back into int_regions data
    int_regions <- rbind(int_regions, mix_data[names(int_regions)])
    
    
    
    #### 4) Update log with new analysis regions 
    
    # merge analysis with matched sets and mat
    man <- merge(analysis, matched, by=c("Region_ID", "Region_name", "Region_class", "Transect"))
    mix_reg <- merge(man, mat[,!names(mat) %in% "Replicate"], by="SET")
    mix_reg$Region_class <- mix_reg$SPECIES_DESC
    
    # remove mixed regions from analysis
    analysis <- analysis[!analysis$Region_class %in% mixed,]
    
    # add partitoned regions back into  analysis
    analysis <- rbind(analysis, mix_reg[names(analysis)])
    
    # check analysis regions 
    table(analysis$Region_class)
    
    
    
    
    
    
    ##########################################################################################
    ##########################################################################################
    # total distance travelled in survey
    
    
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
    
    
    # calculate the distance covered by each transect
    transects <- ddply(int_cells, .(Transect), function(x) 
      data.frame(trans_dist=sum(earth.dist(x$Lon_S, x$Lat_S, x$Lon_E, x$Lat_E))))
    
    
    # calculate the distance covered by regions for each species
    regions <- ddply(int_regions, .(Region_class, Transect), function(x) 
      data.frame(reg_dist=sum(earth.dist(x$Lon_S, x$Lat_S, x$Lon_E, x$Lat_E))))
    
    # proportion of transect that regions covered
    distance <- merge(transects, regions, by = "Transect")
    distance$reg_prop <- distance$reg_dist/distance$trans_dist
    distance$reg_prop <- distance$reg_dist/distance$trans_dist
    distance$trans_prop <- distance$trans_dist/total
    distance
    
    # distance table
    distance$loc <- i
    distance.df <- rbind(distance.df, distance)
    
    
    
    
    ##########################################################################################
    ##########################################################################################
    # OTHER BIOMASS CALCULATIONS
   
    # check for species missing in catch data
    summ$SPECIES_DESC
    unique(int_regions$Region_class)
    missing <- unique(int_regions$Region_class)[!(unique(int_regions$Region_class) %in% summ$SPECIES_DESC)]
    
    # get missing species from summs if necessary
    summ_miss <- summs[summs$SPECIES_DESC %in% missing,]
    summ <- rbind(summ, summ_miss)
    
    # merge non-hake nasc and morpho data
    im <- merge(int_regions, summ[,!names(summ) %in% "Replicate"], 
                 by.x = c("Region_class"), by.y = c("SPECIES_DESC"))
    
    ## check - the same length unless region aren't in fishing data -> CPS, myctophids, etc.
    dim(int_regions)
    dim(im)
    
    # merge coeff data with him
    imc <- merge(im, coeff[coeff$choice == e,c(1,2,3,7)], by = "Region_class")
    
    # Target strength by mean length (in cm)
    imc$TS <- imc$m * log10((imc$weigted.mean.length.mm*imc$convert)/10) + imc$b
    
    # Backscattering cross-section 
    imc$sigma_bs <- 10 ^(imc$TS/10)
    
    # Denisty (#/nmi^2)
    imc$density <- imc$NASC/ (4*pi*imc$sigma_bs)
    
    # Biomass (kg/nmi^2)
    imc$biomass <- imc$density * (imc$mean.weight.kg)
    
    # summary
    summary(imc)
    
    
    
    
    
    
    ##########################################################################################
    ##########################################################################################
    # JOIN BIOMASS TABLES FROM LOOPS --- END 
    
    #bind hake and other species in one table
    headers <- c("Region_class","Region_ID","Region_name","Transect","Date","Time","Interval","Lat_S","Lon_S","NASC","TS","sigma_bs","density","biomass")
    bio <- imc[headers]
    bio$loc <- i
    bio$estimate <- e
    
    biomass.df <- rbind(biomass.df, bio)
    
  }
}

# check 
head(biomass.df)
table(biomass.df$loc)
table(biomass.df$estimate)


# check
ddply(int_region, .(Replicate,Region_class), summarise, n = length(Region_class))
ddply(biomass.df[biomass.df$estimate == "a",], .(loc,Region_class), summarise, n = length(Region_class))





##########################################################################################
##########################################################################################
# BIOMASS SUMMARY 



# average biomass (kg per nmi^2) per species per transect over their regions
tmp <- ddply(biomass.df, .(loc, Region_class, estimate, Transect), summarise, 
             Density = mean(density),
             Biomass = mean(biomass))

# merge with distance data
distance.df <- distance.df[!duplicated(distance.df),]
mrg <- merge(tmp, distance.df, by = c("loc", "Region_class", "Transect"))

# average transect density and biomass (/sq nmi), weighted by region size
mrg$Density <- mrg$Density * mrg$reg_prop
mrg$Biomass <- mrg$Biomass * mrg$reg_prop  


# average survey biomass (kg/sq nmi) , weighted by transect size
avg <- ddply(mrg, .(loc, Region_class, estimate), summarise, 
             Density.sqnmi = sum(Density * trans_prop),
             Biomass.kg.sqnmi = sum(Biomass * trans_prop))


# scale up biomass to area of interest (spatial extent) 
# for replicates = 1900
avg$Area.Biomass.kg <- NA
avg$Area.Biomass.kg[avg$loc == "Replicate1"] <- avg$Biomass.kg.sqnmi[avg$loc == "Replicate1"] * 1900
avg$Area.Biomass.kg[avg$loc == "Replicate2"] <- avg$Biomass.kg.sqnmi[avg$loc == "Replicate2"] * 1900

avg



#######  export #######
write.csv(avg, file = "Other data/Catch data/Biomass.csv")






##########################################################################################
##########################################################################################
# BIOMASS MAPS

#data
bio.rep <- biomass.df

#land Polygon
data(nepacLL)
lp<-data.frame(nepacLL)

## set colour palette
#cols <- c("#00DDFF", "#FCDF00", "#4daf4a", "#ff7f00", "#984ea3", "#377eb8")
cols <- c("#00DDFF", "#FCDF00", "#ff7f00", "#984ea3", "#377eb8")
lg <- length(unique(bio$Region_class))
leg <- ifelse(lg > length(cols), length(cols), lg)
pal <- colorRampPalette(cols[1:leg],space = c("rgb"),interpolate = c("spline"))(lg)


# replicate map
repmap <- ggplot(data = NULL) +
  geom_polygon(data = lp, aes(x = X, y = Y, group=PID), 
               fill = "gray80", colour = "gray60", size = .1) +
  geom_point(data = bio.rep[bio.rep$estimate == "a",], 
             aes(x = Lon_S, y = Lat_S, colour=Region_class, fill=Region_class, size = biomass/1000), 
             pch = 21, alpha=.6) +
  facet_wrap(~loc, ncol=2) +
  scale_size_area(max_size = 18, name = "Biomass (T)")+
  scale_colour_manual(values=pal, name = "") +
  scale_fill_manual(values=pal, name = "") +
  guides(colour = guide_legend(override.aes = list(size=4), ncol=1))+
  labs(x = "Longitude", y = "Latitude") +
  coord_map(xlim = c(min(bio.rep$Lon_S)-.1, max(bio.rep$Lon_S)+.1), 
            ylim = c(min(bio.rep$Lat_S)-.1, max(bio.rep$Lat_S)+.1)) +
  theme(panel.border = element_rect(fill=NA, colour="black"),
        panel.background = element_rect(fill="white",colour="black"),
        strip.background = element_blank(),
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
        legend.justification = c(.5,.5), legend.position = "bottom", legend.box = "horizontal",
        plot.margin = unit(c(.5,.5,.5,.5), "lines")) # top, right, bottom, and left 

repmap
pdf("Other data/Figures/BiomassMap.pdf", width = 11, height = 7)
repmap
dev.off()



##########################################################################################
##########################################################################################
# COMPARE TOTAL BIOMASS

# estimates/replicates plot
comp_est <- ggplot(data = avg) + 
  geom_bar(aes(x = Region_class, y = Area.Biomass.kg/1000, group = estimate, fill = Region_class),
           stat= "identity", position = "dodge", colour = "black", size = .1) +
  geom_text(aes(x=Region_class, y = Area.Biomass.kg/1000, group = estimate, label = estimate), 
            position = position_dodge(width=1), size = 2.5, vjust = -.3) +
  facet_wrap(~loc) +
  labs(x="", y=" Total Biomass (T)") +
  scale_fill_manual(values = pal, guide="none") +
  scale_y_continuous(label=comma, expand =c(0,0), 
                     limit= c(0,max(avg$Area.Biomass.kg/1000)*1.05))+
  # themes
  theme(panel.border = element_rect(fill=NA, colour="black", size = .1),
        panel.background = element_rect(fill="white",colour="white"),
        strip.background = element_blank(),
        axis.ticks = element_line(colour="black"),
        axis.ticks.length = unit(0.1,"cm"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size=8, colour = "black"),
        axis.title = element_text(size=9, colour = "black"),
        plot.margin = unit(c(.2,.2,.2,.2), "lines")) # top, right, bottom, and left
comp_est
pdf("Other data/Figures/Biomass.pdf", width = 8, height = 4)
comp_est
dev.off()




