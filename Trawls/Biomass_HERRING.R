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
summs <- read.csv("Other data/Catch data/morpho_summary.csv", 
                  header=T, stringsAsFactors = FALSE, row.names = 1)

# load lengths, weights and N by species by set
morph <- read.csv("Other data/Catch data/morpho_counts.csv", 
                   header=T, stringsAsFactors = FALSE, row.names = 1)

# load length frequencies
flengths <- read.csv("Other data/Catch data/Length_freq.csv", 
                  header=T, stringsAsFactors = FALSE, row.names = 1)

# load backscatter data
intbyreg <- read.csv("Acoustics/Echoview/Exports/Sv raw pings T2/IntegratedByRegionsByCells.csv"
                     ,header=T, stringsAsFactors = FALSE, row.names = 1)
intbycells <- read.csv("Acoustics/Echoview/Exports/Sv raw pings T2/IntegratedByCells.csv", 
                       header=T, stringsAsFactors = FALSE, row.names = 1)

# load echoview log
logs <- read.csv("Acoustics/Echoview/Exports/Log/Cruiselog.csv", 
                 header=T, stringsAsFactors = FALSE, row.names=1)
colnames(logs)[1] <- "Region_ID"

# load target strength coefficients
coeff <- read.csv("Rscripts/Trawls/TS_coefficients.csv", header=T, stringsAsFactors = FALSE)




##########################################################################################
##########################################################################################
## ADD Transects and Replicates

# change file name into transect
intbyreg$Transect <-  sub(".*/", "", intbyreg$EV_filename) # remove everything before /
intbyreg$Transect <-  sub(".EV", "", intbyreg$Transect)  # removes '.EV'

intbycells$Transect <-  sub(".*/", "", intbycells$EV_filename) # remove everything before /
intbycells$Transect <-  sub(".EV", "", intbycells$Transect)  # removes '.EV'


# remove leading space
intbyreg$Region_class <- sub( "\\s", "" , intbyreg$Region_class)
logs$Region_class <- sub( "\\s", "" , logs$Region_class)


# change transect into replicate
logs$Transect <- sub(".csv", "", logs$File)
logs$Replicate <- sub("T", "", logs$Transect)
logs$Replicate <- as.numeric(sub("[.].*", "", logs$Replicate))

intbyreg$Replicate <-  sub("T", "", intbyreg$Transect)
intbyreg$Replicate <-  as.numeric(sub("[.].*", "", intbyreg$Replicate))

intbycells$Replicate <-  sub("T", "", intbycells$Transect)
intbycells$Replicate <-  as.numeric(sub("[.].*", "", intbycells$Replicate))




##########################################################################################
##########################################################################################
# SPECIES 

# analysis regions classes
reg <- unique(intbyreg$Region_class)
reg

# mixed regions
mixed <- c("Herring-Rockfish mix")




############################################################################
############################################################################
## LOOP THROUGH SURVEYS, AREAS OR REPLICATES and ESIMTATES

# target strength - length regression constants
e <- "a" 

#assign final datasets
biocell.df <- NULL
bioint.df <- NULL


#replicates
reps <- c("Replicate1","Replicate2")


# loop
for (i in reps){
  
  # 1) for replicate 1
  if(i == "Replicate1")  {
    Log <- logs[logs$Replicate == 1,]
    nasc <- intbyreg[intbyreg$Replicate == 1,]
    nasc_cells <- intbycells[intbycells$Replicate == 1,]
    summ <- summs[summs$Replicate == 1, ]
    flength <- flengths[flengths$Replicate == 1, ]
  }
  
  # 2) for replicate 2
  if(i == "Replicate2")  {
    Log <- logs[logs$Replicate == 2,]
    nasc <- intbyreg[intbyreg$Replicate == 2,]
    nasc_cells <- intbycells[intbycells$Replicate == 2,]
    summ <- summs[summs$Replicate == 2, ]
    flength <- flengths[flengths$Replicate == 2, ]
  }
  
  
  
  
  
  ##########################################################################################
  ##########################################################################################
  # MIXED REGIONS
  
  if(any(mixed %in% unique(Log$Region_class))){
    
    ##### 1) Calculate ratio
    
    # mixed regions from log
    matched <- Log[Log$Region_class %in% 
                     mixed,c("Region_ID","Region_name","Region_class","Transect")]
    
    # add matching sets
    matched$SET <- sub(".*set","",matched$Region_name) # remove everything before "set"
    matched$SET <- sub("[A-z ]*","",matched$SET) # remove leading letters and spaces
    matched$SET <- sub("-.*","",matched$SET) # remove everything after dash
    matched$SET <- sub(".*A","",matched$SET) # remove everything before A
    
    # mean weight, length and count data for mixed sets
    mat <- morph[morph$SET %in% matched$SET,]
    
    # merge coeff data with mat
    mat <- merge(mat, coeff[coeff$choice == e,c(1,2,3,7)], 
                 by.x = "SPECIES_DESC", by.y = "Region_class")
    
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
    nasc_mix <- nasc[nasc$Region_class %in% mixed,]
    
    # add set number 
    nasc_sets <- merge(nasc_mix, matched, 
                       by=c("Region_ID", "Region_name", "Region_class", "Transect"))
    
    # check for the same length
    nrow(nasc_mix) == nrow(nasc_sets)
    
    # partition NASC
    mix_data <- merge(nasc_sets, mat[,!names(mat) %in% "Replicate"], by="SET")
    mix_data$Region_class <- mix_data$SPECIES_DESC
    mix_data$PRC_NASC <- mix_data$PRC_NASC * mix_data$R
    
    ## check - is 100% of NASC remaining?
    round(sum(nasc_mix$NASC)) == round(sum(mix_data$NASC))
    
    
    
    ##### 3) Update nasc with new NASC from mixed regions
    
    # remove mixed regions from int_regions data
    nasc_nomix <- nasc[!nasc$Region_class %in% mixed,]
    
    # add partitoned regions back into int_regions data
    nasc <- rbind(nasc_nomix, mix_data[names(nasc_nomix)])
    
  }
  
  
  
  ##########################################################################################
  ##########################################################################################
  # Check for missing lengths or weights in replicate
  
  # check for species missing in catch data
  sw <- unique(summ$SPECIES_DESC)
  srg <- unique(nasc$Region_class)
  missing <- unique(nasc$Region_class)[!(srg %in% sw)]
  
  # get missing species from summs if necessary/possible
  summ_miss <- summs[summs$SPECIES_DESC %in% missing,]
  summ <- rbind(summ, summ_miss)
  
  # check for species missing in catch data
  sfl <- unique(flength$SPECIES_DESC)
  srg <- unique(nasc$Region_class)
  missing <- unique(nasc$Region_class)[!(srg %in% sfl)]
  
  # get missing species from summs if necessary/possible
  flength_miss <- flengths[flengths$SPECIES_DESC %in% missing,]
  flength <- rbind(flength, flength_miss)
  
  
  ##########################################################################################
  ##########################################################################################
  # BIOMASS CALCULATIONS
  
  # merge coeff data with flength
  flc <- merge(flength, coeff[coeff$choice == e,c(1,2,3,7)], 
               by.x = "SPECIES_DESC",by.y = "Region_class")
  
  # Target strength by mean length (in cm)
  flc$TS <- flc$m * log10((flc$Length*flc$convert)/10) + flc$b
  
  # Backscattering cross-section weigthed by proportion
  flc$sigma_bs <-  10^(flc$TS/10) * flc$Prop
  
  # Sum Backscattering cross-section proportions
  sigma <- aggregate(sigma_bs ~ SPECIES_DESC, sum, data = flc)
  
  # merge with nasc
  mns <- merge(nasc,sigma, by.x = "Region_class", by.y = "SPECIES_DESC")
  
  # Denisty (#/nmi^2)
  mns$density <- mns$PRC_NASC/ (4*pi* mns$sigma_bs)
  
  # merge with summ
  mnsw <- merge(mns,summ[,-6], by.x = "Region_class", by.y = "SPECIES_DESC")
  
  # Biomass (kg/nmi^2)
  mnsw$biomass <- mnsw$density * (mnsw$mean.weight.kg)
  
  # summary
  summary(mnsw)
  
  # subset columns
  headers <- c("Replicate","Transect","Date_S","Time_S","Interval"
               ,"Exclude_below_line_depth_mean","Layer_depth_min","Layer_depth_max",
               "Region_ID","Region_name","Region_class","Lat_S","Lon_S",
               "PRC_NASC","density","biomass")
  bio <- mnsw[headers] 
  
  
  
  ###########################################################################################
  ###########################################################################################
  # add zeros
  
  # clean bad data
  nasc_cells <- nasc_cells[!nasc_cells$Lat_S == 999,]
  nasc_cells <- nasc_cells[!nasc_cells$Lon_S == 999,]
  
  # add zero nasc, density and biomass for each cell
  nasc_cells$PRC_NASC <- 0
  nasc_cells$density <- 0
  nasc_cells$biomass <- 0
  
  # add extra columns to bycells data
  nasc_cells$Region_name <- ""
  nasc_cells$Region_class <- ""
  nasc_cells$Region_ID <- ""
  
  # function to bind cells and df together without duplicate rows 
  # finds duplicate cell and region sv using columns in 'matching'
  matching <- c("Layer_depth_min","Layer_depth_max", "Interval", "Transect")
  duprows <- function(dfregions,dfcells){
    reg <- apply(dfregions[matching], 1, paste0, collapse = "_")
    reg <- gsub(" ", "", reg)
    cell <- apply(dfcells[matching], 1, paste0, collapse = "_")
    cell <- gsub(" ", "", cell)
    ind <- which(cell %in% reg)
    return(ind)
  }
  
  
  ###########################
  # Loop through each species
  biocell <- NULL
  bioint <- NULL
  
  for (s in unique(bio$Region_class)){
    
    # bio by species
    bio_s <- bio[bio$Region_class == s,]
    
    # biomass by regions by cells
    biocell_s <- rbind(bio_s,nasc_cells[-duprows(bio_s,nasc_cells),colnames(bio_s)])
    biocell_s$Species <- s
    biocell <- rbind(biocell, biocell_s)
    
    # biomass by regions by interval
    bioint_s <- ddply(biocell_s, .(Replicate, Transect, Interval), summarise,
                      Lon = min(Lon_S),
                      Lat = min(Lat_S),
                      Date = min(Date_S),
                      Time = min(Time_S),
                      Bottom_Depth = mean(Exclude_below_line_depth_mean),
                      Region_name = max(Region_name),
                      Region_class = max(Region_class),
                      NASC_nmi = sum(PRC_NASC),
                      Density_nmi = sum(density),
                      Biomass_kg_nmi = sum(biomass))
    bioint_s$Species <- s
    bioint <- rbind(bioint, bioint_s)
  } 
  
  
  
  ##########################################################################################
  ##########################################################################################
  # JOIN BIOMASS TABLES FROM LOOPS --- END 
  
  biocell.df <- rbind(biocell.df, biocell)
  bioint.df <- rbind(bioint.df, bioint)
}




# check map
ggplot(data = bioint.df, aes(x = Lon, y = Lat, colour = Region_class, size = NASC_nmi))+
  facet_grid(Replicate~Species) +
  geom_point()+
  scale_size_area(max_size = 10)+
  theme_bw()




###########################################################################################
###########################################################################################
# export biomass dataset

write.csv(biocell.df, file = "Other data/Biomass/BiomassByCells_AllSpecies.csv")
write.csv(bioint.df, file = "Other data/Biomass/BiomassByIntervals_AllSpecies.csv")

# save table for each species
for (k in unique(bioint.df$Species)){
  tmp <- bioint.df[bioint.df$Species == k, c("Replicate", "Transect", "Lon",  "Lat", 
                                             "Date", "Time","Bottom_Depth", "NASC_nmi", 
                                             "Density_nmi", "Biomass_kg_nmi")]
  name <- paste0(k, collapse = "")
  write.csv(tmp, file= paste0("Other data/Biomass/BiomassByIntervals_",name,".csv"))
}




##########################################################################################
##########################################################################################
# BIOMASS SUMMARY 

# average biomass (kg per nmi^2) per species 
bio_sum <- ddply(bioint.df, .(Replicate, Species), summarise, 
                 Mean.Density.nmi = mean(Density_nmi),
                 Mean.Biomass.kg.nmi = mean(Biomass_kg_nmi))


# scale up biomass to area of interest (spatial extent) 1900
bio_sum$Area.Biomass.kmt <- (bio_sum$Mean.Biomass.kg.nmi * 1900) /1000 / 1000
bio_sum

#######  export #######
write.csv(bio_sum, file = "Other data/Biomass/BiomassSummary.csv")





##########################################################################################
##########################################################################################
# BIOMASS MAPS


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
  geom_point(data = bioint.df[bioint.df$Biomass_kg_nmi == 0,], 
             aes(x = Lon, y = Lat), 
             pch = 16, size = .5, colour = "grey40") +
  geom_point(data = bioint.df[bioint.df$Biomass_kg_nmi > 0,], 
             aes(x = Lon, y = Lat, colour=Species, 
                 fill=Species, size = Biomass_kg_nmi/1000), 
             pch = 1, stroke = 1) +
  facet_wrap(~Replicate, ncol=2) +
  scale_size_area(max_size = 18, name = "Biomass (T)")+
  scale_colour_manual(values=pal, name = "") +
  scale_fill_manual(values=pal, name = "") +
  guides(colour = guide_legend(override.aes = list(size=4), ncol=1))+
  labs(x = "Longitude", y = "Latitude") +
  coord_map(xlim = c(min(bioint.df$Lon)-.1, max(bioint.df$Lon)+.1), 
            ylim = c(min(bioint.df$Lat)-.1, max(bioint.df$Lat)+.1)) +
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
comp_reps <- ggplot(data = bio_sum) + 
  geom_bar(aes(x = Species, y = Area.Biomass.kmt, fill = Species, group = Replicate),
           stat= "identity", position = "dodge", colour = "black", size = .1) +
  geom_text(aes(x=Species, y = Area.Biomass.kmt, group = Replicate, label = Replicate), 
            position = position_dodge(width=1), size = 2.5, vjust = -.3) +
  labs(x="", y=" Total Biomass (kmt)") +
  scale_fill_manual(values = pal, guide="none") +
  scale_y_continuous(breaks = c(20,40,60,80,100,120), expand =c(0,0), 
                     limit= c(0,max(bio_sum$Area.Biomass.kmt)*1.05))+
  # themes
  theme(panel.border = element_rect(fill=NA, colour="black", size = .1),
        panel.background = element_rect(fill="white",colour="white"),
        strip.background = element_blank(),
        axis.ticks.length = unit(0.1,"cm"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size=8, colour = "black"),
        axis.title = element_text(size=9, colour = "black"),
        plot.margin = unit(c(.2,.2,.2,.2), "lines")) # top, right, bottom, and left
comp_reps

pdf("Other data/Figures/Biomass.pdf", width = 5, height = 4)
comp_reps
dev.off()




