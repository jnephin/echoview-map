require(ggplot2)
require(grid)
require(PBSmapping)
require(plyr)

# set working directory 
setwd('..');setwd('..')

# load  morpho data
morpho <- read.csv("Other data/Fishing/morpho.csv", header=T, stringsAsFactors = FALSE)

# load catch data for lat long
catch <- read.csv("Other data/Fishing/catch.csv", header=T, stringsAsFactors = FALSE)

# load echoview log
log <- read.csv("Other data/Log/Cruiselog.csv", header=T, stringsAsFactors = FALSE, row.names=1)



#########################################################################################
##########################################################################################
# dataframe pre-processing


# remove weiht records - only interested in lengths
morpho <- morpho[grep("LENGTH",morpho$MORPHOMETRICS_ATTRIBUTE_DESC),]

# check length units
table(morpho$MORPHOMETRICS_UNIT_DESC)

# convert all lengths to mm
morpho$SPECIMEN_MORPHOMETRICS_VALUE <- ifelse(morpho$MORPHOMETRICS_UNIT_DESC == "CENTIMETRE", 
                                              morpho$SPECIMEN_MORPHOMETRICS_VALUE*10,
                                              morpho$SPECIMEN_MORPHOMETRICS_VALUE)



# get xy data from log
sdlog <- log[grep("SD", log$Region_name, ignore.case=TRUE),] #find SD
sets <- unlist(strsplit(sdlog$Region_name, split = 'SD')) #clean SD
sdlog$SET <- sub("^[0]+", "", sets[seq(2, length(sets), 2)]) #remove leading zeros
set.xy <- sdlog[c("SET","Lat_s","Lon_s")]

# check --are all the sets present in the echoview log?
sort(unique(as.numeric(sdlog$SET)))
unique(morpho$SET)

# subset morpho data so that it only contains log sets
morpho <- morpho[morpho$SET %in% unique(as.numeric(sdlog$SET)),]


##########################################################################################
##########################################################################################
# Length by species by set


# check mean, sd, and range of lengths within a species
check.lengths <- ddply(morpho, .(SPECIES_DESC), summarise, 
                          mean = mean(SPECIMEN_MORPHOMETRICS_VALUE),
                          median = median(SPECIMEN_MORPHOMETRICS_VALUE),
                          n = length(SPECIMEN_MORPHOMETRICS_VALUE),
                          sd = sd(SPECIMEN_MORPHOMETRICS_VALUE),
                          min = min(SPECIMEN_MORPHOMETRICS_VALUE), 
                          max = max(SPECIMEN_MORPHOMETRICS_VALUE))
check.lengths




##########################################################################################
##########################################################################################
# Length Frequency Distribution


# species from echoview analysis regions only
species_full <- unique(log$Region_class[log$Region_type == " Analysis"])
species <- gsub( "^\\s+|\\s+$", "" , species_full) # removes leading or trailing whitespace from echoview log
species <- unlist(strsplit(species, " "))
species <- unlist(strsplit(species, "-"))

# get lengths for analysis species
length <- NULL
for (i in species){
d <- morpho[grep(i, morpho$SPECIES_DESC, ignore.case=T),]
length <- rbind(length,d)
}

# species in length table
fish <- unique(length$SPECIES_DESC)


##############
# histograms #

for (f in fish){
  
minl <-  min(length$SPECIMEN_MORPHOMETRICS_VALUE[length$SPECIES_DESC == f])
maxl <-  max(length$SPECIMEN_MORPHOMETRICS_VALUE[length$SPECIES_DESC == f])
meanl <- mean(length$SPECIMEN_MORPHOMETRICS_VALUE[length$SPECIES_DESC == f])
name <- gsub( "\\s", "_" , f)
  
histo <-  ggplot(data = length[length$SPECIES_DESC == f,]) + 
  geom_histogram(aes(x = SPECIMEN_MORPHOMETRICS_VALUE), binwidth = (maxl-minl)/20) +
  geom_vline(xintercept = meanl, colour = "red") +
  labs(x="Length (mm)", y="Frequency") +
  scale_y_continuous(expand = c(0.01, 0)) +
  # themes
  theme(panel.border = element_rect(fill=NA, colour="black", size = .1),
        panel.background = element_rect(fill="white",colour="white"),
        axis.ticks = element_line(colour="black"),
        axis.ticks.length = unit(0.1,"cm"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size=7, colour = "black"),
        axis.title = element_text(size=10, colour = "black"),
        plot.margin = unit(c(.2,.2,.2,.2), "lines")) # top, right, bottom, and left
pdf(paste("Other data/Figures/","Lengths_Histogram",name,".pdf", sep=""), width = 4, height = 3.5)
print(histo)
dev.off()
}






##########################################################################################
##########################################################################################
# MAP mean lengths

# calc mean lengths for each species and each set
mean.lengths <- ddply(morpho, .(SET, SPECIES_DESC), summarise, 
                      mean = round(mean(SPECIMEN_MORPHOMETRICS_VALUE)),
                      median = median(SPECIMEN_MORPHOMETRICS_VALUE),
                      sd = sd(SPECIMEN_MORPHOMETRICS_VALUE))
 
# merge lat long with mean lengths
lengths.xy <- merge(mean.lengths, set.xy, by = "SET")


# Regional Polygon 
data(nepacLLhigh)
landT<- thinPolys(nepacLLhigh, tol = 0, filter = 15)
landC <- clipPolys(landT, xlim = c(min(set.xy$Lon_s)-2, max(set.xy$Lon_s)+2), 
                   ylim = c(min(set.xy$Lat_s)-2, max(set.xy$Lat_s)+2))
land <- data.frame(landC)


#####################
# Map mean lengths #

for (f in fish){

name <- gsub( "\\s", "_" , f)
fish.xy <- lengths.xy[lengths.xy$SPECIES_DESC == f,]

mapmean <-  ggplot(data = NULL) + 
  # land polygon  
  geom_polygon(data = land, aes_string(x = "X", y = "Y", group="PID"), 
               fill = "gray80", colour = "gray60", size = .1) +
  # trawl points
  geom_point(data = fish.xy, aes(x = Lon_s, y = Lat_s, size = mean), 
             colour = "#e41a1c" ,pch=21)+
  geom_point(data = fish.xy, aes(x = Lon_s, y = Lat_s, size = mean), 
             colour = "#e41a1c" ,pch=21, fill = "#e41a1c" , alpha =.3)+
  scale_size_area(max_size = 8, name = " Mean\n Length (mm)") +
  # spatial extent
  coord_map(xlim = c(min(set.xy$Lon_s)-2, max(set.xy$Lon_s)+2), 
            ylim = c(min(set.xy$Lat_s)-1, max(set.xy$Lat_s)+1)) +
  # labs
  labs(x="Longitude", y="Latitude") +
  # themes
  theme(panel.border = element_rect(fill=NA, colour="black", size = .1),
        panel.background = element_rect(fill="white",colour="white"),
        axis.ticks = element_line(colour="black"),
        axis.ticks.length = unit(0.1,"cm"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size=7, colour = "black"),
        axis.title = element_text(size=9, colour = "black"),
        legend.text = element_text(size=8),
        legend.title = element_text(size=9, face="plain"),
        legend.key = element_blank(),
        legend.position = "right",
        plot.margin = unit(c(.5,.5,.5,.5), "lines")) +
  ggtitle(lab = name)

#pdf(paste("Other data/Figures/","Lengths_Map",name,".pdf", sep=""), width = 4.5, height = 3.5)
print(mapmean)
#dev.off()
}






##########################################################################################
##########################################################################################
# EXPORT mean length of analysis region species


# compare morpho species to analysis species
species_full
unique(morpho$SPECIES_DESC)

# reclassify morpho species to fit analysis region species
morpho$SPECIES_DESC[morpho$SPECIES_DESC == " PACIFIC HAKE" & 
         morpho$SPECIMEN_MORPHOMETRICS_VALUE > 350  ] <- " Hake"
morpho$SPECIES_DESC[morpho$SPECIES_DESC == " PACIFIC HAKE" & 
         morpho$SPECIMEN_MORPHOMETRICS_VALUE < 350  ] <- " Age-1 Hake"
morpho$SPECIES_DESC[morpho$SPECIES_DESC == " PACIFIC HERRING"] <- " Herring"
morpho$SPECIES_DESC[morpho$SPECIES_DESC == " YELLOWTAIL ROCKFISH"] <- " Rockfish"
morpho$SPECIES_DESC[morpho$SPECIES_DESC == " WIDOW ROCKFISH"] <- " Rockfish"
morpho$SPECIES_DESC[morpho$SPECIES_DESC == " REDSTRIPE ROCKFISH"] <- " Rockfish"

# check species
table(morpho$SPECIES_DESC)

# check weight units
table(morpho$WEIGHT_UNITS)

# calc mean length and weight for each species
data <- ddply(morpho, .(SPECIES_DESC), summarise, 
                      mean.length = mean(SPECIMEN_MORPHOMETRICS_VALUE),
                      weigted.mean.length = sum(SPECIMEN_MORPHOMETRICS_VALUE * WEIGHT)/sum(WEIGHT),
                      mean.weight = mean(WEIGHT),
                      n = length(WEIGHT))


# export data
write.csv(data, file = "Other data/Fishing/morpho_summary.csv")
