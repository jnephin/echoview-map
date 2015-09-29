require(ggplot2)
require(grid)
require(PBSmapping)
require(plyr)

# set working directory 
setwd('..');setwd('..')

# load  morpho data
morpho <- read.csv("Other data/Fishing/morpho.csv", header=T, stringsAsFactors = FALSE)

# load set lat long data
sets <- read.csv("Other data/Fishing/sets.csv", header=T, stringsAsFactors = FALSE)

# load echoview log
log <- read.csv("Other data/Log/Cruiselog.csv", header=T, stringsAsFactors = FALSE, row.names=1)



##########################################################################################
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

# convert to decimal degree
sets$Lat <- sets$FE_START_LATTITUDE_DEGREE + (sets$FE_START_LATTITUDE_MINUTE/60)
sets$Long <- (sets$FE_START_LONGITUDE_DEGREE + (sets$FE_START_LONGITUDE_MINUTE/60))*-1


# change FE_ID to SET
colnames(morpho)[3] <- "SET"
colnames(sets)[4] <- "SET"

#check --are all the sets present in the echoview log?
unique(sets$SET)
unique(morpho$SET)




##########################################################################################
##########################################################################################
# Length by species by set


# check mean, sd, and range of lengths within a species
check.lengths <- ddply(morpho, .(SPECIES_COMMON_NAME), summarise, 
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
species <- unique(log$Region_class[log$Region_type == " Analysis"])
species <- gsub( "^\\s+|\\s+$", "" , species) # removes leading or trailing whitespace from echoview log
species <- unlist(strsplit(species, " "))

# get lengths for analysis species
length <- NULL
for (i in species){
d <- morpho[grep(i, morpho$SPECIES_COMMON_NAME, ignore.case=T),]
length <- rbind(length,d)
}

#species in length table
fish <- unique(length$SPECIES_COMMON_NAME)


##############
# histograms #

for (f in fish){
  
minl <-  min(length$SPECIMEN_MORPHOMETRICS_VALUE[length$SPECIES_COMMON_NAME == f])
maxl <-  max(length$SPECIMEN_MORPHOMETRICS_VALUE[length$SPECIES_COMMON_NAME == f])
meanl <- mean(length$SPECIMEN_MORPHOMETRICS_VALUE[length$SPECIES_COMMON_NAME == f])
name <- gsub( "\\s", "_" , f)
  
histo <-  ggplot(data = length[length$SPECIES_COMMON_NAME == f,]) + 
  geom_histogram(aes(x = SPECIMEN_MORPHOMETRICS_VALUE), binwidth = (maxl-minl)/20)+
  geom_vline(xintercept = meanl, colour = "red")+
  labs(x="Length (mm)", y="Frequency")+
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
pdf(paste("Other data/Figures/","Lengths_Histogram_",name,".pdf", sep=""), width = 4, height = 3.5)
print(histo)
dev.off()
}










##########################################################################################
##########################################################################################
# MAP mean lengths

# calc mean for each species and each set
mean.lengths <- ddply(morpho, .(SET, SPECIES_COMMON_NAME), summarise, 
                      mean = mean(SPECIMEN_MORPHOMETRICS_VALUE),
                      median = median(SPECIMEN_MORPHOMETRICS_VALUE),
                      sd = sd(SPECIMEN_MORPHOMETRICS_VALUE))

# merge lat long with mean lengths
lengths.xy <- merge(mean.lengths, sets, by = "SET")

# Regional Polygon 
data(nepacLLhigh)
landT<- thinPolys(nepacLLhigh, tol = 0, filter = 15)
landC <- clipPolys(landT, xlim = c(min(sets$Long)-2, max(sets$Long)+2), 
                   ylim = c(min(sets$Lat)-2, max(sets$Lat)+2))
land <- data.frame(landC)


#####################
# Map mean lengths #

for (f in fish){

name <- gsub( "\\s", "_" , f)
fish.xy <- lengths.xy[lengths.xy$SPECIES_COMMON_NAME == f,]

mapmean <-  ggplot(data = NULL) + 
  # land polygon  
  geom_polygon(data = land, aes_string(x = "X", y = "Y", group="PID"), 
               fill = "gray80", colour = "gray60", size = .1) +
  # trawl points
  geom_point(data = fish.xy, aes(x = Long, y = Lat, size = mean), 
             colour = "#e41a1c" ,pch=21)+
  geom_point(data = fish.xy, aes(x = Long, y = Lat, size = mean), 
             colour = "#e41a1c" ,pch=21, fill = "#e41a1c" , alpha =.3)+
  scale_size_area(max_size = 8, name = " Mean\n Length (mm)") +
  # spatial extent
  coord_map(xlim = c(min(sets$Long)-2, max(sets$Long)+2), 
            ylim = c(min(sets$Lat)-1, max(sets$Lat)+1)) +
  #labs
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
        plot.margin = unit(c(.5,.5,.5,.5), "lines")) # top, right, bottom, and

pdf(paste("Other data/Figures/","Lengths_Map_",name,".pdf", sep=""), width = 4.5, height = 3.5)
print(mapmean)
dev.off()
}
