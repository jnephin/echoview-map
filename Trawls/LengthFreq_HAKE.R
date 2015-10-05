require(ggplot2)
require(grid)
require(PBSmapping)
require(plyr)

# set working directory 
setwd('..');setwd('..')

# load morpho data
morpho <- read.csv("Other data/Fishing/morpho.csv", header=T, stringsAsFactors = FALSE)
colnames(morpho)[3] <- "SET"

# load catch data for lat long
catch <- read.csv("Other data/Fishing/catch.csv", header=T, stringsAsFactors = FALSE)
colnames(catch)[4] <- "SET"

# load echoview log
log <- read.csv("Other data/Log/Cruiselog.csv", header=T, stringsAsFactors = FALSE, row.names=1)


##########################################################################################
##########################################################################################
# dataframe pre-processing

# get weight records from morpho
weights <- morpho[grep("WEIGHT",morpho$MORPHOMETRICS_ATTRIBUTE_DESC),]

# check length units
table(weights$MORPHOMETRICS_UNIT_DESC)


# get length records from morpho
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

# subset length and weight data so that it only contains log sets
morpho <- morpho[morpho$SET %in% unique(as.numeric(sdlog$SET)),]
weights <- weights[weights$SET %in% unique(as.numeric(sdlog$SET)),]



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
species_full <- unique(log$Region_class[log$Region_type == " Analysis"])
species <- gsub( "^\\s+|\\s+$", "" , species_full) # removes leading or trailing whitespace from echoview log
species <- unlist(strsplit(species, " "))
species <- unlist(strsplit(species, "-"))

# get lengths for analysis species
length <- NULL
for (i in species){
d <- morpho[grep(i, morpho$SPECIES_COMMON_NAME, ignore.case=T),]
length <- rbind(length,d)
}

# species in length table
fish <- unique(length$SPECIES_COMMON_NAME)


##############
# histograms #

for (f in fish){
  
minl <-  min(length$SPECIMEN_MORPHOMETRICS_VALUE[length$SPECIES_COMMON_NAME == f])
maxl <-  max(length$SPECIMEN_MORPHOMETRICS_VALUE[length$SPECIES_COMMON_NAME == f])
meanl <- mean(length$SPECIMEN_MORPHOMETRICS_VALUE[length$SPECIES_COMMON_NAME == f])
name <- gsub( "\\s", "_" , f)
  
histo <-  ggplot(data = length[length$SPECIES_COMMON_NAME == f,]) + 
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


##----------------------------------##
# choose length cut-off for age 1 Hake
cutoff <- 350



##########################################################################################
##########################################################################################
# MAP mean lengths

# calc mean lengths for each species and each set
mean.lengths <- ddply(morpho, .(SET, SPECIES_COMMON_NAME), summarise, 
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
fish.xy <- lengths.xy[lengths.xy$SPECIES_COMMON_NAME == f,]

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
# EXPORT mean length and weight by species


# group rockfish catch together
rock <- grep("rockfish|perch", catch$SPECIES_COMMON_NAME, ignore.case=T)
r_catch <- catch[rock,c("SET", "SPECIES_COMMON_NAME", "CATCH_WEIGHT")]
wr_catch <- catch[-rock,c("SET", "SPECIES_COMMON_NAME", "CATCH_WEIGHT")]
rs_catch <- ddply(r_catch, .(SET), transform, CATCH_WEIGHT = sum(CATCH_WEIGHT))
grp_catch <- rbind(wr_catch,rs_catch)

# prep weight data for merge with length data
weights <- weights[c("SET", "SPECIES_COMMON_NAME", "CATCH_ID", "SAMPLE_ID", 
                     "SPECIMEN_ID", "SPECIMEN_MORPHOMETRICS_VALUE", "MORPHOMETRICS_UNIT_DESC")]
colnames(weights)[6:7] <- c("WEIGHT", "WEIGHT_UNITS")

# merge weight and length data
morphs <- merge(morpho, weights, 
                by = c("SET", "SPECIES_COMMON_NAME", "CATCH_ID", "SAMPLE_ID", "SPECIMEN_ID"), 
                all.x=T)

# compare number of length samples to weight samples
sam <- morphs[!is.na(morphs$WEIGHT),]
table(morpho$SPECIES_COMMON_NAME)
table(sam$SPECIES_COMMON_NAME)


# merge catch and morphs
comb <- merge(morphs, grp_catch[c("SET", "SPECIES_COMMON_NAME", "CATCH_WEIGHT")], 
              by = c("SET", "SPECIES_COMMON_NAME"))


# compare morpho species to analysis species
species_full
unique(comb$SPECIES_COMMON_NAME)


# reclassify morpho species to fit analysis region species
comb$SPECIES_COMMON_NAME[comb$SPECIES_COMMON_NAME == "PACIFIC HAKE" & 
                    comb$SPECIMEN_MORPHOMETRICS_VALUE > cutoff  ] <- "Hake"
comb$SPECIES_COMMON_NAME[comb$SPECIES_COMMON_NAME == "PACIFIC HAKE" & 
                    comb$SPECIMEN_MORPHOMETRICS_VALUE < cutoff  ] <- "Age-1 Hake"
comb$SPECIES_COMMON_NAME[grep("herring", comb$SPECIES_COMMON_NAME, ignore.case=T)] <- "Herring"
comb$SPECIES_COMMON_NAME[grep("rockfish|perch", comb$SPECIES_COMMON_NAME, ignore.case=T)] <- "Rockfish"
comb$SPECIES_COMMON_NAME[grep("sardine", comb$SPECIES_COMMON_NAME, ignore.case=T)] <- "Sardine"
comb$SPECIES_COMMON_NAME[grep("myctophid", comb$SPECIES_COMMON_NAME, ignore.case=T)] <- "Myctophids"
comb$SPECIES_COMMON_NAME[grep("cps|sardine", comb$SPECIES_COMMON_NAME, ignore.case=T)] <- "CPS"
comb$SPECIES_COMMON_NAME[grep("mackerel", comb$SPECIES_COMMON_NAME, ignore.case=T)] <- "Mackerel"
comb$SPECIES_COMMON_NAME[grep("pollock", comb$SPECIES_COMMON_NAME, ignore.case=T)] <- "Pollock"
comb$SPECIES_COMMON_NAME[grep("eulachon", comb$SPECIES_COMMON_NAME, ignore.case=T)] <- "Eulachon"

# check species
table(comb$SPECIES_COMMON_NAME)

# check weight units - divide by 1000 in morph_sum if grams
table(comb$WEIGHT_UNITS)

# calc mean length and weight for each species
morph_sum <- ddply(comb, .(SPECIES_COMMON_NAME), summarise, 
                      mean.length.mm = mean(SPECIMEN_MORPHOMETRICS_VALUE,na.rm = T),
                      weigted.mean.length.mm = sum(SPECIMEN_MORPHOMETRICS_VALUE * WEIGHT, na.rm = T)/
                     sum(WEIGHT,na.rm = T),
                      mean.weight.kg = mean(WEIGHT, na.rm = T)/1000,
                      n = length(WEIGHT))
morph_sum

# export
write.csv(morph_sum, file = "Other data/Fishing/morpho_summary.csv")



##########################################################################################
##########################################################################################
# EXPORT mean length, weight and counts by species by set


# partition hake and age1 hake CATCH_WEIGHT based on individual weights
comb$phake <- 1
for (h in unique(comb$SET)){
  a1 <- comb$WEIGHT[comb$SPECIES_COMMON_NAME == "Age-1 Hake" & comb$SET == h]
  ad <- comb$WEIGHT[comb$SPECIES_COMMON_NAME == "Hake" & comb$SET == h]
  ma1 <- mean(a1) * length(a1)
  ma1 <- ifelse(is.na(ma1),0,ma1)
  mad <- mean(ad) * length(ad)
  mad <- ifelse(is.na(mad),0,mad)
  pa1 <-  ma1/(ma1+mad)
  pad <- 1 - pa1
  comb$phake[comb$SPECIES_COMMON_NAME == "Age-1 Hake" & comb$SET == h] <- pa1
  comb$phake[comb$SPECIES_COMMON_NAME == "Hake" & comb$SET == h] <- pad
}


# calculate new total catch weights for hake and age1 hake in the same tows
comb$CATCH_WEIGHT <- comb$CATCH_WEIGHT * comb$phake

# calc mean weights (in kg) for each species and each set
morph_count <- ddply(comb, .(SET, SPECIES_COMMON_NAME), summarise,
                     mean.length.mm = mean(SPECIMEN_MORPHOMETRICS_VALUE),
                     mean.weight.kg = mean(WEIGHT, na.rm = T)/1000,
                     total.weight.kg = min(CATCH_WEIGHT))

#merge catch and mean length data
morph_count$Estimated_N <- round(morph_count$total.weight.kg/morph_count$mean.weight.kg)
morph_count

# export summary
write.csv(morph_count, file = "Other data/Fishing/morpho_counts.csv")














