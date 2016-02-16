require(ggplot2)
require(grid)
require(PBSmapping)
require(plyr)
require(reshape2)


# set working directory 
setwd('..');setwd('..')



#########################################################################################
# load  morpho data
morpho <- read.csv("Other data/Catch data/morpho.csv", header=T, stringsAsFactors = FALSE)

# load catch data for lat long
catch <- read.csv("Other data/Catch data/catch.csv", header=T, stringsAsFactors = FALSE)
catch <- catch[!is.na(catch$CATCH_WEIGHT_kg),]
colnames(catch)[8] <- "SPECIES_DESC"

# load echoview log
log <- read.csv("Acoustics/Echoview/Exports/Log/Cruiselog.csv", header=T, stringsAsFactors = FALSE, row.names=1)

# load target strength coefficients
coeff_TS <- read.csv("Rscripts/Trawls/TS_coefficients.csv", header=T, stringsAsFactors = FALSE)


##########################################################################################
##########################################################################################
# dataframe pre-processing


## GET SETS PRESENT IN ECHOVIEW LOG

# get xy data from log
sdlog <- log[grep("SD", log$Region_name, ignore.case=TRUE),] #find SD
sdlog$SET <-  as.numeric(sub(".*SD", "", sdlog$Region_name))
sdlog$Replicate <- gsub("[A-Za-z ]", "", sdlog$File)
sdlog$Replicate <- as.numeric(sub("[.].*", "", sdlog$Replicate))
set.xy <- sdlog[c("SET","Lat_s","Lon_s", "Replicate")]

# check --are all the sets present in the echoview log?
sort(unique(as.numeric(sdlog$SET)))
sort(unique(morpho$SET))

# subset morpho data so that it only contains log sets
morpho <- merge(morpho, set.xy, by = "SET")



## UPDATE UNITS

# check length units
table(morpho$LENGTH_UNITS)

# convert all lengths to mm
morpho$LENGTH <- ifelse(morpho$LENGTH_UNITS == "CENTIMETRE", 
                              morpho$LENGTH*10,
                              morpho$LENGTH)

## UPDATE SPECIES DESC
morpho$SPECIES_DESC <- sub(".*- ", "", morpho$SPECIES_CODE)

## CHECK

# do species lengths in morpho match species length in coeff for regressions?
coeff_TS[!duplicated(coeff_TS$Region_class),c("Region_class", "MORPHOMETRICS_ATTRIBUTE_DESC")]
ddply(morpho, .(SPECIES_DESC), summarise, LENGTH_TYPE = unique(LENGTH_TYPE))



##########################################################################################
##########################################################################################
# Length by species by set


# check mean, sd, and range of lengths within a species
check.lengths <- ddply(morpho, .(SPECIES_DESC), summarise, 
                          mean = mean(LENGTH),
                          median = median(LENGTH),
                          n = length(LENGTH),
                          sd = sd(LENGTH),
                          min = min(LENGTH), 
                          max = max(LENGTH))
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
d$SPECIES_DESC[grep("rockfish|perch", d$SPECIES_DESC, ignore.case=T)] <- "Rockfish"
length <- rbind(length,d)
}

# species in length table
fish <- unique(length$SPECIES_DESC)


##############
# histograms #

for (f in fish){
  
minl <-  min(length$LENGTH[length$SPECIES_DESC == f])
maxl <-  max(length$LENGTH[length$SPECIES_DESC == f])
meanl <- mean(length$LENGTH[length$SPECIES_DESC == f])
name <- gsub( "\\s", "_" , f)
  
histo <-  ggplot(data = length[length$SPECIES_DESC == f,]) + 
  geom_histogram(aes(x = LENGTH), binwidth = (maxl-minl)/20) +
  geom_vline(xintercept = meanl, colour = "red") +
  labs(x="Length (mm)", y="Frequency") +
  scale_y_continuous(expand = c(0.01, 0)) +
  facet_wrap(~Replicate, ncol=2) +
  # themes
  theme(panel.border = element_rect(fill=NA, colour="black", size = .1),
        panel.background = element_rect(fill="white",colour="white"),
        strip.background = element_blank(),
        axis.ticks = element_line(colour="black"),
        axis.ticks.length = unit(0.1,"cm"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size=7, colour = "black"),
        strip.text = element_text(size=10, colour = "black"),
        axis.title = element_text(size=10, colour = "black"),
        plot.margin = unit(c(.2,.2,.2,.2), "lines")) + # top, right, bottom, and left
  ggtitle(f)
print(histo)
}


##----------------------------------##
# choose length cut-off for age 1 Hake
cutoff <- 350



##########################################################################################
##########################################################################################
# MAP mean lengths

# calc mean lengths for each species and each set
mean.lengths <- ddply(morpho, .(SET, SPECIES_DESC, Lat_s, Lon_s), summarise, 
                      mean = round(mean(LENGTH)),
                      median = median(LENGTH),
                      sd = sd(LENGTH))
mean.lengths$SPECIES_DESC[grep("rockfish|perch", mean.lengths$SPECIES_DESC, ignore.case=T)] <- "Rockfish"


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
fish.xy <- mean.lengths[mean.lengths$SPECIES_DESC == f,]

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
  coord_map(xlim = c(min(set.xy$Lon_s)-1, max(set.xy$Lon_s)+1), 
            ylim = c(min(set.xy$Lat_s)-.5, max(set.xy$Lat_s)+.5)) +
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
  ggtitle(name)
print(mapmean)
}




##########################################################################################
##########################################################################################
# EXPORT mean length and weight by species


# group rockfish catch together
rock <- grep("rockfish|perch", catch$SPECIES_DESC, ignore.case=T)
r_catch <- catch[rock,c("SET", "SPECIES_DESC", "CATCH_WEIGHT_kg")]
rs_catch <- ddply(r_catch, .(SET), transform, CATCH_WEIGHT_kg = sum(CATCH_WEIGHT_kg))

# group salmon catch together
salmon <- grep("COHO SALMON|CHUM SALMON|CHINOOK SALMON|PINK SALMON|SOCKEYE SALMON", catch$SPECIES_DESC, ignore.case=T)
s_catch <- catch[salmon,c("SET", "SPECIES_DESC", "CATCH_WEIGHT_kg")]
ss_catch <- ddply(s_catch, .(SET), transform, CATCH_WEIGHT_kg = sum(CATCH_WEIGHT_kg))


# group myctophid catch together
myctophid <- grep("myctophid|lampfish|headlight", catch$SPECIES_DESC, ignore.case=T)
m_catch <- catch[myctophid,c("SET", "SPECIES_DESC", "CATCH_WEIGHT_kg")]
sm_catch <- ddply(m_catch, .(SET), transform, CATCH_WEIGHT_kg = sum(CATCH_WEIGHT_kg))


w_catch <- catch[-c(rock,salmon,myctophid),c("SET", "SPECIES_DESC", "CATCH_WEIGHT_kg")]
grp_catch <- rbind(w_catch,rs_catch, ss_catch, sm_catch)


# merge catch and morpho
comb <- merge(morpho, grp_catch[c("SET", "SPECIES_DESC", "CATCH_WEIGHT_kg")], 
              by = c("SET", "SPECIES_DESC"))


# compare morpho species to analysis species
species_full
unique(comb$SPECIES_DESC)


# reclassify morpho species to fit analysis region species
comb$SPECIES_DESC[comb$SPECIES_DESC == "PACIFIC HAKE" & 
                    comb$LENGTH > cutoff  ] <- "Hake"
comb$SPECIES_DESC[comb$SPECIES_DESC == "PACIFIC HAKE" & 
                    comb$LENGTH <= cutoff  ] <- "Age-1 Hake"
comb$SPECIES_DESC[grep("herring", comb$SPECIES_DESC, ignore.case=T)] <- "Herring"
comb$SPECIES_DESC[grep("rockfish|perch", comb$SPECIES_DESC, ignore.case=T)] <- "Rockfish"
comb$SPECIES_DESC[grep("sardine", comb$SPECIES_DESC, ignore.case=T)] <- "Sardine"
comb$SPECIES_DESC[grep("myctophid|lampfish|headlight", comb$SPECIES_DESC, ignore.case=T)] <- "Myctophids"
comb$SPECIES_DESC[grep("cps", comb$SPECIES_DESC, ignore.case=T)] <- "CPS"
comb$SPECIES_DESC[grep("mackerel", comb$SPECIES_DESC, ignore.case=T)] <- "Mackerel"
comb$SPECIES_DESC[grep("pollock", comb$SPECIES_DESC, ignore.case=T)] <- "Pollock"
comb$SPECIES_DESC[grep("eulachon", comb$SPECIES_DESC, ignore.case=T)] <- "Eulachon"
comb$SPECIES_DESC[grep("salmon", comb$SPECIES_DESC, ignore.case=T)] <- "Salmon"

# check species
table(comb$SPECIES_DESC)

# check weight units - divide by 1000 in morph_sum if grams
table(comb$WEIGHT_UNITS)

# calc mean length and weight for each species
morph_sum <- NULL
for (r in unique(comb$Replicate)){
df <- ddply(comb[comb$Replicate == r,], .(SPECIES_DESC), summarise, 
                      mean.length.mm = mean(LENGTH),
                      weigted.mean.length.mm = sum(LENGTH * CATCH_WEIGHT_kg)/
                                                    sum(CATCH_WEIGHT_kg),
                      mean.weight.kg = mean(WEIGHT, na.rm = T)/1000,
                      n = length(WEIGHT))
df$Replicate <- r
morph_sum <- rbind(morph_sum, df)
}

# export
write.csv(morph_sum, file = "Other data/Catch data/morpho_summary.csv")



##########################################################################################
##########################################################################################
# EXPORT mean length, weight and counts by species by set


# partition hake and age1 hake CATCH_WEIGHT based on individual weights
comb$phake <- 1
for (h in unique(comb$SET)){
  a1 <- comb$WEIGHT[comb$SPECIES_DESC == "Age-1 Hake" & comb$SET == h]
  ad <- comb$WEIGHT[comb$SPECIES_DESC == "Hake" & comb$SET == h]
  ma1 <- mean(a1) * length(a1)
  ma1 <- ifelse(is.na(ma1),0,ma1)
  mad <- mean(ad) * length(ad)
  mad <- ifelse(is.na(mad),0,mad)
  pa1 <-  ma1/(ma1+mad)
  pad <- 1 - pa1
  comb$phake[comb$SPECIES_DESC == "Age-1 Hake" & comb$SET == h] <- pa1
  comb$phake[comb$SPECIES_DESC == "Hake" & comb$SET == h] <- pad
}


# calculate new total catch weights for hake and age1 hake in the same tows
comb$CATCH_WEIGHT_kg <- comb$CATCH_WEIGHT_kg * comb$phake

# calc mean weights (in kg) for each species and each set
morph_count <- NULL
for (r in unique(comb$Replicate)){
df <- ddply(comb[comb$Replicate == r,], .(SET, SPECIES_DESC), summarise,
                mean.length.mm = mean(LENGTH),
                mean.weight.kg = mean(WEIGHT, na.rm = T)/1000,
                total.weight.kg = min(CATCH_WEIGHT_kg))
df$Replicate <- r
morph_count <- rbind(morph_count, df)
}

#merge catch and mean length data
morph_count$Estimated_N <- round(morph_count$total.weight.kg/morph_count$mean.weight.kg)
morph_count

# export summary
write.csv(morph_count, file = "Other data/Catch data/morpho_counts.csv")



##########################################################################################
##########################################################################################
# EXPORT length frequencies by species

# round lengths to 1 cm resolution
comb$Lenth <- round_any(comb$LENGTH, 10)

# calc length freq 
freq_sum <- NULL
for (r in unique(comb$Replicate)){
  tmp <- comb[comb$Replicate == r,]
  freqtable <- table(tmp$Lenth, tmp$SPECIES_DESC)
  proptable <- prop.table(freqtable, 2)
  propmelt <- melt(proptable)
  propmelt$Replicate <- r
  colnames(propmelt) <- c("Length","SPECIES_DESC","Prop", "Replicate")
  freq_sum <- rbind(freq_sum, propmelt)
}

# remove rows with zeros
freq_sum <- freq_sum[!freq_sum$Prop == 0, ]

# export summary
write.csv(freq_sum, file = "Other data/Catch data/Length_freq.csv")









