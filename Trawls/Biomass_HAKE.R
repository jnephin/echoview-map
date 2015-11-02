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
summ <- read.csv("Other data/Catch data/morpho_summary.csv", header=T, stringsAsFactors = FALSE, 
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

# load survey data
trans <- read.csv("Other data/Catch data/transects.csv", header=T, stringsAsFactors = FALSE)




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
## MERGE SURVEY INFO

## update trans Transects
trans$Transect <- sub(".csv", "", trans$File)

## merge nasc and log with replicate info
test <- nrow(int_region)
logs <- merge(logs, trans, by = "File")
int_region <- merge(int_region, trans, by = "Transect")
int_cell <- merge(int_cell, trans, by = "Transect")


# check for changes in int_regions (TRUE = no change)
test == nrow(int_region)




############################################################################
############################################################################
## LOOP THROUGH SURVEYS, AREAS OR REPLICATES and ESIMTATES

biomass.df <- NULL
distance.df <- NULL
options <- c("Survey","Area1","Area2","Replicate1","Replicate2")
con <- c("a", "b") # target strength - length regression constants

for (i in options){

  for (e in con){
     
# 1) for survey
  if(i == "Survey")  {
log <- logs[logs$Survey == 1,]
int_regions <- int_region[int_region$Survey == 1,]
int_cells <- int_cell[int_cell$Survey == 1 ,] 
  }
  
# 2) for area 1
  if(i == "Area1")  {
log <- logs[logs$Survey == 1 & logs$Area == 1,]
int_regions <- int_region[int_region$Survey == 1 &  int_region$Area == 1,]
int_cells <- int_cell[int_cell$Survey == 1 &  int_cell$Area == 1, ] 
  }
  
# 3) for area 2
  if(i == "Area2")  {
log <- logs[logs$Survey == 1 & logs$Area == 2,]
int_regions <- int_region[int_region$Survey == 1 &  int_region$Area == 2,]
int_cells <- int_cell[int_cell$Survey == 1 &  int_cell$Area == 2, ] 
  }
  
# 4) for replicate 1
  if(i == "Replicate1")  {
log <- logs[logs$Survey == 1 & logs$Replicate == "y",]
int_regions <- int_region[int_region$Survey == 1 &   int_region$Replicate == "y",]
int_cells <- int_cell[int_cell$Survey == 1 &   int_cell$Replicate == "y", ] 
  }
  
# 5) for replicate 2
  if(i == "Replicate2")  {
log <- logs[logs$Survey == 2 & logs$Replicate == "y",]
int_regions <- int_region[int_region$Survey == 2 &   int_region$Replicate == "y",]
int_cells <- int_cell[int_cell$Survey == 2 &   int_cell$Replicate == "y", ] 
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
mixed <- c("Hake mix")

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
mat <- merge(mat, coeff[coeff$choice == e,c(1,2,3,7)], by.x = "SPECIES_COMMON_NAME", by.y = "Region_class")

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
mix_data <- merge(int_sets, mat, by="SET")
mix_data$Region_class <- mix_data$SPECIES_COMMON_NAME
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
mix_reg <- merge(man, mat, by="SET")
mix_reg$Region_class <- mix_reg$SPECIES_COMMON_NAME

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
# HAKE BIOMASS CALCULATIONS


# get int data for hake region classes only
int_hake <- int_regions[grep("hake",int_regions$Region_class, ignore.case=T),]

# add sets
for_sets <- int_hake[!duplicated(int_hake[c("Region_ID", "Region_name", "Region_class", "Transect")]),] 
for_sets$SET <- sub(".*set","",for_sets$Region_name) # remove everything before "set"
for_sets$SET <- sub("[A-z ]*","",for_sets$SET) # remove leading letters
for_sets$SET <- sub("-.*","",for_sets$SET) # remove everything after dash
for_sets$SET <- sub(".*A","",for_sets$SET) # remove everything before A
for_sets

# merge sets and int hake data
int_hake <- merge(int_hake, 
                  for_sets[c("Region_ID", "Region_name", "Region_class", "Transect", "SET")], 
                  by=c("Region_ID", "Region_name", "Region_class", "Transect"))

# only hake morpho data
morph_hake <- morph[grep("hake",morph$SPECIES_COMMON_NAME, ignore.case=T),]

# merge hake nasc and morpho data
him <- merge(int_hake, morph_hake, by.x = c("Region_class","SET"), by.y = c("SPECIES_COMMON_NAME", "SET"))

## check - should be same length
dim(int_hake)
dim(him)


# merge coeff data with him
himc <- merge(him, coeff[coeff$choice == e,c(1,2,3,7)], by = "Region_class")

# Target strength by mean length (in cm)
himc$TS <- himc$m * log10((himc$mean.length.mm*himc$convert)/10) + himc$b
  
# Backscattering cross-section 
himc$sigma_bs <- 10 ^(himc$TS/10)
  
# Denisty (#/nmi^2)
himc$density <- himc$NASC/ (4*pi*himc$sigma_bs)
  
 # Biomass (kg/nmi^2)
himc$biomass <- himc$density * (himc$mean.weight.kg)

# summary
summary(himc)



##########################################################################################
##########################################################################################
# OTHER BIOMASS CALCULATIONS

# get int data for all other region classes 
int_other <- int_regions[-(grep("hake",int_regions$Region_class, ignore.case=T)),]

# non-hake morpho summary data
summ_other <- summ[-(grep("hake",summ$SPECIES_COMMON_NAME, ignore.case=T)),]

# merge non-hake nasc and morpho data
oim <- merge(int_other, summ_other, by.x = c("Region_class"), by.y = c("SPECIES_COMMON_NAME"))

## check - the same length unless region aren't in fishing data -> CPS, myctophids, etc.
dim(int_other)
dim(oim)

# merge coeff data with him
oimc <- merge(oim, coeff[coeff$choice == e,c(1,2,3,7)], by = "Region_class")

# Target strength by mean length (in cm)
oimc$TS <- oimc$m * log10((oimc$weigted.mean.length.mm*oimc$convert)/10) + oimc$b

# Backscattering cross-section 
oimc$sigma_bs <- 10 ^(oimc$TS/10)

# Denisty (#/nmi^2)
oimc$density <- oimc$NASC/ (4*pi*oimc$sigma_bs)

# Biomass (kg/nmi^2)
oimc$biomass <- oimc$density * (oimc$mean.weight.kg)

# summary
summary(oimc)






##########################################################################################
##########################################################################################
# JOIN BIOMASS TABLES FROM LOOPS --- END 

#bind hake and other species in one table
headers <- c("Region_class","Region_ID","Region_name","Transect","Date","Time","Interval","Lat_S","Lon_S","NASC","TS","sigma_bs","density","biomass")
bio <- rbind(himc[headers],oimc[headers])
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
ddply(int_region, .(Survey, Area, Region_class), summarise, n = length(Region_class))
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
# Survey = 30,700 
# Area 1 = 7,300
# Area 2 = 22,300
# for replicates = 5,900
avg$Area.Biomass.kg <- NA
avg$Area.Biomass.kg[avg$loc == "Survey"] <- avg$Biomass.kg.sqnmi[avg$loc == "Survey"] * 30700
avg$Area.Biomass.kg[avg$loc == "Area1"] <- avg$Biomass.kg.sqnmi[avg$loc == "Area1"] * 7300
avg$Area.Biomass.kg[avg$loc == "Area2"] <- avg$Biomass.kg.sqnmi[avg$loc == "Area2"] * 22300
avg$Area.Biomass.kg[avg$loc == "Replicate1"] <- avg$Biomass.kg.sqnmi[avg$loc == "Replicate1"] * 5900
avg$Area.Biomass.kg[avg$loc == "Replicate2"] <- avg$Biomass.kg.sqnmi[avg$loc == "Replicate2"] * 5900

avg



#######  export #######
write.csv(avg, file = "Other data/Catch data/Biomass.csv")






##########################################################################################
##########################################################################################
# BIOMASS MAPS

#data
bio.survey <- biomass.df[biomass.df$loc == "Survey",]
bio.rep <- biomass.df[biomass.df$loc %in%  c("Replicate1","Replicate2"),]

#land Polygon
data(nepacLL)
lp<-data.frame(nepacLL)

## set colour palette
#cols <- c("#00DDFF", "#FCDF00", "#4daf4a", "#ff7f00", "#984ea3", "#377eb8")
cols <- c("#00DDFF", "#FCDF00", "#ff7f00", "#984ea3", "#377eb8")
lg <- length(unique(bio$Region_class))
leg <- ifelse(lg > length(cols), length(cols), lg)
pal <- colorRampPalette(cols[1:leg],space = c("rgb"),interpolate = c("spline"))(lg)


# survey map
surmap <- ggplot(data = NULL) +
  geom_polygon(data = lp, aes(x = X, y = Y, group=PID), 
               fill = "gray80", colour = "gray60", size = .1) +
  geom_point(data = bio.survey[bio.survey$estimate == "a",], 
             aes(x = Lon_S, y = Lat_S, colour=Region_class, fill=Region_class, size = biomass/1000), 
             pch = 21, alpha=.6) +
  scale_size_area(max_size = 12, name = "Biomass (T)")+
  scale_colour_manual(values=pal, name = "") +
  scale_fill_manual(values=pal, name = "") +
  guides(colour = guide_legend(override.aes = list(size=4)))+
  labs(x = "Longitude", y = "Latitude") +
  coord_map(xlim = c(min(bio.survey$Lon_S)-.2, max(bio.survey$Lon_S)+.2), 
            ylim = c(min(bio.survey$Lat_S)-.2, max(bio.survey$Lat_S)+.2)) +
  theme(panel.border = element_rect(fill=NA, colour="black"),
        panel.background = element_rect(fill="white",colour="black"),
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
        legend.justification = c(1,1), legend.position = "right", 
        plot.margin = unit(c(.5,.5,.5,.5), "lines")) # top, right, bottom, and left 

surmap
pdf("Other data/Figures/BiomassMap_Survey.pdf", width = 9, height = 8)
surmap
dev.off()



# replicate map
repmap <- ggplot(data = NULL) +
  geom_polygon(data = lp, aes(x = X, y = Y, group=PID), 
               fill = "gray80", colour = "gray60", size = .1) +
  geom_point(data = bio.rep[bio.rep$estimate == "a",], 
             aes(x = Lon_S, y = Lat_S, colour=Region_class, fill=Region_class, size = biomass/1000), 
             pch = 21, alpha=.6) +
  facet_wrap(~loc, ncol=2) +
  scale_size_area(max_size = 25, name = "Biomass (T)")+
  scale_colour_manual(values=pal, name = "") +
  scale_fill_manual(values=pal, name = "") +
  guides(colour = guide_legend(override.aes = list(size=4), ncol=1))+
  labs(x = "Longitude", y = "Latitude") +
  coord_map(xlim = c(min(bio.rep$Lon_S)-.2, max(bio.rep$Lon_S)+.2), 
            ylim = c(min(bio.rep$Lat_S)-.2, max(bio.rep$Lat_S)+.2)) +
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
pdf("Other data/Figures/BiomassMap_Replicate.pdf", width = 15, height = 8)
repmap
dev.off()



##########################################################################################
##########################################################################################
# COMPARE TOTAL BIOMASS



# data
area <- avg[avg$loc == "Area1" | avg$loc == "Area2",]
areas <- ddply(area, .(Region_class, estimate), summarise,
               Density.sqnmi = mean(Density.sqnmi),
               Biomass.kg.sqnmi = mean(Biomass.kg.sqnmi),
               Area.Biomass.kg = sum(Area.Biomass.kg))
areas$loc <- "Areas"
survey <- avg[avg$loc == "Survey",]
areas <- rbind(areas, survey)
areas <- areas[areas$estimate == "a",]

replicates <- avg[avg$loc == "Replicate1" | avg$loc == "Replicate2",]
replicates <- replicates[replicates$estimate == "a",]


# estimates plot
comp_est <- ggplot(data = survey) + 
  geom_bar(aes(x = Region_class, y = Area.Biomass.kg/1000, group = estimate, fill = Region_class),
           stat= "identity", position = "dodge", colour = "black", size = .1) +
  geom_text(aes(x=Region_class, y = Area.Biomass.kg/1000, group = estimate, label = estimate), 
            position = position_dodge(width=1), size = 3, vjust = -.3) +
  labs(x="", y=" Total Biomass (T)") +
  scale_fill_manual(values = pal, guide="none") +
  scale_y_continuous(label=comma, expand =c(0,0), 
                     limit= c(0,max(survey$Area.Biomass.kg/1000)*1.05))+
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
pdf("Other data/Figures/Biomass_Estimates.pdf", width = 6, height = 4)
comp_est
dev.off()


# replicate plot
comp_rep <- ggplot(data = replicates) + 
  geom_bar(aes(x = Region_class, y = Area.Biomass.kg/1000, group = loc, fill = Region_class),
           stat= "identity", position = "dodge", colour = "black", size = .1) +
  geom_text(aes(x=Region_class, y = Area.Biomass.kg/1000, group = loc, label = loc), 
            position = position_dodge(width=.9), size = 2.5, vjust = -.3) +
  labs(x="", y=" Total Biomass (T)") +
  scale_fill_manual(values = pal, guide="none") +
  scale_y_continuous(label=comma, expand =c(0,0), 
                     limit= c(0,max(replicates$Area.Biomass.kg/1000)*1.05))+
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
comp_rep
pdf("Other data/Figures/Biomass_Replicates.pdf", width = 6, height = 4)
comp_rep
dev.off()



# area plot
comp_area <- ggplot(data = areas) + 
  geom_bar(aes(x = Region_class, y = Area.Biomass.kg/1000, group = loc, fill = Region_class),
           stat= "identity", position = "dodge", colour = "black", size = .1) +
  geom_text(aes(x=Region_class, y = Area.Biomass.kg/1000, group = loc, label = loc), 
            position = position_dodge(width=.9), size = 2.5, vjust = -.3) +
  labs(x="", y=" Total Biomass (T)") +
  scale_fill_manual(values = pal, guide="none") +
  scale_y_continuous(label=comma, expand =c(0,0), 
                     limit= c(0,max(areas$Area.Biomass.kg/1000)*1.05))+
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
comp_area
pdf("Other data/Figures/Biomass_Survey.pdf", width = 6, height = 4)
comp_area
dev.off()
