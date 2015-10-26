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
summ <- read.csv("Other data/Fishing/morpho_summary.csv", header=T, stringsAsFactors = FALSE, 
                 row.names = 1)

# load mean lengths, weights and counts per set per species
morph <- read.csv("Other data/Fishing/morpho_counts.csv", header=T, stringsAsFactors = FALSE, 
                  row.names = 1)

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

# load survey data
trans <- read.csv("Other data/Fishing/transects.csv", header=T, stringsAsFactors = FALSE)



#### Choose target strength - length regression constants (a or b estimtes)
con <- "b"


#### Choose replicate or Area
#reps <- 2
 Area <- 2



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





############################################################################
############################################################################
## MERGE SURVEY INFO

## update trans Transects
trans$Transect <- sub(".csv", "", trans$File)

## merge nasc and log with replicate info
dim(int_regions)
log <- merge(log, trans, by = "File")
int_regions <- merge(int_regions, trans, by = "Transect")
int_cells <- merge(int_cells, trans, by = "Transect")
dim(int_regions)


# --  repeat script once for each survey or replicate or area 
log <- log[log$Survey == 1 & log$Area == Area, ] #log$Replicate == "y",] 
int_regions <- int_regions[int_regions$Survey == 1 &  int_regions$Area == Area, ] # int_regions$Replicate == "y",]
int_cells <- int_cells[int_cells$Survey == 1 &  int_cells$Area == Area, ] # int_cells$Replicate == "y",]


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
mixed <- c("Hake mix")

# mixed regions from log
matched <- log[log$Region_class %in% mixed,c("Region_ID","Region_name","Region_class","File")]
matched$File <- unlist(strsplit(matched$File, "[.]csv"))
colnames(matched)[4] <- "Transect"


# add matching sets
matched$sets <- sub(".*set","",matched$Region_name) # remove everything before "set"
matched$sets <- sub("[A-z ]*","",matched$sets) # remove leading letters and spaces
matched$sets <- sub("-.*","",matched$sets) # remove everything after dash
matched$sets <- sub(".*A","",matched$sets) # remove everything before A

# mean weight, length and count data for mixed sets
mat <- morph[morph$SET %in% matched$sets,]


# merge coeff data with mat
mat <- merge(mat, coeff[coeff$choice == con,c(1,2,3,7)], by.x = "SPECIES_COMMON_NAME", by.y = "Region_class")

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
          for (z in rat$SPECIES_COMMON_NAME){
            pdat <- int_s
            pdat$Region_class <- z
            pdat$NASC <- pdat$NASC * (rat$R[rat$SPECIES_COMMON_NAME == z])
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




## Update mixed anlaysis regions in log


# merge matched sets with analysis
man <- merge(analysis, matched, by=c("Region_ID", "Region_name", "Region_class", "Transect"))

# add new classes for mixed regions
mix_reg <- NULL
for (y in unique(man$sets)){
  # partiton data by reference set
  sp_sets <- mat[mat$SET == y,]
  man_sets <- man[man$sets == y,]
  # create table for each species of mix
  for (z in sp_sets$SPECIES_COMMON_NAME){
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

## check analysis regions 
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





##########################################################################################
##########################################################################################
# biomass calculation for HAKE


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
himc <- merge(him, coeff[coeff$choice == con,c(1,2,3,7)], by = "Region_class")


# biomass calculations

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
# biomass calculation for all other regions

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
oimc <- merge(oim, coeff[coeff$choice == con,c(1,2,3,7)], by = "Region_class")


# biomass calculations

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
# Average biomass 

#bind hake and other species in one table
headers <- c("Region_class","Region_ID","Region_name","Transect","Date","Time","Interval","Lat_S","Lon_S","NASC","TS","sigma_bs","density","biomass")
bio <- rbind(himc[headers],oimc[headers])
assign(paste("bio", Area, sep="_"), bio)

# average biomass (kg per nmi^2) per species per transect over their regions
tmp <- ddply(bio, .(Region_class, Transect), summarise, 
             Density = mean(density),
             Biomass = mean(biomass))


# merge with distance data
mrg <- merge(tmp, distance, by = c("Region_class", "Transect"))

# corrected mean density and biomass values to account for zeros
mrg$Density <- mrg$Density * mrg$reg_prop
mrg$Biomass <- mrg$Biomass * mrg$reg_prop  


# average biomass (kg per nmi^2) per species over the survey area
# transect density weight by transect/survey length to caluclate total density
sums <- ddply(mrg, .(Region_class), summarise, 
              Density.sqnmi = sum(Density * trans_prop),
              Biomass.kg.sqnmi = sum(Biomass * trans_prop))
sums$Estimate <- con
#sums$Replicate <- reps
sums$Area <- Area


# scale up biomass to area of interest
# area of survey (spatial extent) 
# for Hake 2015 = 30,700 
# survey 1, Area 1 = 7,300
# survey 1, Area 2 = 22,300
# for replicates = 5,900
sums$Area.Biomass.kg <- round(sums$Biomass.kg.sqnmi[sums$Area == 1] * 7300)
sums$Area.Biomass.kg <- round(sums$Biomass.kg.sqnmi[sums$Area == 2] * 22300)
#sums$Area.Biomass.kg <- round(sums$Biomass.kg.sqnmi * 5900)




# average biomass (kg per nmi^2) per species over the survey area 
assign(paste("avg", con, Area, sep="_"), sums)
get(paste("avg", con, Area, sep="_"))





# ---------------------------------------------------------------------#
# RERUN script with other estimates 

# bind density and biomass from difference estimates
avg <- rbind(avg_a_1,avg_a_2, avg_b_1, avg_b_2)
avg <- avg[order(avg$Region_class),]
avg

# add total biomass from both Areas
tot <- ddply(avg, .(Region_class, Estimate), summarise, Total.Biomass.T = sum(Area.Biomass.kg/1000))

# exports
#write.csv(avg, file = "Other data/Fishing/Biomass_Replicate.csv")
write.csv(avg, file = "Other data/Fishing/Biomass_Survey.csv")
write.csv(tot, file = "Other data/Fishing/BiomassTotals_Survey.csv")






##########################################################################################
##########################################################################################
# Map biomass 

bio <- rbind(bio_1,bio_2)
bio$Replicate <- c(rep(1,nrow(bio_1)), rep(2,nrow(bio_2)))
             
#land Polygon
data(nepacLL)
lp<-data.frame(nepacLL)

## set colour palette
#cols <- c("#00DDFF", "#FCDF00", "#4daf4a", "#ff7f00", "#984ea3", "#377eb8")
cols <- c("#00DDFF", "#FCDF00", "#ff7f00", "#984ea3", "#377eb8")
lg <- length(unique(bio$Region_class))
leg <- ifelse(lg > length(cols), length(cols), lg)
pal <- colorRampPalette(cols[1:leg],space = c("rgb"),interpolate = c("spline"))(lg)


# map biomass
biomap <- ggplot(data = NULL) +
  geom_polygon(data = lp, aes(x = X, y = Y, group=PID), 
               fill = "gray80", colour = "gray60", size = .1) +
  geom_point(data = bio, aes(x = Lon_S, y = Lat_S, colour=Region_class, size = biomass), pch = 20, alpha=.7) +
  #facet_wrap(~Replicate, ncol=2) +
  scale_size_area(max_size = 16, name = "Biomass")+
  scale_colour_manual(values=pal) +
  guides(colour = guide_legend(override.aes = list(size=4)))+
  labs(x = "Longitude", y = "Latitude") +
  coord_map(xlim = c(min(bio$Lon_S)-.2, max(bio$Lon_S)+.2), 
            ylim = c(min(bio$Lat_S)-.2, max(bio$Lat_S)+.2)) +
  guides(colour = guide_legend(override.aes = list(size=4)))+
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
        legend.justification = c(1,1), legend.position = "right", 
        #legend.justification = c(.5,.5), legend.position = "bottom", legend.box = "horizontal",
        plot.margin = unit(c(.5,.5,.5,.5), "lines")) # top, right, bottom, and left 

biomap
pdf("Other data/Figures/BiomassMap_Survey.pdf", width = 9, height = 8)
#pdf("Other data/Figures/BiomassMap_Replicate.pdf", width = 15, height = 8)
biomap
dev.off()



###################################################################################
# plot biomass comparison

comp_est <- ggplot(data = tot) + 
  geom_bar(aes(x = Region_class, y = Total.Biomass.T, group = Estimate, fill = Region_class),
           stat= "identity", position = "dodge", colour = "black", size = .1) +
  geom_text(aes(x=Region_class, y = Total.Biomass.T, group = Estimate, label = Estimate), 
            position = position_dodge(width=1), size = 3, vjust = -.3) +
  labs(x="", y=" Total Biomass (mT)") +
  scale_fill_manual(values = pal, guide="none") +
  scale_y_continuous(label=comma, expand =c(0,0), limit= c(0,1560000))+
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
pdf("Other data/Figures/Biomass_Estimates.pdf", width = 8, height = 5)
comp_est
dev.off()



comp_rep <- ggplot(data = avg) + 
  geom_bar(aes(x = Region_class, y = Area.Biomass.kg/1000, group = Replicate, fill = Region_class),
           stat= "identity", position = "dodge", colour = "black", size = .1) +
  geom_text(aes(x=Region_class, y = Area.Biomass.kg/1000, group = Replicate, label = Replicate), 
            position = position_dodge(width=1), size = 3, vjust = -.3) +
  labs(x="", y=" Total Biomass (mT)") +
  scale_fill_manual(values = pal, guide="none") +
  scale_y_continuous(label=comma, expand =c(0,0), limit= c(0,1560000))+
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


