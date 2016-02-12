
require(ggplot2)
require(grid)
require(PBSmapping)
require(classInt)
require(plyr)

#move back to parent directory (cruise name)
setwd('..'); setwd('..')

# load catch data
catch <- read.csv("Other data/Catch data/catch.csv", header=T, stringsAsFactors = FALSE)
reps <- read.csv("Other data/Catch data/replicates.csv", header=T, stringsAsFactors = FALSE)


# load cruise log
log <- read.csv("Acoustics/Echoview/Exports/Log/Cruiselog.csv", header=T, stringsAsFactors = FALSE, row.names=1)
log <- log[!(log$Lat_s == 999),]
trans <- read.csv("Other data/Catch data/transects.csv", header=T, stringsAsFactors = FALSE)





############################################################################
# group by species by tow
catch <- ddply(catch, .(FE_MAJOR_LEVEL_ID, SPECIES_COMMON_NAME, SPECIES_SCIENCE_NAME, FE_TOTAL_CATCH_WEIGHT), 
               summarise, CATCH_WEIGHT = sum(CATCH_WEIGHT))

# percent catch by tow
catch$FE_TOTAL_CATCH_WEIGHT[is.na(catch$FE_TOTAL_CATCH_WEIGHT)] <- 0
catch$CATCH_WEIGHT[is.na(catch$CATCH_WEIGHT)] <- 0
catch$PERCENT <- catch$CATCH_WEIGHT/catch$FE_TOTAL_CATCH_WEIGHT*100
colnames(catch)[1] <- "SET"

# merge catch and log with replicate info
catch <- merge(catch, reps, by = "SET")
log <- merge(log, trans, by = "File")

# export summary
catch_summ <- catch[c("SET","SPECIES_COMMON_NAME","SPECIES_SCIENCE_NAME", "CATCH_WEIGHT","PERCENT", "Survey")]
write.csv(catch_summ, file = "Other data/Catch data/catch_summary.csv")




############################################################################
############################################################################

# --  repeat script once for each survey
log <- log[log$Survey == 2, ]
catch <- catch[catch$Survey == 2, ]

############################################################################
############################################################################



## total catch data for all sets combined
total.catch <- aggregate(CATCH_WEIGHT ~ SPECIES_COMMON_NAME, sum, data = catch)


## Number of species to include in 'top' species
top <- 6


## set colour palette
cols <- c("#e41a1c", "#ff7f00", "#FFD900",  "#4daf4a","#377eb8", "#984ea3", "#f781bf", "#a65628","#999999")
leg <- ifelse(top > length(cols), length(cols), top)
pal <- colorRampPalette(cols[1:leg],space = c("rgb"),interpolate = c("spline"))(top)




############################################################################
# plot percentage of total catch for the survey

# percent catch
total.catch$percent <- total.catch$CATCH_WEIGHT/sum(total.catch$CATCH_WEIGHT)*100

# top species 
top.catch <- tail(total.catch[order(total.catch$percent),],top)  
top.catch$SPECIES_COMMON_NAME <- as.factor(top.catch$SPECIES_COMMON_NAME)

# group the rest into 'other'
other.catch <- head(total.catch[order(total.catch$percent),],nrow(total.catch)-top) 
other <- data.frame(SPECIES_COMMON_NAME="OTHER",
                    CATCH_WEIGHT = sum(other.catch$CATCH_WEIGHT), 
                    percent = sum(other.catch$percent))

# bind together and order
top.catch <- rbind(top.catch, other)
top.catch$CATCH_WEIGHT <- round(top.catch$CATCH_WEIGHT)
top.catch <- top.catch[order(top.catch$percent),]
ord.catch <- top.catch[order(top.catch$SPECIES_COMMON_NAME),]


# check that percent sums to 1
sum(top.catch$percent)


# colours
pal.t <- colorRampPalette(cols[1:(leg+1)],space = c("rgb"),interpolate = c("spline"))(top + 1)

## plot
plototal <- ggplot(data = top.catch) +
  geom_bar(aes(x=1, y = percent, fill = SPECIES_COMMON_NAME), stat = "identity", 
           colour="black", size=.05)+
  scale_fill_manual(values= pal.t, name = "", 
                    labels = paste(ord.catch$SPECIES_COMMON_NAME," - ",
                                   ord.catch$CATCH_WEIGHT,"kg", sep=""),
                    guide=guide_legend(override.aes = list(colour=NA)))+
  scale_y_continuous(limits=c(0,100))+
  coord_polar(theta = "y")+
  theme(panel.border = element_blank(),
        panel.background = element_blank(),
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        legend.text = element_text(size=13),
        legend.background = element_blank(), legend.key = element_blank(),
        legend.position = "right",
        plot.margin = unit(c(.2,.2,.2,.2), "lines"))
plototal

pdf("Other data/Figures/TotalCatch_Percent.pdf", width = 6, height = 3.5)
plototal
dev.off()




############################################################################
##  plot percent catch per set 
# 1) for species in analysis region only

species <- unique(log$Region_class[log$Region_type == " Analysis"])
analysis.catch <- NULL
for (i in species){
  temp <- catch[grep(i, catch$SPECIES_COMMON_NAME, ignore.case=TRUE),]
  analysis.catch <- rbind(analysis.catch, temp)
}

# colours
leg.a <- ifelse(top > length(cols), length(cols), top)
pal.a <- colorRampPalette(cols[1:(leg+1)],space = c("rgb"),interpolate = c("spline"))(top + 1)


## plot
ggplot(data = analysis.catch) +
  geom_bar(aes(x=factor(SET), y = PERCENT, fill = SPECIES_COMMON_NAME), stat = "identity")+
 # scale_fill_manual(values= pal, name = "Species")+
  labs(x="SET")+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())



############################################################################
##  plot percent catch per set 
# 2) for top species only

main.species <- tail(total.catch$SPECIES_COMMON_NAME[order(total.catch$CATCH_WEIGHT)],top)             
main.catch <- catch[catch$SPECIES_COMMON_NAME %in% main.species,]

# plot
ggplot(data = main.catch) +
  geom_bar(aes(x=factor(SET), y = PERCENT, fill = SPECIES_COMMON_NAME), stat = "identity")+
  scale_fill_manual(values= pal, name = "Species")+
  labs(x="SET")+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())





############################################################################
# Georeference catch data 

# merge catch data with xy data from log
sdlog <- log[grep("SD", log$Region_name, ignore.case=TRUE),] #find SD
sets <- unlist(strsplit(sdlog$Region_name, split = 'SD')) #clean SD
sdlog$SET <- sub("^[0]+", "", sets[seq(2, length(sets), 2)]) #remove zeros
catch.xy <- merge(catch, sdlog, by="SET")
set.xy <- aggregate(. ~ SET, mean, data = catch.xy[c("Lat_s","Lon_s", "SET")])

#check --are all the sets present in the echoview log?
sort(unique(as.numeric(sdlog$SET)))
unique(catch$SET)

# merge main and analysis catch subsets with xy data from log
main.catch.xy <- merge(main.catch, sdlog, by="SET")
analysis.catch.xy <- merge(analysis.catch, sdlog, by="SET")


# Regional Polygon 
data(nepacLLhigh)
landT<- thinPolys(nepacLLhigh, tol = 0, filter = 15)
landC <- clipPolys(landT, xlim = c(min(log$Lon_s)-1, max(log$Lon_s)+1), 
                       ylim = c(min(log$Lat_s)-1, max(log$Lat_s)+1))
land <- data.frame(landC)





##########################################################################
# Map Weight (kg)]

breaks <- signif(classIntervals(main.catch.xy$CATCH_WEIGHT, 5, style = "kmeans")$brks,1)[-1]

mapkg <-  ggplot(data = NULL) + 
  # facets
  facet_wrap(~SPECIES_COMMON_NAME, nrow=2)+
  # land polygon  
  geom_polygon(data = land, aes_string(x = "X", y = "Y", group="PID"), 
               fill = "gray80", colour = "gray60", size = .1) +
  # trawl points
  geom_point(data = main.catch.xy, aes(x = Lon_s, y = Lat_s, 
                                  colour = SPECIES_COMMON_NAME, size = CATCH_WEIGHT), pch=20)+
  geom_text(data = set.xy, aes(x = Lon_s, y = Lat_s, label = SET), pch = 4, size = 2)+
  scale_size_area(max_size = 12, name = "Weight (kg)", breaks = breaks) +
  scale_colour_manual(values= pal, name = "Species")+
  # spatial extent
  coord_map(xlim = c(min(log$Lon_s)-.2, max(log$Lon_s)+.2), 
          ylim = c(min(log$Lat_s)-.2, max(log$Lat_s)+.2)) +
  # guides
  guides(colour = guide_legend(order = 1, override.aes = list(size=4)))+
  # themes
  theme(panel.border = element_rect(fill=NA, colour="black", size = .1),
        panel.background = element_rect(fill="white",colour="white"),
        strip.background = element_blank(),
        axis.ticks = element_line(colour="black"),
        axis.ticks.length = unit(0.1,"cm"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.text = element_text(size=9, colour = "black", vjust = .3),
        axis.text = element_text(size=7, colour = "black"),
        axis.title = element_blank(),
        legend.text = element_text(size=8),
        legend.title = element_text(size=9, face="plain"),
        legend.key = element_blank(),
        legend.justification = c(0,1), legend.position = "right",
        plot.margin = unit(c(.5,.5,.5,.5), "lines"), 
        panel.margin = unit(0.3, "lines")) # top, right, bottom, and
mapkg

pdf("Other data/Figures/CatchWeight_Map.pdf", width = 8.5, height = 5)
mapkg
dev.off()


##########################################################################
# Map Percent Weight (%)

mapper <-  ggplot(data = NULL) + 
  # facets
  facet_wrap(~SPECIES_COMMON_NAME, nrow=2)+
  # land polygon  
  geom_polygon(data = land, aes_string(x = "X", y = "Y", group="PID"), 
               fill = "gray80", colour = "gray60", size = .1) +
  # trawl points
  geom_point(data = main.catch.xy, aes(x = Lon_s, y = Lat_s, 
                                       colour = SPECIES_COMMON_NAME, size = PERCENT), pch=20)+
  geom_text(data = set.xy, aes(x = Lon_s, y = Lat_s, label = SET), pch = 4, size = 2)+
  scale_size_area(max_size = 9, name = "% of tow (by weight)") +
  scale_colour_manual(values= pal, name = "Species")+
  # spatial extent
  coord_map(xlim = c(min(log$Lon_s)-.2, max(log$Lon_s)+.2), 
            ylim = c(min(log$Lat_s)-.2, max(log$Lat_s)+.2)) +
  # guides
  guides(colour = guide_legend(order = 1, override.aes = list(size=4)))+
  # themes
  theme(panel.border = element_rect(fill=NA, colour="black", size = .1),
        panel.background = element_rect(fill="white",colour="white"),
        strip.background = element_blank(),
        axis.ticks = element_line(colour="black"),
        axis.ticks.length = unit(0.1,"cm"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.text = element_text(size=9, colour = "black", vjust = .3),
        axis.text = element_text(size=7, colour = "black"),
        axis.title = element_blank(),
        legend.text = element_text(size=8),
        legend.title = element_text(size=9, face="plain"),
        legend.key = element_blank(),
        legend.justification = c(0,1), legend.position = "right",
        plot.margin = unit(c(.5,.5,.5,.5), "lines"), 
        panel.margin = unit(0.3, "lines")) # top, right, bottom, and
mapper

pdf("Other data/Figures/CatchPercent_Map.pdf", width = 8.5, height = 5)
mapper
dev.off()




