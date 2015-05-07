require(ggplot2)
library(ggsubplot)
require(grid)
require(PBSmapping)


# set working directory 
setwd('..');setwd('..')

# load catch data
catch <- read.csv("Other data/RAW Catch data from GFBio.csv", header=T, stringsAsFactors = FALSE)

# load cruise log
log <- read.csv("Other data/Log/Cruiselog.csv", header=T, stringsAsFactors = FALSE)
log <- log[!(log$Lat_s == 999),]





############################################################################
# get the percent catch by tow
catch$FE_TOTAL_CATCH_WEIGHT[is.na(catch$FE_TOTAL_CATCH_WEIGHT)] <- 0
catch$WT[is.na(catch$WT)] <- 0
catch$PERCENT <- catch$WT/catch$FE_TOTAL_CATCH_WEIGHT*100





############################################################################
# subset catch data

# 1) only inlcude species in cruise log analysis regions
species <- unique(log$Region_class[log$Region_type == " Analysis"])
analysis.catch <- NULL
for (i in species){
temp <- catch[grep(i, catch$SPECIES_DESC, ignore.case=TRUE),]
analysis.catch <- rbind(analysis.catch, temp)
}

# plot
ggplot(data = analysis.catch) +
  geom_bar(aes(x=factor(SET), y = PERCENT, fill = SPECIES_DESC), stat = "identity")+
  scale_fill_brewer(palette="Set1", name = "Species")+
  labs(x="SET")+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())


# 2) only inlcude top 6 species by weight across all tows
main.species <- aggregate(WT ~ SPECIES_DESC, sum, data = catch)
main.species <- tail(main.species$SPECIES_DESC[order(main.species$WT)],6)             
main.catch <- catch[catch$SPECIES_DESC %in% main.species,]

# plot
ggplot(data = main.catch) +
  geom_bar(aes(x=factor(SET), y = PERCENT, fill = SPECIES_DESC), stat = "identity")+
  scale_fill_brewer(palette="Set1", name = "Species")+
  labs(x="SET")+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())




############################################################################
# Georeference catch data 

# merge catch data with xy data from log
sdlog <- log[log$Region_class == " SD",]
sdlog$SET <- grep( "0+" , sdlog$Region_name)
catch.xy <- merge(catch, sdlog, by="SET")
set.xy <- aggregate(. ~ SET, mean, data = catch.xy[c("Lat_s","Lon_s", "SET")])

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
# Map Weight (kg)
mapkg <-  ggplot(data = NULL) + 
  # facets
  facet_wrap(~SPECIES_DESC, nrow=2)+
  # land polygon  
  geom_polygon(data = land, aes_string(x = "X", y = "Y", group="PID"), 
               fill = "gray80", colour = "gray60", size = .1) +
  # trawl points
  geom_point(data = main.catch.xy, aes(x = Lon_s, y = Lat_s, 
                                  colour = SPECIES_DESC, size = WT), pch=20)+
  geom_text(data = set.xy, aes(x = Lon_s, y = Lat_s, label = SET), pch = 4, size = 2)+
  scale_size_area(max_size = 9, name = "Weight (kg)") +
  scale_colour_brewer(palette="Dark2", name = "Species")+
  # spatial extent
  coord_map(xlim = c(min(log$Lon_s)-.5, max(log$Lon_s)+.5), 
          ylim = c(min(log$Lat_s)-.5, max(log$Lat_s)+.5)) +
  # guides
  guides(colour = guide_legend(override.aes = list(size=4)))+
  # themes
  theme(panel.border = element_rect(fill=NA, colour="black", size = .1),
        panel.background = element_rect(fill="white",colour="white"),
        strip.background = element_blank(),
        axis.ticks = element_line(colour="black"),
        axis.ticks.length = unit(0.1,"cm"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.text = element_text(size=9, colour = "black", vjust = .3),
        axis.text = element_text(size=8, colour = "black"),
        axis.title = element_blank(),
        legend.text = element_text(size=8),
        legend.title = element_text(size=9, face="plain"),
        legend.key = element_blank(),
        legend.justification = c(0,1), legend.position = "right",
        plot.margin = unit(c(.5,.5,.5,.5), "lines"), 
        panel.margin = unit(0.2, "lines")) # top, right, bottom, and

pdf("Other data/Maps/CatchWeight_kg.pdf", width = 8.5, height = 5)
mapkg
dev.off()


##########################################################################
# Map Percent Weight (%)
mapper <-  ggplot(data = NULL) + 
  # facets
  facet_wrap(~SPECIES_DESC, nrow=2)+
  # land polygon  
  geom_polygon(data = land, aes_string(x = "X", y = "Y", group="PID"), 
               fill = "gray80", colour = "gray60", size = .1) +
  # trawl points
  geom_point(data = main.catch.xy, aes(x = Lon_s, y = Lat_s, 
                                       colour = SPECIES_DESC, size = PERCENT), pch=20)+
  geom_text(data = set.xy, aes(x = Lon_s, y = Lat_s, label = SET), pch = 4, size = 2)+
  scale_size_area(max_size = 9, name = "Weight (% of tow)") +
  scale_colour_brewer(palette="Dark2", name = "Species")+
  # spatial extent
  coord_map(xlim = c(min(log$Lon_s)-.5, max(log$Lon_s)+.5), 
            ylim = c(min(log$Lat_s)-.5, max(log$Lat_s)+.5)) +
  # guides
  guides(colour = guide_legend(override.aes = list(size=4)))+
  # themes
  theme(panel.border = element_rect(fill=NA, colour="black", size = .1),
        panel.background = element_rect(fill="white",colour="white"),
        strip.background = element_blank(),
        axis.ticks = element_line(colour="black"),
        axis.ticks.length = unit(0.1,"cm"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.text = element_text(size=9, colour = "black", vjust = .3),
        axis.text = element_text(size=8, colour = "black"),
        axis.title = element_blank(),
        legend.text = element_text(size=8),
        legend.title = element_text(size=9, face="plain"),
        legend.key = element_blank(),
        legend.justification = c(0,1), legend.position = "right",
        plot.margin = unit(c(.5,.5,.5,.5), "lines"), 
        panel.margin = unit(0.2, "lines")) # top, right, bottom, and
mapper

pdf("Other data/Maps/CatchWeight_Percent.pdf", width = 8.5, height = 5)
mapper
dev.off()


