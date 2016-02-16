
require(ggplot2)
require(grid)
require(PBSmapping)
require(classInt)
require(plyr)

# move back to parent directory (cruise name)
setwd('..'); setwd('..')



############################################################################
# load catch data
catch <- read.csv("Other data/Catch data/catch.csv", header=T, stringsAsFactors = FALSE)
colnames(catch)[8] <- "SPECIES_DESC"

# load cruise log
log <- read.csv("Acoustics/Echoview/Exports/Log/Cruiselog.csv", header=T, stringsAsFactors = FALSE, row.names=1)
log <- log[!(log$Lat_s == 999),]



############################################################################
# summarise catch data

# add transect # to catch data
trans <- log[grep("SD",log$Region_name),c("Region_name", "File")]
trans$SET <- as.numeric(sub("*.SD", "", trans$Region_name))
trans$Replicate <- gsub("[A-Za-z ]", "", trans$File)
trans$Replicate <- as.numeric(sub("[.].*", "", trans$Replicate))
catch <- merge(catch, trans, by = "SET")

# percent catch by tow
catch$CATCH_WEIGHT_kg[is.na(catch$CATCH_WEIGHT_kg)]  <- 0
catch <- ddply(catch, .(SET), transform, TOTAL_CATCH = sum(CATCH_WEIGHT_kg))
catch$PERCENT <- catch$CATCH_WEIGHT_kg/catch$TOTAL_CATCH*100




############################################################################
##########    export summary    ############

catch_summ <- catch[c("SET","SPECIES_DESC","CATCH_WEIGHT_kg","PERCENT","Replicate")]
write.csv(catch_summ, file = "Other data/Catch data/catch_summary.csv")





############################################################################
# plot percentage of total catch for the survey


## Number of species to include in 'top' species
top <- 6

## total catch data for all sets combined
total.catch <- aggregate(CATCH_WEIGHT_kg ~ SPECIES_DESC + Replicate, sum, data = catch)

# percent catch
total.catch <- ddply(total.catch, .(Replicate), transform,
                     percent = CATCH_WEIGHT_kg/sum(CATCH_WEIGHT_kg)*100)

# top species 
top.species <- aggregate(CATCH_WEIGHT_kg ~ SPECIES_DESC, sum, data = catch)
top.species <- tail(top.species[order(top.species$CATCH_WEIGHT_kg),],top)  

# bind all 'other' than top species
total.catch$SPECIES_DESC[!(total.catch$SPECIES_DESC %in% top.species$SPECIES_DESC)] <- "OTHER"
total.catch <- ddply(total.catch, .(Replicate, SPECIES_DESC), summarize,
                     CATCH_WEIGHT_kg = sum(CATCH_WEIGHT_kg),
                     percent = sum(percent))

# check that percent sums to 1
round(sum(total.catch$percent))/length(unique(total.catch$Replicate)) == 100

#colours
cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#999999","#F0E442", "#0072B2", "#D55E00", "#CC79A7")


## plot
plototal <- ggplot(data = total.catch) +
      geom_bar(aes(x=factor(Replicate), y = percent, fill = SPECIES_DESC), 
               stat = "identity", colour="black", size=.05, width = .9)+
      scale_fill_manual(values = cbPalette, name = "")+
      scale_y_continuous(limits=c(0,100), expand = c(0,0))+
      labs(y = "Percent", x = "Replicate") + 
      theme(panel.border = element_rect(fill=NA, colour="black", size = .1),
            panel.background = element_rect(fill="white",colour="white"),
            axis.ticks = element_line(colour="black"),
            axis.ticks.length = unit(0.1,"cm"),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.text = element_text(size=8, colour = "black"),
            axis.title =  element_text(size=9, colour = "black"),
            legend.text = element_text(size=8),
            legend.title = element_text(size=9, face="plain"),
            legend.key = element_blank(),
            legend.justification = c(0,1), legend.position = "right",
            plot.margin = unit(c(.5,.5,.5,.5), "lines")) # top, right, bottom, and
plototal

pdf("Other data/Figures/TotalCatch_Percent.pdf", width = 6, height = 3.5)
print(plototal)
dev.off()





############################################################################
# Mapped catch data 

#colours
cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

## percent catch top species only
main.catch <- catch[catch$SPECIES_DESC %in% top.species$SPECIES_DESC,]


# merge catch data with xy data from log
sdlog <- log[grep("SD", log$Region_name, ignore.case=TRUE),] #find SD
sets <- unlist(strsplit(sdlog$Region_name, split = 'SD')) #clean SD
sdlog$SET <- sub("^[0]+", "", sets[seq(2, length(sets), 2)]) #remove zeros
catch.xy <- merge(catch, sdlog, by="SET")
set.xy <- aggregate(. ~ SET, mean, data = catch.xy[c("Lat_s","Lon_s", "SET")])

#check --are all the sets present in the echoview log?
all(sort(unique(as.numeric(sdlog$SET)))  %in% unique(catch$SET))

# merge main and analysis catch subsets with xy data from log
main.catch.xy <- merge(main.catch, sdlog, by="SET")


# Regional Polygon 
data(nepacLLhigh)
landT<- thinPolys(nepacLLhigh, tol = 0, filter = 15)
landC <- clipPolys(landT, xlim = c(min(log$Lon_s)-.4, max(log$Lon_s)+.4), 
                       ylim = c(min(log$Lat_s)-.4, max(log$Lat_s)+.4))
land <- data.frame(landC)





##########################################################################
# Map Weight (kg)]
  
breaks <- signif(classIntervals(main.catch.xy$CATCH_WEIGHT_kg, 5, style = "kmeans")$brks,1)[-1]

mapkg <-  ggplot(data = NULL) + 
  # facets
  facet_wrap(~SPECIES_DESC, nrow=2)+
  # land polygon  
  geom_polygon(data = land, aes_string(x = "X", y = "Y", group="PID"), 
               fill = "gray80", colour = "gray60", size = .1) +
  # trawl points
  geom_point(data = main.catch.xy, aes(x = Lon_s, y = Lat_s, 
                                  colour = SPECIES_DESC, size = CATCH_WEIGHT_kg), 
             pch=20)+
  geom_text(data = set.xy, aes(x = Lon_s, y = Lat_s, label = SET), 
            size = 2)+
  scale_size_area(max_size = 12, name = "Weight (kg)", breaks = breaks) +
  scale_colour_manual(values= cbPalette, name = "")+
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
        strip.text = element_text(size=8, colour = "black", vjust = .3),
        axis.text = element_text(size=7, colour = "black"),
        axis.title = element_blank(),
        legend.text = element_text(size=8),
        legend.key = element_blank(),
        legend.justification = c(0,1), legend.position = "right",
        plot.margin = unit(c(.5,.5,.5,.5), "lines"), 
        panel.margin = unit(0.2, "lines")) # top, right, bottom, and
mapkg

pdf("Other data/Figures/CatchWeight_Map.pdf", width = 8.5, height = 5)
print(mapkg)
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
                                       colour = SPECIES_DESC, size = PERCENT), 
             pch=20)+
  geom_text(data = set.xy, aes(x = Lon_s, y = Lat_s, label = SET), size = 2)+
  scale_size_area(max_size = 9, name = "% of tow (by weight)") +
  scale_colour_manual(values= cbPalette, name = "")+
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
        strip.text = element_text(size=8, colour = "black", vjust = .3),
        axis.text = element_text(size=7, colour = "black"),
        axis.title = element_blank(),
        legend.text = element_text(size=8),
        legend.key = element_blank(),
        legend.justification = c(0,1), legend.position = "right",
        plot.margin = unit(c(.5,.5,.5,.5), "lines"), 
        panel.margin = unit(0.2, "lines")) # top, right, bottom, and
mapper

pdf("Other data/Figures/CatchPercent_Map.pdf", width = 8.5, height = 5)
print(mapper)
dev.off()





