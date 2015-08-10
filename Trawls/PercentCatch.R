### load or install
have <- "classInt" %in% installed.packages()[,"Package"]
if (have == FALSE){
  install.packages("classInt", repos = "http://cran.stat.sfu.ca/")
}


require(ggplot2)
require(grid)
require(PBSmapping)
require(classInt)

# set working directory 
setwd('..');setwd('..')

# load catch data
catch <- read.csv("Other data/RAW Catch data from GFBio.csv", header=T, stringsAsFactors = FALSE)

# load cruise log
log <- read.csv("Other data/Log/Cruiselog.csv", header=T, stringsAsFactors = FALSE)
log <- log[!(log$Lat_s == 999),]





############################################################################
# percent catch by tow
catch$FE_TOTAL_CATCH_WEIGHT[is.na(catch$FE_TOTAL_CATCH_WEIGHT)] <- 0
catch$WT[is.na(catch$WT)] <- 0
catch$PERCENT <- catch$WT/catch$FE_TOTAL_CATCH_WEIGHT*100


## total catch data for all sets combined
total.catch <- aggregate(WT ~ SPECIES_DESC, sum, data = catch)


## Number of species to include in 'top' species
top <- 6


## set colour palette
cols <- c("#e41a1c", "#ff7f00", "#FFD900",  "#4daf4a","#377eb8", "#984ea3", "#f781bf", "#a65628","#999999")
leg <- ifelse(top > length(cols), length(cols), top)
pal <- colorRampPalette(cols[1:leg],space = c("rgb"),interpolate = c("spline"))(top)




############################################################################
# plot percentage of total catch for the survey

# percent catch
total.catch$percent <- total.catch$WT/sum(total.catch$WT)*100

# top species 
top.catch <- tail(total.catch[order(total.catch$percent),],top)  
top.catch$SPECIES_DESC <- as.factor(top.catch$SPECIES_DESC)

# group the rest into 'other'
other.catch <- head(total.catch[order(total.catch$percent),],nrow(total.catch)-top) 
other <- data.frame(SPECIES_DESC="OTHER",
                    WT = sum(other.catch$WT), 
                    percent = sum(other.catch$percent))

# bind together and order
top.catch <- rbind(top.catch, other)
top.catch$WT <- round(top.catch$WT)
top.catch <- top.catch[order(top.catch$percent),]
ord.catch <- top.catch[order(top.catch$SPECIES_DESC),]


# check that percent sums to 1
sum(top.catch$percent)


# colours
pal.t <- colorRampPalette(cols[1:(leg+1)],space = c("rgb"),interpolate = c("spline"))(top + 1)

## plot
plototal <- ggplot(data = top.catch) +
      geom_bar(aes(x=1, y = percent, fill = SPECIES_DESC), stat = "identity", colour="black", size=.05)+
      scale_fill_manual(values= pal.t, name = "Species", 
                        labels = paste(ord.catch$SPECIES_DESC," - ",ord.catch$WT,"kg", sep=""),
                        guide=guide_legend(override.aes = list(colour=NA)))+
      scale_y_continuous(limits=c(0,100))+
      coord_polar(theta = "y")+
      theme_bw()+
      theme(panel.grid.major = element_blank(),
        panel.border = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        legend.text = element_text(size=7),
        legend.key.size = unit(.4, "cm"),
        legend.key =  element_blank(),
        legend.title = element_text(size=9, face="plain"),
        plot.margin = unit(c(0,0,0,0), "lines"))
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
  temp <- catch[grep(i, catch$SPECIES_DESC, ignore.case=TRUE),]
  analysis.catch <- rbind(analysis.catch, temp)
}

# colours
leg.a <- ifelse(top > length(cols), length(cols), top)
pal.a <- colorRampPalette(cols[1:(leg+1)],space = c("rgb"),interpolate = c("spline"))(top + 1)


## plot
ggplot(data = analysis.catch) +
  geom_bar(aes(x=factor(SET), y = PERCENT, fill = SPECIES_DESC), stat = "identity")+
  scale_fill_manual(values= pal, name = "Species")+
  labs(x="SET")+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())



############################################################################
##  plot percent catch per set 
# 2) for top species only

main.species <- tail(total.catch$SPECIES_DESC[order(total.catch$WT)],top)             
main.catch <- catch[catch$SPECIES_DESC %in% main.species,]

# plot
ggplot(data = main.catch) +
  geom_bar(aes(x=factor(SET), y = PERCENT, fill = SPECIES_DESC), stat = "identity")+
  scale_fill_manual(values= pal, name = "Species")+
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
# Map Weight (kg)]

breaks <- signif(classIntervals(main.catch.xy$WT, 5, style = "kmeans")$brks,1)[-1]

  
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
  scale_size_area(max_size = 12, name = "Weight (kg)", breaks = breaks) +
  scale_colour_manual(values= pal, name = "Species")+
  # spatial extent
  coord_map(xlim = c(min(log$Lon_s)-.5, max(log$Lon_s)+.5), 
          ylim = c(min(log$Lat_s)-.5, max(log$Lat_s)+.5)) +
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
        panel.margin = unit(0.2, "lines")) # top, right, bottom, and
mapkg

pdf("Other data/Figures/CatchWeight_Map.pdf", width = 8.5, height = 5)
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
  scale_size_area(max_size = 9, name = "% of tow (by weight)") +
  scale_colour_manual(values= pal, name = "Species")+
  # spatial extent
  coord_map(xlim = c(min(log$Lon_s)-.5, max(log$Lon_s)+.5), 
            ylim = c(min(log$Lat_s)-.5, max(log$Lat_s)+.5)) +
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
        panel.margin = unit(0.2, "lines")) # top, right, bottom, and
mapper

pdf("Other data/Figures/CatchPercent_Map.pdf", width = 8.5, height = 5)
mapper
dev.off()




