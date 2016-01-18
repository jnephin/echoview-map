#######################################
## Export Biomass by cells for kriging


# set working directory 
setwd('..');setwd('..')


# load
load("Rscripts/Trawls/Biomassbycell.Rda")
load("Rscripts/Trawls/Cell.Rda")


# add units to colnames
colnames(biomass.df)[13:14] <- c("Density_sqnmi", "Biomass_kg_sqnmi")


# only interested in estimate == a
biomass.a <- biomass.df[biomass.df$estimate == "a",]

# get species biomass tables
a1hake <- biomass.a[biomass.a$Region_class == "Age-1 Hake",
                    c(4:10,13:15)]
hake <- biomass.a[biomass.a$Region_class == "Hake",
                  c(4:10,13:15)]
herring <- biomass.a[biomass.a$Region_class == "Herring",
                     c(4:10,13:15)]
rockfish <- biomass.a[biomass.a$Region_class == "Rockfish",
                      c(4:10,13:15)]


# merge species biomass with zeros from cell integration for each replicate
r1.a1hake <- merge(a1hake[a1hake$loc == "Replicate1",c(4,7:9)], 
                   int_cell[int_cell$Replicate == 1,1:5], 
                   by = "Interval", all.y=TRUE)
r2.a1hake <- merge(a1hake[a1hake$loc == "Replicate2",c(4,7:9)], 
                   int_cell[int_cell$Replicate == 2,1:5], 
                   by = "Interval", all.y=TRUE)
r1.a1hake[is.na(r1.a1hake)] <- 0
r2.a1hake[is.na(r2.a1hake)] <- 0

r1.hake <- merge(hake[hake$loc == "Replicate1",c(4,7:9)], 
                 int_cell[int_cell$Replicate == 1,1:5], 
                 by = "Interval", all.y=TRUE)
r2.hake <- merge(hake[hake$loc == "Replicate2",c(4,7:9)], 
                 int_cell[int_cell$Replicate == 2,1:5], 
                 by = "Interval", all.y=TRUE)
r1.hake[is.na(r1.hake)] <- 0
r2.hake[is.na(r2.hake)] <- 0

r1.herring <- merge(herring[herring$loc == "Replicate1",c(4,7:9)], 
                    int_cell[int_cell$Replicate == 1,1:5], 
                 by = "Interval", all.y=TRUE)
r2.herring <- merge(herring[herring$loc == "Replicate2",c(4,7:9)], 
                    int_cell[int_cell$Replicate == 2,1:5], 
                 by = "Interval", all.y=TRUE)
r1.herring[is.na(r1.herring)] <- 0
r2.herring[is.na(r2.herring)] <- 0

r1.rockfish <- merge(rockfish[rockfish$loc == "Replicate1",c(4,7:9)], 
                     int_cell[int_cell$Replicate == 1,1:5], 
                    by = "Interval", all.y=TRUE)
r2.rockfish <- merge(rockfish[rockfish$loc == "Replicate2",c(4,7:9)], 
                     int_cell[int_cell$Replicate == 2,1:5], 
                    by = "Interval", all.y=TRUE)
r1.rockfish[is.na(r1.rockfish)] <- 0
r2.rockfish[is.na(r2.rockfish)] <- 0


require(ggplot2)

ggplot(data = r2.hake, 
       aes(x = Lon_S, y = Lat_S, size = Biomass_kg_sqnmi))+
  geom_point()+
  scale_size(range = c(1, 6)) +
  #scale_size_area(max_size = 10) +
  theme_bw()





#######  export #######

write.csv(r1.a1hake, file = "Other data/Biomass/Replicate1/Age1Hake_Biomass.csv")
write.csv(r1.hake, file = "Other data/Biomass/Replicate1/Hake_Biomass.csv")
write.csv(r1.herring, file = "Other data/Biomass/Replicate1/Herring_Biomass.csv")
write.csv(r1.rockfish, file = "Other data/Biomass/Replicate1/Rockfish_Biomass.csv")

write.csv(r2.a1hake, file = "Other data/Biomass/Replicate2/Age1Hake_Biomass.csv")
write.csv(r2.hake, file = "Other data/Biomass/Replicate2/Hake_Biomass.csv")
write.csv(r2.herring, file = "Other data/Biomass/Replicate2/Herring_Biomass.csv")
write.csv(r2.rockfish, file = "Other data/Biomass/Replicate2/Rockfish_Biomass.csv")







