
path <- "Other data/Log"

#read in cruise log
if (file.exists(file.path(getwd(), path, "CruiseLog.csv"))) {
  cruiselog <- read.csv(file.path(getwd(), path,"CruiseLog.csv"), header=T, stringsAsFactors = FALSE)
} else {
  stop("No CruiseLog.csv found in '", file.path(getwd(), path), "' directory")
}

#analysis
region <- cruiselog[cruiselog$Region_type == " Analysis", c("Region_class", "Date_s","Time_s","Lat_s","Lon_s","Lat_e","Lon_e","Region_name")]

#analysis region labels
reg <- region[!duplicated(region$Region_class),]
#output region classes for shiny ui
reg.names <- reg$Region_class
dput(reg.names, file="R/Map/Layers/Species")


