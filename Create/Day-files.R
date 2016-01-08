require(RDCOMClient)
require(StreamMetabolism)

#move back to parent directory (cruise name)
setwd('..'); setwd('..')



#location of calibration file (.ecs)
CALdir <- "Acoustics/Echoview"

#location of .raw files
RAWdir <- "Acoustics/RAW"

#location of EV day-files
EVdir <- "Acoustics/Echoview/Day-files"
dir.create(file.path(getwd(), EVdir))

# location of echoveiw template
TEMPdir <- "Acoustics/Echoview/Other/Templates"





###############################################
#             Load in cruisetrack             #
###############################################

# load
track <- read.csv("Acoustics/Echoview/Exports/GPSTrack/CruiseTrack.csv")

# GPS status
track <- track[!track$GPS_status == 1,]

# get one recod for each raw file
track <- track[!duplicated(track$GPS_filename),]

# raw filename
track$filename <- sub(".*/", "", track$GPS_filename)
  
  
# extract datetime
track$GPS_date <- sub(" ", "", track$GPS_date)
track$datetime <- sub(".*D", "", track$GPS_filename)
track$datetime <- sub(".raw", "", track$datetime)
track$datetime <- sub("-", " ", track$datetime)
track$datetime <- as.POSIXct(track$datetime, tz = "UTC", format = "%Y%m%d T%H%M%S")

track <- track[,c(2,3,5,6,9,10)]




###############################################
#              Sunset - Sunrise               #
###############################################

daytrack <- track[!duplicated(track$GPS_date),]

riseset <- NULL
for (i in 1:nrow(daytrack)){
r <- sunrise.set(daytrack$Latitude[i], daytrack$Longitude[i], daytrack$GPS_date[i], timezone = "UTC", num.days = 1)
riseset <- rbind(riseset, r)
}

# reformat day 
daytrack$GPS_date <- gsub("-", "", daytrack$GPS_date)





###############################################
#           Day - Night rawfiles             #
###############################################


# find day raw files
dayfiles <- list()
for (i in 1:nrow(riseset)){
  ind <- which(track$datetime >= riseset$sunrise[i] & track$datetime <= riseset$sunset[i])
  if(length(ind) != 0){
    c.ind <- c(min(ind)-1, ind, max(ind)+1) # complete indices
    r.ind <- c.ind[c.ind %in% 1:(nrow(track))] # only real indices
    day <- daytrack$GPS_date[i]
    dayfiles[[day]] <- track$filename[r.ind]
  }
}


# add first night
nightfiles <- list()
ind <- which(track$datetime <= riseset$sunrise[1])
if(length(ind) != 0){
  f.ind <- c(ind, max(ind)+1) # first indices
  night <- daytrack$GPS_date[1]
  nightfiles[[night]] <- track$filename[f.ind]
}

# find night raw files
for (i in 2:(nrow(riseset)-1)){
  ind <- which(track$datetime <= riseset$sunrise[i+1] & track$datetime >= riseset$sunset[i])
  if(length(ind) != 0){
    c.ind <- c(min(ind)-1, ind, max(ind)+1) # complete indices
    r.ind <- c.ind[c.ind %in% 1:(nrow(track))] # only real indices
    night <- daytrack$GPS_date[i]
    nightfiles[[night]] <- track$filename[r.ind]
  }
}

# add last night
ind <- which(track$datetime >= riseset$sunset[nrow(riseset)])
if(length(ind) != 0){
  l.ind <- c(min(ind)-1, ind) # last indices
  night <- daytrack$GPS_date[nrow(riseset)]
  nightfiles[[night]] <- track$filename[l.ind]
}





###############################################
#              RUN in Echoview                #
###############################################


# create COM connection between R and Echoview
EVApp <- COMCreate("EchoviewCom.EvApplication")

# loop through day files
for (k in (1:length(dayfiles))){
  
  # Open template EV file
  EVfile <- EVApp$OpenFile(file.path(getwd(), TEMPdir, "template.EV"))
  
  # Set fileset object
  filesetObj <- EVfile[["Filesets"]]$Item(0)
  
  # Set calibration file
  CalfilePath <- file.path(getwd(),"Acoustics", "Echoview")
  CALfile <- list.files(CalfilePath, pattern="*.ecs")
  add.calibration <- filesetObj$SetCalibrationFile(file.path(CalfilePath, CALfile))
  
  # Add raw files
  raws <- dayfiles[[k]]
  for (j in raws){
    filesetObj[["DataFiles"]]$Add(file.path(getwd(),RAWdir,j))
  }
  
  # Filename
    datename <- names(dayfiles[k])
    filename <- paste(datename, "_DAY.EV", sep="")
  
  # Save EV file
  EVfile$SaveAS(file.path(getwd(), EVdir ,filename))
  
  # Close EV file
  EVApp$CloseFile(EVfile)
  
}

# loop through night files
for (k in (1:length(nightfiles))){
  
  # Open template EV file
  EVfile <- EVApp$OpenFile(file.path(getwd(), TEMPdir, "template.EV"))
  
  # Set fileset object
  filesetObj <- EVfile[["Filesets"]]$Item(0)
  
  # Set calibration file
  CalfilePath <- file.path(getwd(),"Acoustics", "Echoview")
  CALfile <- list.files(CalfilePath, pattern="*.ecs")
  add.calibration <- filesetObj$SetCalibrationFile(file.path(CalfilePath, CALfile))
  
  # Add raw files
  raws <- nightfiles[[k]]
  for (j in raws){
    filesetObj[["DataFiles"]]$Add(file.path(getwd(),RAWdir,j))
  }
  
  # Filename
  datename <- names(nightfiles[k])
  filename <- paste(datename, "_NIGHT.EV", sep="")
  
  # Save EV file
  EVfile$SaveAS(file.path(getwd(), EVdir ,filename))
  
  # Close EV file
  EVApp$CloseFile(EVfile)
  
}

# quit echoview
EVApp$Quit()
