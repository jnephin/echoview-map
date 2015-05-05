
#move back to parent directory (cruise name)
setwd('..'); setwd('..')


          # CHANGE THESE LOCATIONS #

###################################################
###--------------------------------------------####

#location of EV files
EVdir <- "Acoustics/Echoview/Transects"

#location of calibration file (.ecs)
CALdir <- "Other data"

#location of .raw files
RAWdir <- "Acoustics/RAW"


###################################################
        # Makes directories if nessecary #

#location to put gps track files
GPSdir <- "Other data/GPSTrack"
dir.create(file.path(getwd(), GPSdir))

#location to put log files
LOGdir <- "Other data/Log"
dir.create(file.path(getwd(), LOGdir))

#location to place  exports
EXPdir <- "Acoustics/Echoview/Exports"
dir.create(file.path(getwd(), EXPdir))

###--------------------------------------------####
###################################################