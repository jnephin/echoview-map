
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

#location to place  exports
EXPdir <- "Acoustics/Echoview/Exports"
dir.create(file.path(getwd(), EXPdir))

#location to put gps track files
GPSdir <- "Acoustics/Echoview/Exports/GPSTrack"
dir.create(file.path(getwd(), GPSdir))

#location to put log files
LOGdir <- "Acoustics/Echoview/Exports/Log"
dir.create(file.path(getwd(), LOGdir))


###--------------------------------------------####
###################################################