require(RDCOMClient)

#move back to parent directory (cruise name)
setwd('..'); setwd('..')


###---------------------------------------------###

#location of calibration file (.ecs)
CALdir <- "Acoustics/Echoview"

#location of .raw files
RAWdir <- "Acoustics/RAW"

#location of EV files
EVOlddir <- "Acoustics/Echoview/Transects/Old Template"
EVdir <- "Acoustics/Echoview/Transects"
dir.create(file.path(getwd(), EVOlddir))

# location of echoveiw template
TEMPdir <- "Acoustics/Echoview/Other/Templates"

#location of Regions Exports
EVregs <- "Acoustics/Echoview/Exports/Regions"
dir.create(file.path(getwd(), EVregs))

#location of Line Exports
EVlines <- "Acoustics/Echoview/Exports/Lines"
dir.create(file.path(getwd(), EVlines))

###---------------------------------------------###


### list the EV files
EVfile.list <- list.files(file.path(getwd(), EVdir), pattern="*.EV")


### move old ev files to old template directory
file.copy(file.path(getwd(), EVdir, EVfile.list), file.path(getwd(), EVOlddir))
file.remove(file.path(getwd(), EVdir, EVfile.list))



###############################################
#                   EXPORT                    #
###############################################


##----------- loop through EV files ---------------##
for (i in EVfile.list){
  
  # create COM connection between R and Echoview
  EVApp <- COMCreate("EchoviewCom.EvApplication")
  
  EVfileName <- file.path(getwd(), EVOlddir, i)
  
  # Open existing EV file
  EVfile <- EVApp$OpenFile(EVfileName)
  
  # get the first fileset object 
  filesetObj <- EVfile[["Filesets"]]$Item(0)
  
  # list raw files in EV file
  rawnumber <- filesetObj[["DataFiles"]]$Count()
  rawfiles <- NULL
  for (j in 0:(rawnumber-1)){
    dataObj <- filesetObj[["DataFiles"]]$Item(j)
    g <- dataObj$FileName()
    rawfiles <- c(rawfiles, g)
  }
  
  # export regions
  regionfilename <- paste(file.path(getwd(), EVregs, i), "evr", sep = ".")
  EVfile[["Regions"]]$ExportDefinitionsAll(regionfilename)
  
  # set EvVariable object
  evVarObj <- EVfile[["Variables"]]$Item(0)
  
  # export lines
  linenumber <- EVfile[["Lines"]]$Count()
  for (k in 0:(linenumber-1)){
    lineObj <- EVfile[["Lines"]]$Item(k)
    linename <- lineObj$Name()
    linefilename <- paste(file.path(getwd(), EVlines, linename),i, "evl", sep = ".")
    linefilename <- sub(": ","_",linefilename)
    evVarObj$ExportLine(lineObj,linefilename, -1, -1)
  }
  
  
  # Close EV file
  EVApp$CloseFile(EVfile)
  
  # quit echoview
  EVApp$Quit()
  
  
  
  ###############################################
  #                   CREATE                    #
  ###############################################
  
  # list all exported lines
  linepattern <- paste("*", i,"evl", sep = ".")
  lines.list <- list.files(file.path(getwd(), EVlines), pattern=linepattern)
  
  # list all rawfiles
  rawfiles <- sub(".*\\\\", "", rawfiles)
  
  ##-------------- run in echoview ----------------- ##

  # create COM connection between R and Echoview
  EVApp <- COMCreate("EchoviewCom.EvApplication")
  
  # Open template EV file
  EVfile <- EVApp$OpenFile(file.path(getwd(), TEMPdir, "template.EV"))
  
  # Set fileset object
  filesetObj <- EVfile[["Filesets"]]$Item(0)
  
  # Set calibration file
  CalfilePath <- file.path(getwd(),"Acoustics", "Echoview")
  CALfile <- list.files(CalfilePath, pattern="*.ecs")
  add.calibration <- filesetObj$SetCalibrationFile(file.path(CalfilePath, CALfile))
  
  # Add raw files
  for (w in rawfiles){
    filesetObj[["DataFiles"]]$Add(file.path(getwd(),RAWdir,w))
  }
  
  # Import lines
  for(y in lines.list){
    LinefileName <- file.path(getwd(), EVlines, y)
    EVfile$Import(LinefileName)
  }
  
  # Import regions
  EVfile$Import(regionfilename)

  # Save EV file
  EVfile$SaveAS(file.path(getwd(), EVdir ,i))
  
  # Close EV file
  EVApp$CloseFile(EVfile)
  
  # quit echoview
  EVApp$Quit()
  
}



