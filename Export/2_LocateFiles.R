
### load or install COM client
have <- "RDCOMClient" %in% installed.packages()[,"Package"]
if (have == FALSE){
  install.packages(new.packages, repos = "http://cran.stat.sfu.ca/")
}
require(RDCOMClient)

### Run Paths.R
source("1_Paths.R")


###############################################
#           Load in file names                #
###############################################

# list the EV files
EVfile.list <- list.files(file.path(getwd(), EVdir), pattern="*.EV")

# get calibration file
CALfile <- list.files(file.path(getwd(), CALdir), pattern="*.ecs")
CALfileName <- file.path(getwd(), CALdir, CALfile)



###############################################
#              RUN in Echoview                #
###############################################

# create COM connection between R and Echoview
EVApp <- COMCreate("EchoviewCom.EvApplication")


## ------------- start loop

for (i in EVfile.list){
  EVfileName <- file.path(getwd(), EVdir, i)
  
  
  # open EV file
  EVfile <- EVApp$OpenFile(EVfileName)
  
  #### set data properties object and input new datafiles folder location
  evPropObj <- EVfile[["Properties"]]
  evPropObj[["DataPaths"]]$Clear()
  evPropObj[["DataPaths"]]$Add(file.path(getwd(), RAWdir))
  
  
  #set fileset object
  filesetObj <- EVfile[["Filesets"]]$FindByName('Fileset1')
  
  # set calibration file
  add.calibration <- filesetObj$SetCalibrationFile(CALfileName)
  
  # save EV file
  EVfile$Save()
  
  #close EV file
  EVApp$CloseFile(EVfile)
  
  
}


##########################################
#quit echoview
EVApp$Quit()
