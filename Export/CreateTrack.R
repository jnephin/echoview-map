require(RDCOMClient)
source("1_Paths.R")




###############################################
#           Load in file names                #
###############################################
# list .raw files
RAWfile.list <- list.files(file.path(getwd(), RAWdir), pattern="*.raw$")
RAWfileNames <- file.path(getwd(), RAWdir, RAWfile.list)



###############################################
#              RUN in Echoview                #
###############################################

# create COM connection between R and Echoview
EVApp <- COMCreate("EchoviewCom.EvApplication")

#create new EV file
EVfile <- EVApp$NewFile("temp.EV")

#set fileset object
filesetObj <- EVfile[["Filesets"]]$Item(0)

#add data
for (i in RAWfileNames){
filesetObj[["DataFiles"]]$Add(i)
}





####----------------------------------------------------------------------------------------####
####----------------------------------------------------------------------------------------####
####          Wait for all .raw files to be loaded in Echoview before continuing            ####
####----------------------------------------------------------------------------------------####
####----------------------------------------------------------------------------------------####





#set EvVariable object
evVarObj <- EVfile[["Variables"]]

#get the gps fixes variable
gpsfix <- evVarObj$FindByName("position GPS fixes")

#export cruise track
gpsfix$ExportData(file.path(getwd(), GPSdir, "CruiseTrack.csv"))

#close EV file
EVApp$CloseFile(EVfile)


##########################################
#quit echoview
EVApp$Quit()

