require(RDCOMClient)
source("1_Paths.R")



###############################################
#          Load in EV file names              #
###############################################

# list the EV files
EVfile.list <- list.files(file.path(getwd(), EVdir), pattern="*.EV")


###############################################
#              RUN in Echoview                #
###############################################

# create COM connection between R and Echoview
EVApp <- COMCreate("EchoviewCom.EvApplication")




################################################
#             Create cruise log                #
################################################


## ------------- start loop

for (i in EVfile.list){
  EVfileName <- file.path(getwd(), EVdir, i)

# open EV file
EVfile <- EVApp$OpenFile(EVfileName)


#set EvVariable object
evVarObj <- EVfile[["Variables"]]

#get the first variable in EvVariableAcoustic 
evVarObj$Item(1)$FullName()
EvVar <- evVarObj$Item(1)$AsVariableAcoustic()

#export log book
logName <- strsplit(i, split = '*.EV')[[1]]
logFileName <- file.path(getwd(), LOGdir, paste(logName , ".csv", sep=""))
EvVar$ExportRegionsLogAll(logFileName)

#close EV file
EVApp$CloseFile(EVfile)


## ------------- end loop
}


##########################################
#quit echoview
EVApp$Quit()





#### ------ If there are more EV files in another folder -> Run again with EV folder change 





################################################
#           Combine all .csv logs              #
################################################

unlink(file.path(getwd(), LOGdir, "CruiseLog.csv"))

log.list <- list.files(file.path(getwd(), LOGdir), pattern="*.csv")

df <- NULL
for (i in log.list){
  d <- read.csv(file.path(getwd(), LOGdir, i), header = T)
  if (nrow(d)>0){
    a <- d
    a$File <- i
  df <- rbind(df,a)
  }
}

write.csv(df, file = file.path(getwd(), LOGdir,  "CruiseLog.csv"))




