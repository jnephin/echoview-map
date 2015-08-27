require(RDCOMClient)
source("1_Paths.R")



###-----------------  EDIT  --------------------###
###################################################
###--------------------------------------------####

# INTEGRATE 
# Make TRUE the method(s) you which to integrate by
ByCell <- FALSE
ByRegion <- FALSE
ByCellByRegion <- TRUE

# ACOUSTIC variables 
# list all variables you want to intergrate:
variables <- c("Sv raw pings T2")

###--------------------------------------------####
###################################################
###--------------------------------------------####








# ---------------DO NOT EDIT ----------------- #

#################################################
#           Load in EV file names               #
#################################################

# list the EV files
EVfile.list <- list.files(file.path(getwd(), EVdir), pattern="*.EV")



#################################################
#    Create directories for variable exports    #
#################################################

for (l in variables){
  dir.create(file.path(getwd(), EXPdir, l))
}


###############################################
#              RUN in Echoview                #
###############################################

# create COM connection between R and Echoview
EVApp <- COMCreate("EchoviewCom.EvApplication")




################################################
#                  Integration                 #
################################################


## ------------- start EV loop

for (i in EVfile.list){
  EVfileNames <- file.path(getwd(), EVdir, i)
  
  
# open EV file
EVfile <- EVApp$OpenFile(EVfileNames)

# Variables object
VarObj <- EVfile[["Variables"]]

# export properties variables object
ExpVarObj <- EVfile[["Properties"]][["Export"]][["Variables"]]

# enable export variables
exNASC <- ExpVarObj$Item("NASC"); exNASC[["Enabled"]] = 1

# export properties mode set to spreadsheet
ExpObj <- EVfile[["Properties"]][["Export"]]
ExpObj[["Mode"]] = 2



# EV filename
EvName <- strsplit(i, split = '*.EV')[[1]]


# loop through variables for integration
for (j in variables) {
        var <- VarObj$FindByName(j)$AsVariableAcoustic()
        
# export by cells
    if (ByCell == TRUE){
        exportFileName <- file.path(getwd(), EXPdir, j, paste(EvName, "_IntegratedByCells", ".csv", sep=""))
        var$ExportIntegrationByCellsAll(exportFileName)
    }
# export by regions
    if (ByRegion == TRUE){
        exportFileName <- file.path(getwd(), EXPdir, j, paste(EvName, "_IntegratedByRegions", ".csv", sep=""))
        var$ExportIntegrationByRegionsAll(exportFileName)
    }
# export by cells by region 
   if (ByCellByRegion == TRUE){
        exportFileName <- file.path(getwd(), EXPdir, j, paste(EvName, "_IntegratedByRegionsByCells", ".csv", sep=""))
        var$ExportIntegrationByRegionsByCellsAll(exportFileName)
    } 
}


#close EV file
EVApp$CloseFile(EVfile)
  

## ------------- end EV loop
}


##########################################
#quit echoview
EVApp$Quit()






################################################
#         Combine all .csv exports             #
################################################



# By cells
#########################################################################################
for (j in variables) {

  unlink(file.path(getwd(), EXPdir, j, "IntegratedByCells.csv"))
  
  exp.list <- list.files(file.path(getwd(), EXPdir, j), pattern="*IntegratedByCells.csv")

columns <- c("Process_ID", "Interval", "Layer", "Sv_mean",	"NASC",	"Layer_depth_min",   "Layer_depth_max", "VL_start",	"VL_end",	"Date_S",	 "Time_S",	 "Lat_S",	 "Lon_S",	 "Exclude_below_line_depth_mean",	 "Processing_version",	 "Processing_date",	 "Processing_time",	 "EV_filename",	 "Alpha",	 "Gain_constant",	 "Noise_Sv_1m",	 "Minimum_Sv_threshold_applied",	 "Minimum_integration_threshold",	 "Maximum_Sv_threshold_applied",	 "Maximum_integration_threshold",	 "Exclude_above_line_applied",	 "Exclude_above_line_depth_mean",	 "Exclude_below_line_applied",	 "Standard_deviation")

df <- NULL
for (k in exp.list){
  d <- read.csv(file.path(getwd(), EXPdir, j, k), header = T)
  colnames(d)[1] <- "Process_ID"
  d <- d[,columns]
  df <- rbind(df,d)
}

if (is.null(df)) {
  print("NULL")
    } else {
      write.csv(df, file = file.path(getwd(), EXPdir,  j, "IntegratedByCells.csv"))
    }
}




# By region
#########################################################################################
for (j in variables) {
  
  unlink(file.path(getwd(), EXPdir, j, "IntegratedByRegions.csv"))
  
  exp.list <- list.files(file.path(getwd(), EXPdir, j), pattern="*IntegratedByRegions.csv")
  
  columns <- c("Region_ID",   "Region_name",   "Region_class",	 "Process_ID", "Sv_mean",	 "NASC",	 "VL_start",	 "VL_end",	 "Date_S",	 "Time_S",	 "Lat_S",	 "Lon_S",	 "Exclude_below_line_depth_mean",	 "Processing_version",	 "Processing_date",	 "Processing_time",	 "EV_filename",	 "Alpha",	 "Gain_constant",	 "Noise_Sv_1m",	 "Minimum_Sv_threshold_applied",	 "Minimum_integration_threshold",	 "Maximum_Sv_threshold_applied",	 "Maximum_integration_threshold",	 "Exclude_above_line_applied",	 "Exclude_above_line_depth_mean",	 "Exclude_below_line_applied",	 "Standard_deviation")
  
  df <- NULL
  for (k in exp.list){
    d <- read.csv(file.path(getwd(), EXPdir, j, k), header = T)
    colnames(d)[1] <- "Region_ID"
    d <- d[,columns]
    df <- rbind(df,d)
  }
  
  if (is.null(df)) {
    print("NULL")
  } else {  
  write.csv(df, file = file.path(getwd(), EXPdir,  j, "IntegratedByRegions.csv"))
  }
}



# By cells by region
#########################################################################################
for (j in variables) {
  
  unlink(file.path(getwd(), EXPdir, j, "IntegratedByRegionsByCells.csv"))
  
  exp.list <- list.files(file.path(getwd(), EXPdir, j), pattern="*IntegratedByRegionsByCells.csv")
  
  columns <- c("Region_ID",   "Region_name",	 "Region_class",	 "Process_ID",	 "Interval",	 "Layer",	 "Sv_mean",	 "PRC_NASC",	 "Layer_depth_min",	 "Layer_depth_max",	 "VL_start",	 "VL_end",	 "Date_S",	 "Time_S",	 "Lat_S",	 "Lon_S",	 "Exclude_below_line_depth_mean",	 "Processing_version",	 "Processing_date",	 "Processing_time",	 "EV_filename",	 "Alpha",	 "Gain_constant",	 "Noise_Sv_1m",	 "Minimum_Sv_threshold_applied",	 "Minimum_integration_threshold",	 "Maximum_Sv_threshold_applied",	 "Maximum_integration_threshold",	 "Exclude_above_line_applied",	 "Exclude_above_line_depth_mean",	 "Exclude_below_line_applied",	 "Standard_deviation")
  
  df <- NULL
  for (k in exp.list){
    d <- read.csv(file.path(getwd(), EXPdir, j, k), header = T)
    colnames(d)[1] <- "Region_ID"
    d <- d[,columns]
    df <- rbind(df,d)
  }
  
  if (is.null(df)) {
    print("NULL")
  } else {
  write.csv(df, file = file.path(getwd(), EXPdir,  j, "IntegratedByRegionsByCells.csv"))
  }
}






