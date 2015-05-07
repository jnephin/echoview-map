

# set working directory 
setwd('..');setwd('..')

# load code 
source("EchoviewR/Map/Code/Interactive.R")



# Run the app
shinyApp(ui = ui, server = server, options = list(launch.browser=TRUE))








# To stop the app 
# --------------->  press ESC in the consol

