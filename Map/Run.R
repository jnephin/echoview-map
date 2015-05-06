

# set working directory and source code
setwd('..');setwd('..')
source("EchoviewR/Map/Code/Interactive.R")



# Run the app
shinyApp(ui = ui, server = server, options = list(launch.browser=TRUE))








# To stop the app 
# --------------->  press ESC in the consol

