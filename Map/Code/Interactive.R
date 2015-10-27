
#load needed packages if necessary
packages.needed <- c("shiny", "PBSmapping", "ggplot2", "grid", "plyr", "chron", "mapproj")
new.packages <- packages.needed[!(packages.needed %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, repos = "http://cran.stat.sfu.ca/")


###################################################################################
# load source files
source("Rscripts/Map/Code/RegionNames.R")
source("Rscripts/Map/Code/MapInput.R")

#load analysis region names
species.names <- dget("Rscripts/Map/Layers/Species")

# create Maps directory if necessary
Mapdir <- "Other data/Figures"
suppressWarnings(dir.create(file.path(getwd(), Mapdir)))

###################################################################################   
#shiny app
require(shiny)

# ui    
ui = fluidPage(

  br(),
  
  # Sidebar with controls to select the variable to plot against
  sidebarLayout(
    sidebarPanel(width = 3,
                 strong("Map Layers"),
                 fluidRow(
                   column(4,
                          checkboxInput("shiplog", label = "Transects", value = TRUE)),
                   column(3,
                          checkboxInput("trawl", label = "Trawls", value = TRUE)),
                   column(2,
                          checkboxInput("ctds", label = "CTD", value = TRUE)),
                   column(3,
                          checkboxInput("city", label = "Cities", value = TRUE)),
                   column(4,
                          checkboxInput("survey", label = "Track", value = FALSE)), 
                   column(4,
                          checkboxInput("day", label = "Dates", value = FALSE)),                             
                   column(4,
                          checkboxInput("hour", label = "Times", value = FALSE))),
                 fluidRow(
                   column(5,
                          selectInput("analysis", label = "Analysis", 
                                      choices = c("Regions", "NASC", ""),
                                        selected = "Regions")),
                   column(7,
                          selectInput("species", label = NULL,
                                      choices = species.names,  multiple = TRUE, 
                                      selected = species.names))),
                 sliderInput("ylims", label = "Latitude",
                             min = 30, max = 65, 
                             value = c(y1, y2), step=0.1),
                 sliderInput("xlims", label = "Longitude",
                             min = -145, max = -105, 
                             value = c(x1,x2), step=0.1),
                 fluidRow(
                   column(6,
                          selectInput("dataset", 
                                      label = "Bathymetry Data", 
                                      choices = c("BC", "Pacific"),
                                      selected = "BC")),
                   column(5,
                          selectInput("bathy", "& Display", 
                                      choices = c("Polygons" = "poly",
                                                  "Lines" = "cont"),
                                      selected = "cont"))),
                 textInput("isob", "Contours",
                           value = "50 100 200 500 1000 2000 3000"),
                 
                 selectInput("project", label = "Projections",
                             choices =c("Cylindrical" = "cylindrical",
                                        "Mercator" = "mercator",
                                        "Lambert Equal Area" = "azequalarea"), 
                             selected = "mercator"),
                 fluidRow(
                   column(4,
                          sliderInput("pointsize", label = "Label", 
                                      min = 0, max = 6, value = 3, step = 1, ticks = FALSE)),
                   column(4,
                          sliderInput("linesize", label = "Line", 
                                      min = 0, max = 3, value = 1, step = .5, ticks = FALSE)),
                   column(4,
                          sliderInput("maxsize", label = "Area", 
                                      min = 5, max = 20, value = 10, step = 1, ticks = FALSE))),
                 fluidRow(
                   column(12, 
                          h4("Print PDF options", align ="center")),
                   column(3,     
                          numericInput("width", label = "Width", value = 10)),
                   column(3,
                          numericInput("height", label = "Height", value = 9)),
                   column(6,
                          textInput("name",  label = "File Name", value = "Map"))),
                 hr(),
                 fluidRow(
                   column(5, align="center",
                          actionButton("button", "Create PDF")),
                   column(7, align="center",
                          submitButton("Apply")))
    ),
    
    # Show the caption and plot of the requested variable against
    # mpg
    mainPanel(
      plotOutput("Imap", height = 900, width = 1000)
    )
  )
)



###################################################################################    
# server    
server = function(input, output) {
  
  
  # Reactive plot
  reactmap <- reactive({
    basePlot(dataset = input$dataset, city = input$city, survey = input$survey, day = input$day, hour = input$hour, shiplog = input$shiplog, analysis = input$analysis, species = input$species, ctds = input$ctds, trawl = input$trawl, project = input$project, xlims = input$xlims, ylims = input$ylims, isob = input$isob, bathy = input$bathy,  pointsize = input$pointsize, linesize = input$linesize, maxsize = input$maxsize)
  })
  
  # Generate the reactive plot 
  output$Imap <- renderPlot({
    reactmap()
  })
  
  # Print plot when button is pressed        
  observeEvent(input$button, {
    
    #print map to file
    pdf(paste(file.path(Mapdir, input$name), ".pdf", sep = ""), 
        width=input$width, height=input$height)            
    print(basePlot(dataset = input$dataset, city = input$city, survey = input$survey, day = input$day, hour = input$hour, shiplog = input$shiplog, analysis = input$analysis, species = input$species, ctds = input$ctds, trawl = input$trawl, project = input$project, xlims = input$xlims, ylims = input$ylims, isob = input$isob, bathy = input$bathy,  pointsize = input$pointsize, linesize = input$linesize , maxsize = input$maxsize))
    dev.off()
    
  })
}

###################################################################################    

