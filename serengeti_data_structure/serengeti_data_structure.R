#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# LIBRARIES

library(shiny)
library(dplyr)

tweaks <- 
  list(tags$head(tags$style(HTML("
                                 .multicol { 
                                   -webkit-column-count: 5; /* Chrome, Safari, Opera */ 
                                   -moz-column-count: 5;    /* Firefox */ 
                                   column-count: 5; 
                                   -moz-column-fill: auto;
                                   -column-fill: auto;
                                 } 
                                 ")) 
  ))

# DATA 
  # Read in data 
  Serengeti <-read.csv("S1-11_filtered.csv")

  # Generate master species list
  speciesList <- as.list(Serengeti %>% distinct(Species))

  # Select cam specific data from and filter to 1 row per camera, discard observation data
  camData_distinct <- Serengeti %>% distinct(Camera.Site, .keep_all = TRUE)     
  camData <- select(camData_distinct, Camera.Site:Camera.Mount)

# UI ###################################
# UI ###################################
# UI ###################################

ui <- fluidPage(tweaks,
   
   # TITLE
  
   titlePanel("Overall Data Structure"),
   
  # LAYOUT


  # ROW 1

      fluidRow(
        column(1),
        column(10,
        tabsetPanel(
  
          
          #TAB - Histogram of all species
          tabPanel( "Obs. by Species",
                plotOutput("distPlotS"),
                downloadButton(outputId = "downS", label = "Download the plot")),
                
          #TAB - Histogram of all species
          tabPanel( "Obs. by Camera",
                    plotOutput("distPlotC"),
                    downloadButton(outputId = "downC", label = "Download the plot")),     
                
          #TAB - Heatmap of species on grid
          tabPanel( "Obs. by Location",
                    plotOutput("heatPlot"),
                    downloadButton(outputId = "downH", label = "Download the plot"))           

                  ) # end tabset
        ) # end column
        ),    # end row
  # ROW 2
  fluidRow(
  column(1),
  column(11,
    p(""),
    tags$div(align = "left", 
             class = "multicol",
             checkboxGroupInput( "SpeciesInput",
                                 "Species to include:", 
                                 choices  = sort(speciesList$Species),
                                 selected = sort(speciesList$Species)
             ))
    ) #end column 
  )  # end row

   )    # end UI

# SERVER ###################################
# SERVER ###################################
# SERVER ###################################

server <-
  function(input, output) {

# filter data by species selected, generate counts per species and per camera  
    
Serengeti_filt <- reactive({subset(Serengeti, Serengeti$Species %in% input$SpeciesInput)})

  
#Join observation totals to camData as Freq

        
   output$distPlotS <- renderPlot({
      countS    <- table(Serengeti_filt()$Species)
      countS2 <- countS[countS != 0]
      par(mar=c(8,4,4,4))
      barplot(countS2, main ="Captures by Species", las = 2, cex.names=0.75)
      
   })
   
   output$distPlotC <- renderPlot({
     countC    <- table(Serengeti_filt()$Camera.Site)
     hist(countC, breaks = 30, main ="Captures by Camera", xlab = "Number of Images Captured", ylab = "Number of Camera Sites")
     
   })
   
   output$heatPlot <- renderPlot({
     countC    <- table(Serengeti_filt()$Camera.Site)
     countC_df <- as.data.frame(countC)  
     camData_counts <- left_join(countC_df,camData, by = c("Var1"="Camera.Site"))
     plot(camData_counts$Longitude..m.,camData_counts$Latitude..m., 
          asp = 1,
          cex = (camData_counts$Freq/5000),
          xlab = "Longitude (m)",
          ylab = "Latitude (m)"
          )
     
   })
   
   
  # DOWNLOADS

  # downloadHandler contains 2 arguments as functions, namely filename, content
  # code from: https://gist.github.com/aagarw30/6c10d6d92f5d512cae41

      output$downS <- downloadHandler(
     filename =  function() {
       paste("Obs_by_Species", "pdf", sep=".")
     },
     # content is a function with argument file. content writes the plot to the device
     content = function(file) {
       pdf(file) # open the pdf device
       
       #PLOT
       countS    <- table(Serengeti_filt()$Species)
       countS2 <- countS[countS != 0]
       par(mar=c(8,4,4,4))
       barplot(countS2, main ="Captures by Species", las = 2, cex.names=0.75)
       
       dev.off()  # turn the device off
     } 
   )
   
   output$downC <- downloadHandler(
     filename =  function() {
       paste("Obs_by_Cam", "pdf", sep=".")
     },
     # content is a function with argument file. content writes the plot to the device
     content = function(file) {
       pdf(file) # open the pdf device
       
       #PLOT
       countC    <- table(Serengeti_filt()$Camera.Site)
       hist(countC, breaks = 30, main ="Captures by Camera", xlab = "Number of Images Captured", ylab = "Number of Camera Sites")
       
       dev.off()  # turn the device off
     } 
   )
   


output$downH <- downloadHandler(
  filename =  function() {
    paste("Obs_by_loc", "pdf", sep=".")
  },
  # content is a function with argument file. content writes the plot to the device
  content = function(file) {
    pdf(file) # open the pdf device
    
    #PLOT
    countC    <- table(Serengeti_filt()$Camera.Site)
    countC_df <- as.data.frame(countC)  
    camData_counts <- left_join(countC_df,camData, by = c("Var1"="Camera.Site"))
    plot(camData_counts$Longitude..m.,camData_counts$Latitude..m., 
         cex = (camData_counts$Freq/5000),
         xlab = "Longitude (m)",
         ylab = "Latitude (m)"
    )
    
    dev.off()  # turn the device off
  } 
)

}


# Run the application 
shinyApp(ui = ui, server = server)

