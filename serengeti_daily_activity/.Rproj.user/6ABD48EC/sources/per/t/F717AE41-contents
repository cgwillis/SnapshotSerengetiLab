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
library(ggplot2)
library(lubridate)

# DATA 
# Read in data 
Serengeti <-read.csv("Full_Serengeti_Data.csv")

# Generate  species lists
speciesList <- as.list(Serengeti %>% distinct(Species))
speciesList<- sort(speciesList$Species)
commonGrazerList <- as.list(c("buffalo","eland","elephant","gazelleGrants", "gazelleThomsons","giraffe","hartebeest","hippopotamus","impala", "reedbuck", "rhinoceros", "topi", "warthog", "wildebeest","zebra"))
predList <- as.list(c("batEaredFox","jackal","hyenaSpotted","hyenaStriped","wildcat","serval","cheetah","leopard","lionMale","lionFemale"))

# Select cam specific data from and filter to 1 row per camera, discard observation data
camData_distinct <- Serengeti %>% distinct(Camera.Site, .keep_all = TRUE)     
camData <- select(camData_distinct, Camera.Site:Camera.Mount)

# UI ###################################
# UI ###################################
# UI ###################################

ui <- fluidPage(
    
    # TITLE
    
    titlePanel("Serengeti Animals Activity Time"),
    

    # ROW1
    fluidRow(
        
        # SIDEBAR ROW1
        column(4,
               
               checkboxGroupInput("speciesPreds",
                                  "Select species:", 
                                  choices = predList,
                                  selected  = c("wildcat","serval","cheetah","leopard","lionMale","lionFemale")
               )
        ),
        
        
        # MAIN PANEL ROW1
        
        column(8,
               plotOutput("timePredPrey"),
               downloadButton(outputId = "downTimePredPrey", label = "Download the plot"),
               p("")
        )    # end column
        
    ) # end row  
) # end ui

# SERVER ###################################
# SERVER ###################################
# SERVER ###################################

server <-
    function(input, output) {
        
        #Row 3 output
        output$timePredPrey <- renderPlot({
            
            Serengeti_filt <- droplevels(Serengeti[Serengeti$Species %in% input$speciesPreds, ])
            
            ggplot(Serengeti_filt, 
                   (aes(x=Species, y = parse_date_time(Time..24.hour.,'%I:%M:%S %p')))) + geom_violin() + ylab("Time of Day of Observation (ignore date)") +ggtitle("Species Activity by Time")
        })
        
        
# DOWNLOADS
        
# downloadHandler contains 2 arguments as functions, namely filename, content
# code from: https://gist.github.com/aagarw30/6c10d6d92f5d512cae41       
        
        
        output$downTimePredPrey <- downloadHandler(
            filename =  function() {
                paste("Species Activity Patterns", "pdf", sep=".")
            },
            # content is a function with argument file. content writes the plot to the device
            content = function(file) {
                pdf(file) # open the pdf device
                
                #PLOT
                
                Serengeti_filt <- droplevels(Serengeti[Serengeti$Species %in% input$speciesPreds, ])
                print(ggplot(Serengeti_filt, 
                             (aes(x=Species, y = parse_date_time(Time..24.hour.,'%I:%M:%S %p')))) + geom_violin() + ylab("Time of Day of Observation (ignore date)") +ggtitle("Species Activity by Time"))
                
                
                
                dev.off()  # turn the device off
            } 
        ) #end download handler 
        
               
    }# end server -> function()


# Run the application 
shinyApp(ui = ui, server = server)
