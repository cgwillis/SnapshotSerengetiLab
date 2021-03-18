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
  
  # Generate lists for continuous variables
  contVarList = c("Amount.of.Shade", "Distance.to.River..m.",
                  "Distance.to.Confluence..m.","Distance.to.Kopje..m.",
                  "Tree.Density.Measure", "Lion.Risk..Wet.",
                  "Lion.Risk..Dry.","Greeness..Wet.",
                  "Greeness..Dry." )
  contVarNames = c("Shade (0-4 scale)","Distance to river (m)",
                   "Distance to confluence (m)", "Distance to kopje (m)",
                   "Ave. distance to trees (m)", "Lion encounter rate (wet)",
                   "Lion encounter rate (dry)", "Greenness, (wet)",
                   "Greenness, (dry)"
                   )
  

  
  # Select cam specific data from and filter to 1 row per camera, discard observation data
  camData_distinct <- Serengeti %>% distinct(Camera.Site, .keep_all = TRUE)     
  camData <- select(camData_distinct, Camera.Site:Camera.Mount)

# UI ###################################
# UI ###################################
# UI ###################################

ui <- fluidPage(
   
   # TITLE
  
   titlePanel("Species Movement and Co-occurrence in Space and Time"),
   
  # ROW4
  fluidRow(
           
           # SIDEBAR ROW4
           column(4,
                  
                  selectInput("moveSpecies1",
                                     "Select Species 1:", 
                                     choices = speciesList,
                                     selected = "wildebeest"
                                     
                  ),
                  
                  selectInput("moveSpecies2",
                              "Select Species2:", 
                              choices = speciesList,
                              selected = "lionFemale"
                  ),
                  
                  dateRangeInput("Date1","Select Date Range 1:",
                    start = "2012-04-17",end = "2012-04-17",
                    min = "2010-07-18", max ="2013-03-10" 
                  ),
                  
                  dateRangeInput("Date2","Select Date Range 2:",
                            start = "2012-04-20",end = "2012-04-20",
                            min = "2010-07-18", max ="2013-03-10" 
                  ),
                  p("Species 1, Date Range 1 = Cyan"),
                  p("Species 1, Date Range 2 = Blue"),
                  p("Species 2, Date Range 1 = Orange"),
                  p("Species 2, Date Range 2 = Red"),
                  p("All circles are normalized to the total number of observations of that species, the circle size represents the proportion of observations made during the date range."),
                  p("Database errors mean one or more species did not meet the criteria given. For rarer species, try wider date ranges."),
                  p("")
                  
           ),
           
           # MAIN PANEL ROW4
           
           column(8,
                  plotOutput("moveSpecies"),
                  downloadButton(outputId = "downMoveSpecies", label = "Download the plot"),
                  p("")
           )    # end column
           
  ) # end row   
   
)    # end UI

# SERVER ###################################
# SERVER ###################################
# SERVER ###################################

server <-
  function(input, output) {

   
   #Row 4 output
   
   output$moveSpecies <- renderPlot({
     
     #make base plot to fix axis length and etc, points for cam locations
     plot(camData$Longitude..m., camData$Latitude..m., 
          cex = 0.5, pch = 16, asp =1,         
          xlab = "Longitude (m)",
          ylab = "Latitude (m)")
     
     #make plot Species 1, date range 1
     min <- input$Date1[1]
     max <- input$Date1[2]
     moveSpecies <-  input$moveSpecies1
     mycol <- rgb(0, 255, 255, max = 255, alpha = 125)
     filtered <- filter(Serengeti,
                          (parse_date_time(Serengeti$Date,'mdy') >= min
                          & parse_date_time(Serengeti$Date,'mdy') <= max)
                          & Species == moveSpecies)
     count <- table(filtered$Camera.Site)
     count_df <- as.data.frame(count)  
     Data <- left_join(count_df,camData, by = c("Var1"="Camera.Site"))
     points(Data$Longitude..m.,Data$Latitude..m., 
          cex = (sqrt((Data$Freq/sum(Data$Freq)*64))), pch = 21, bg = mycol
     )
     
     #make plot Species 1, date range 2
     min <- input$Date2[1]
     max <- input$Date2[2]
     moveSpecies <-  input$moveSpecies1
     mycol <- rgb(0, 0, 255, max = 255, alpha = 125)
     filtered <- filter(Serengeti,
                          (parse_date_time(Serengeti$Date,'mdy') >= min
                          & parse_date_time(Serengeti$Date,'mdy') <= max)
                          & Species == moveSpecies)
     count <- table(filtered$Camera.Site)
     count_df <- as.data.frame(count)  
     Data <- left_join(count_df,camData, by = c("Var1"="Camera.Site"))
     points(Data$Longitude..m.,Data$Latitude..m., 
          cex = (sqrt((Data$Freq/sum(Data$Freq)*64))), pch = 21, bg = mycol
     )
   
       #make plot Species 2, date range 1
     min <- input$Date1[1]
     max <- input$Date1[2]
     moveSpecies <-  input$moveSpecies2
     mycol <- rgb(255, 125, 0, max = 255, alpha = 125)
     filtered <- filter(Serengeti,
                        (parse_date_time(Serengeti$Date,'mdy') >= min
                         & parse_date_time(Serengeti$Date,'mdy') <= max)
                        & Species == moveSpecies)
     count <- table(filtered$Camera.Site)
     count_df <- as.data.frame(count)  
     Data <- left_join(count_df,camData, by = c("Var1"="Camera.Site"))
     points(Data$Longitude..m.,Data$Latitude..m., 
            cex = (sqrt((Data$Freq/sum(Data$Freq)*64))), pch = 21, bg = mycol
     ) 
     
     #make plot Species 2, date range 2
     min <- input$Date2[1]
     max <- input$Date2[2]
     moveSpecies <-  input$moveSpecies2
     mycol <- rgb(255, 0, 0, max = 255, alpha = 125)
     filtered <- filter(Serengeti,
                        (parse_date_time(Serengeti$Date,'mdy') >= min
                         & parse_date_time(Serengeti$Date,'mdy') <= max)
                        & Species == moveSpecies)
     count <- table(filtered$Camera.Site)
     count_df <- as.data.frame(count)  
     Data <- left_join(count_df,camData, by = c("Var1"="Camera.Site"))
     points(Data$Longitude..m.,Data$Latitude..m., 
            cex = (sqrt((Data$Freq/sum(Data$Freq)*64))), pch = 21, bg = mycol
     ) 
     
   })
   
  # DOWNLOADS

  # downloadHandler contains 2 arguments as functions, namely filename, content
  # code from: https://gist.github.com/aagarw30/6c10d6d92f5d512cae41

      
      
      output$downMoveSpecies <- downloadHandler(
        filename =  function() {
          paste("Joint Distribution Map", "pdf", sep=".")
        },
        # content is a function with argument file. content writes the plot to the device
        content = function(file) {
          pdf(file) # open the pdf device
          
          #PLOT
          
          #make base plot to fix axis length and etc, points for cam locations
          plot(camData$Longitude..m., camData$Latitude..m., 
               cex = 0.5, pch = 16,          
               xlab = "Longitude (m)",
               ylab = "Latitude (m)",
               asp = 1 )
          
          #make plot Species 1, date range 1
          min <- input$Date1[1]
          max <- input$Date1[2]
          moveSpecies <-  input$moveSpecies1
          mycol <- rgb(0, 255, 255, max = 255, alpha = 125)
          filtered <- filter(Serengeti,
                             (parse_date_time(Serengeti$Date,'mdy') >= min
                              & parse_date_time(Serengeti$Date,'mdy') <= max)
                             & Species == moveSpecies)
          count <- table(filtered$Camera.Site)
          count_df <- as.data.frame(count)  
          Data <- left_join(count_df,camData, by = c("Var1"="Camera.Site"))
          points(Data$Longitude..m.,Data$Latitude..m., asp = 1, 
                 cex = (sqrt((Data$Freq/sum(Data$Freq)*64))), pch = 21, bg = mycol
          )
          
          #make plot Species 1, date range 2
          min <- input$Date2[1]
          max <- input$Date2[2]
          moveSpecies <-  input$moveSpecies1
          mycol <- rgb(0, 0, 255, max = 255, alpha = 125)
          filtered <- filter(Serengeti,
                             (parse_date_time(Serengeti$Date,'mdy') >= min
                              & parse_date_time(Serengeti$Date,'mdy') <= max)
                             & Species == moveSpecies)
          count <- table(filtered$Camera.Site)
          count_df <- as.data.frame(count)  
          Data <- left_join(count_df,camData, by = c("Var1"="Camera.Site"))
          points(Data$Longitude..m.,Data$Latitude..m., 
                 cex = (sqrt((Data$Freq/sum(Data$Freq)*64))), pch = 21, bg = mycol
          )
          
          #make plot Species 2, date range 1
          min <- input$Date1[1]
          max <- input$Date1[2]
          moveSpecies <-  input$moveSpecies2
          mycol <- rgb(255, 125, 0, max = 255, alpha = 125)
          filtered <- filter(Serengeti,
                             (parse_date_time(Serengeti$Date,'mdy') >= min
                              & parse_date_time(Serengeti$Date,'mdy') <= max)
                             & Species == moveSpecies)
          count <- table(filtered$Camera.Site)
          count_df <- as.data.frame(count)  
          Data <- left_join(count_df,camData, by = c("Var1"="Camera.Site"))
          points(Data$Longitude..m.,Data$Latitude..m., 
                 cex = (sqrt((Data$Freq/sum(Data$Freq)*64))), pch = 21, bg = mycol
          ) 
          
          #make plot Species 2, date range 2
          min <- input$Date2[1]
          max <- input$Date2[2]
          moveSpecies <-  input$moveSpecies2
          mycol <- rgb(255, 0, 0, max = 255, alpha = 125)
          filtered <- filter(Serengeti,
                             (parse_date_time(Serengeti$Date,'mdy') >= min
                              & parse_date_time(Serengeti$Date,'mdy') <= max)
                             & Species == moveSpecies)
          count <- table(filtered$Camera.Site)
          count_df <- as.data.frame(count)  
          Data <- left_join(count_df,camData, by = c("Var1"="Camera.Site"))
          points(Data$Longitude..m.,Data$Latitude..m., 
                 cex = (sqrt((Data$Freq/sum(Data$Freq)*64))), pch = 21, bg = mycol
          ) 
          
          
          
          dev.off()  # turn the device off
        } 
      ) #end download handler
      
      
      
} # end server -> function()


# Run the application 
shinyApp(ui = ui, server = server)

