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
  
   titlePanel("Serengeti Animals in Time and Space"),
   
  # ROW1
  fluidRow(
    
  # SIDEBAR ROW1
      column(4,
         
         radioButtons("catVar",
                     "Select category to count:", choiceNames = c("Month", "Year",  "Number Animals", "Habitat", "Mount Type"),
                     choiceValues  = c("Month..1.Jan..","Year","Number.Of.Animals","Habitat","Camera.Mount")
                     )
      ),
      
  # MAIN PANEL ROW1

      column(8,
          plotOutput("catPlot"),
          downloadButton(outputId = "downCatVar", label = "Download the plot"),
          p("")
               )    # end column
  
      ), # end row
  
  # ROW2
  fluidRow(style = "border: 1px solid black;",
    
    # SIDEBAR ROW2
    column(2,
           
           checkboxGroupInput("speciesGrazers",
                        "Select species:", 
                        choices = commonGrazerList,
                        selected  = commonGrazerList
           )
           ),
    column(2,
           radioButtons("contVar",
                        "Select a Continuous Variable:", 
                        choiceValues = contVarList,
                        choiceNames = contVarNames
           )
     
           
    ),
    
    # MAIN PANEL ROW2
    
    column(8,
           plotOutput("spaceGrazers"),
           downloadButton(outputId = "downSpaceGrazers", label = "Download the plot"),
           p("")
    )    # end column
    
  ), # end row  
  
  # ROW2.5
  fluidRow(
           
           # SIDEBAR ROW2.5
           column(2,
                  
                  checkboxGroupInput("speciesContPred",
                                     "Select species:", 
                                     choices = predList,
                                     selected  = predList
                  )
           ),
           column(2,
                  radioButtons("contVarPred",
                               "Select a Continuous Variable:", 
                               choiceValues = contVarList,
                               choiceNames = contVarNames
                  )
                  
                  
           ),
           
           # MAIN PANEL ROW2.5
           
           column(8,
                  plotOutput("spacePreds"),
                  downloadButton(outputId = "downSpacePreds", label = "Download the plot"),
                  p("")
           )    # end column
           
  ), # end row  
  
  
  # ROW3
  fluidRow(
    
    # SIDEBAR ROW3
    column(4,
           
           checkboxGroupInput("speciesPreds",
                              "Select species:", 
                              choices = predList,
                              selected  = c("wildcat","serval","cheetah","leopard","lionMale","lionFemale")
           )
    ),
           
    
    # MAIN PANEL ROW3
    
    column(8,
           plotOutput("timePredPrey"),
           downloadButton(outputId = "downTimePredPrey", label = "Download the plot"),
           p("")
    )    # end column
    
  ), # end row  
  
  # ROW4
  fluidRow(style = "border: 1px solid black;",
           
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
                  h3("Joint Distributions of Species"),
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

    
    
    
# Plot counts of different categorical variables
    
  #Row 1 output
   output$catPlot <- renderPlot({
      countCat    <- table(Serengeti[input$catVar])
      barplot(countCat, main ="Count by Category")
      
   })

   #Row 2 output
   output$spaceGrazers <- renderPlot({

     Serengeti_filt <- droplevels(Serengeti[Serengeti$Species %in% input$speciesGrazers, ])
     par(mar=c(8,4,4,4))
     # note for this formula the as.formula function is needed b/c this is reactive
     ## not necessary for the same graph in non-reactive situation
     boxplot(as.formula(paste(input$contVar, '~ Species')), data = Serengeti_filt, main ="Species differences in Continuous Variables", las = 2, xlab = "")
     
   })
   
   #Row 2.5 output
   output$spacePreds <- renderPlot({
     
     Serengeti_filt <- droplevels(Serengeti[Serengeti$Species %in% input$speciesContPred, ])
     par(mar=c(8,4,4,4))
     # note for this formula the as.formula function is needed b/c this is reactive
     ## not necessary for the same graph in non-reactive situation
     boxplot(as.formula(paste(input$contVarPred, '~ Species')), data = Serengeti_filt, main ="Species differences in Continuous Variables", las = 2, xlab = "")
     
   })
   
   
   
   #Row 3 output
   output$timePredPrey <- renderPlot({
     
     Serengeti_filt <- droplevels(Serengeti[Serengeti$Species %in% input$speciesPreds, ])

     ggplot(Serengeti_filt, 
       (aes(x=Species, y = parse_date_time(Time..24.hour.,'%I:%M:%S %p')))) + geom_violin() + ylab("Time of Day of Observation (ignore date)") +ggtitle("Species Activity by Time")
   })
   
   #Row 4 output
   
   output$moveSpecies <- renderPlot({
     
     #make base plot to fix axis length and etc, points for cam locations
     plot(camData$Longitude..m., camData$Latitude..m., 
          cex = 0.5, pch = 16,          
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

      output$downCatVar <- downloadHandler(
     filename =  function() {
       paste( "Category Distribution", "pdf", sep=".")
     },
     # content is a function with argument file. content writes the plot to the device
     content = function(file) {
       pdf(file) # open the pdf device
       
       #PLOT
       countCat    <- table(Serengeti[input$catVar])
       barplot(countCat, main ="Count by Category")
       
       dev.off()  # turn the device off
     } 
         ) #end download handler
   
      output$downSpaceGrazers <- downloadHandler(
        filename =  function() {
          paste("Grazer Observations Continuous ", "pdf", sep=".")
        },
        # content is a function with argument file. content writes the plot to the device
        content = function(file) {
          pdf(file) # open the pdf device
          
          #PLOT
          Serengeti_filt <- droplevels(Serengeti[Serengeti$Species %in% input$speciesGrazers, ])
          par(mar=c(8,4,4,4))
          # note for this formula the as.formula function is needed b/c this is reactive
          ## not necessary for the same graph in non-reactive situation
          boxplot(as.formula(paste(input$contVar, '~ Species')), data = Serengeti_filt, main ="Species differences in Continuous Variables", las = 2, xlab = "")
          

          
          dev.off()  # turn the device off
        } 
      ) #end download handler

      output$downSpacePreds <- downloadHandler(
        filename =  function() {
          paste("Predator Observations Continuous ", "pdf", sep=".")
        },
        # content is a function with argument file. content writes the plot to the device
        content = function(file) {
          pdf(file) # open the pdf device
          
          #PLOT
          
          Serengeti_filt <- droplevels(Serengeti[Serengeti$Species %in% input$speciesContPred, ])
          par(mar=c(8,4,4,4))
          # note for this formula the as.formula function is needed b/c this is reactive
          ## not necessary for the same graph in non-reactive situation
          boxplot(as.formula(paste(input$contVarPred, '~ Species')), data = Serengeti_filt, main ="Species differences in Continuous Variables", las = 2, xlab = "")
          
          
          
          dev.off()  # turn the device off
        } 
      ) #end download handler 
      
      
      
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
          
          
          
          
          
          dev.off()  # turn the device off
        } 
      ) #end download handler
      
      
      
} # end server -> function()


# Run the application 
shinyApp(ui = ui, server = server)

