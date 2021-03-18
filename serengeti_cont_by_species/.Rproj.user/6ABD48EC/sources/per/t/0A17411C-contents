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
# library(lubridate)

# DATA 
# Read in data 
Serengeti <-read.csv("Full_Serengeti_Data.csv")

# Generate  species lists

#speciesList <- as.list(Serengeti %>% distinct(Species))
#speciesList<- sort(speciesList$Species)

grazerList <- c("buffalo","bushbuck","dikDik","eland","elephant","gazelleGrants", "gazelleThomsons","giraffe","hartebeest","hippopotamus","impala", "reedbuck", "rhinoceros", "topi", "warthog", "wildebeest","zebra")
predList <- c("aardwolf","batEaredFox","caracal","civet","genet","jackal","hyenaSpotted","hyenaStriped","wildcat","serval","cheetah","leopard","lionMale","lionFemale")
otherList <-c("aardvark","baboon","guineaFowl","hare","honeyBadger","human","koriBustard","mongoose","ostrich","otherBird","porcupine","reptiles","rodents","secretaryBird")

# Generate lists of categories

categoryNames <- c("Grazers","Predators","Other")
categoryList <- c("grazerList","predList","otherList")


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
# camData_distinct <- Serengeti %>% distinct(Camera.Site, .keep_all = TRUE)     
# camData <- select(camData_distinct, Camera.Site:Camera.Mount)

# UI ###################################
# UI ###################################
# UI ###################################

ui <- fluidPage(
    
    # TITLE
    
    titlePanel("Continuous Variables By Species"),
    
    # ROW1
    fluidRow(
             
             # SIDEBAR ROW1
             column(2,
                    
                    radioButtons("speciesCategory",
                                       "Select species:",
                                        choices = c("Grazers","Predators","Other")
                    )
             ),
             column(2,
                    radioButtons("contVar",
                                 "Select a Continuous Variable:", 
                                 choiceValues = contVarList,
                                 choiceNames = contVarNames
                    )
             ),
             
             # MAIN PANEL ROW1
             
             column(8,
                    plotOutput("spaceGrazers"),
                    downloadButton(outputId = "downSpaceGrazers", label = "Download the plot"),
                    p("")
             )    # end column
             
    ) # end row  
)    # end UI

# SERVER ###################################
# SERVER ###################################
# SERVER ###################################

server <-
    function(input, output) {
        
        #Row 1 output
        output$spaceGrazers <- renderPlot({
            
        # This is inelegant, passes variable from radio buttons into variable to
        ## use to filter the dataset
            if ( input$speciesCategory == "Grazers") {
                filtCategory <- grazerList
            } else if ( input$speciesCategory == "Predators") {
                filtCategory <- predList
            } else if ( input$speciesCategory == "Other") {
                filtCategory <- otherList
            }
            
         #   filtCategory <- input$speciesCategory
            Serengeti_filt <- droplevels(Serengeti[Serengeti$Species %in% filtCategory, ])
            par(mar=c(8,4,4,4))
            # note for this formula the as.formula function is needed b/c this is reactive
            ## not necessary for the same graph in non-reactive situation
            boxplot(as.formula(paste(input$contVar, '~ Species')), data = Serengeti_filt, main ="Species differences in Continuous Variables", las = 2, xlab = "", ylab = contVarNames[match(input$contVar,contVarList)])
            
        })
        
        
        # DOWNLOADS
        
        # downloadHandler contains 2 arguments as functions, namely filename, content
        # code from: https://gist.github.com/aagarw30/6c10d6d92f5d512cae41
        
 
        output$downSpaceGrazers <- downloadHandler(
            filename =  function() {
                paste("Grazer Observations Continuous ", "pdf", sep=".")
            },
            # content is a function with argument file. content writes the plot to the device
            content = function(file) {
                pdf(file) # open the pdf device
                
                #PLOT

                if ( input$speciesCategory == "Grazers") {
                    filtCategory <- grazerList
                } else if ( input$speciesCategory == "Predators") {
                    filtCategory <- predList
                } else if ( input$speciesCategory == "Other") {
                    filtCategory <- otherList
                }
                
                #   filtCategory <- input$speciesCategory
                Serengeti_filt <- droplevels(Serengeti[Serengeti$Species %in% filtCategory, ])
                par(mar=c(8,4,4,4))
                # note for this formula the as.formula function is needed b/c this is reactive
                ## not necessary for the same graph in non-reactive situation
                boxplot(as.formula(paste(input$contVar, '~ Species')), data = Serengeti_filt, main ="Species differences in Continuous Variables", las = 2, xlab = "", ylab = contVarNames[match(input$contVar,contVarList)] )
                
                
                
                
                dev.off()  # turn the device off
            } 
        ) #end download handler

    } # end server -> function()


# Run the application 
shinyApp(ui = ui, server = server)

