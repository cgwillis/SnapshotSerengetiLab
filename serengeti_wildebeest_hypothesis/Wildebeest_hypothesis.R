# LIBRARIES

library(shiny)
library(dplyr)
library(ggplot2)
library(lubridate)

# DATA 
# Read in data 
Serengeti <-read.csv("S1-11_wildebeest.csv")

# Create season variable
Serengeti$Season <- "Inter" 
Serengeti$Season[Serengeti$Month..1.Jan.. %in% c(6,7,8,9,10)]  <- "Dry"
Serengeti$Season[Serengeti$Month..1.Jan.. %in% c(11,12,1,2,3,4) ] <- "Wet"
Serengeti$Month..1.Jan.. <- as.factor(Serengeti$Month..1.Jan..)
# Create wet and dry season datasets
Wilde <- filter(Serengeti, Species == "wildebeest" & Season != "Inter")


# Generate lists for continuous variables
contVarList = c("Amount.of.Shade", "Distance.to.River..m.",
                "Distance.to.Confluence..m.","Distance.to.Kopje..m.",
                "Tree.Density.Measure",
                "Greeness..Dry.", "Greeness..Wet.")
contVarNames = c("Shade (0-4 scale)","Distance to river (m)",
                 "Distance to confluence (m)", "Distance to kopje (m)",
                 "Ave. distance to trees (m)",
                 "Greenness, dry season", "Greenness, wet season")


caption = "Environmental values at cameras where wildebeest were recorded by month. Boxes show the interquartile range, horizontal line is the median, diamond is the mean value. Whiskers show the values of points included within 1.5 times the intnerquartile range, values outside of this range (outliers) are represented by open circles"   


# UI ###################################
# UI ###################################
# UI ###################################

ui <- fluidPage(
    
    # TITLE
    
    titlePanel("Environment at Sites of Wildebeest Observation, by Season"),
    
    # ROW1
    fluidRow(
        
        # SIDEBAR ROW1
        column(3,
               
            p(""),
            p(""),
        
            radioButtons("contVar",
                "Select Environmental Variable:", 
                choiceValues = contVarList,
                choiceNames = contVarNames)
                      
        ), # end sidebar
        
        # MAIN PANEL ROW1
        column (8,
         
                plotOutput("wildeGraph"),
                p(""),
                p(caption),
                p(""),
                downloadButton(outputId = "downWildeGraph", label = "Download the plot"),
                p("")
        )#end main panel
    )#endrow
)#end ui



# SERVER ###################################
# SERVER ###################################
# SERVER ###################################

server <-
    function(input, output) {
        
        
        #Row 1 output
        data <- reactiveValues()
        observeEvent(input$contVar,{
            data$plot <-  ggplot(Wilde,aes_string(x="Month..1.Jan..", y=input$contVar)) +
                geom_boxplot(outlier.colour="black", outlier.shape=1, outlier.size=4) +
                theme_gray(base_size = 22) +
                ylab(contVarNames[match(input$contVar,contVarList)]) +
                stat_summary(fun=mean, geom="point", shape=9, size=8)})
            
            output$wildeGraph <- renderPlot({ data$plot })

        
        # DOWNLOADS
        
        # downloadHandler contains 2 arguments as functions, namely filename, content
        # code from: https://gist.github.com/aagarw30/6c10d6d92f5d512cae41
        
        output$downWildeGraph <- downloadHandler(
            filename =  function() {
                st=format(Sys.time(), "%Y-%m-%d_%H:%M")
                paste(st,"Boxplot", ".pdf", sep="")
            },
            # content is a function with argument file. content writes the plot to the device
            content = function(file) {
                ggsave(file,plot=data$plot)
            } 
        ) #end download handler
        
        
        
        
    } # end server -> function()


# Run the application 
shinyApp(ui = ui, server = server)
