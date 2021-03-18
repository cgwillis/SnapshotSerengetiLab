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

# DATA 
  # Read in minimized data set 
  Serengeti <-read.csv("Serengeti_Data_200_lines.csv")

# UI ###################################
# UI ###################################
# UI ###################################

ui <- fluidPage(
   
   # TITLE
  
   titlePanel("First 200 lines of the Snapshot Serengeti Dataset"),
   
  # LAYOUT
      
  # MAIN PANEL

      mainPanel(
        
              tableOutput("dataTable")
               
              )    # end main panel
   )    # end UI

# SERVER ###################################
# SERVER ###################################
# SERVER ###################################

server <-
  function(input, output) {
  
   output$dataTable <- renderTable({ 

     Serengeti
     
      })  # end output
}         # end server

# Run the application 
shinyApp(ui = ui, server = server)

