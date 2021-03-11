library(shiny)
library(shinythemes)
library(tidyverse)
library(lubridate)
library(scales)


# A colorblind-friendly palette with grey:
cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#999999")



ss_data <- read_csv("/Volumes/Groups/PROJ-CURE Assessment/BIOL 1001/Snapshot Serengeti Shiny App/Snapshot Serengeti Scatter Plot/Full Serengeti Data.csv") 


names(ss_data) <- gsub(" ", "_", names(ss_data))
names(ss_data) <- gsub("\\(", "", names(ss_data))
names(ss_data) <- gsub("\\)", "", names(ss_data))


ss_data <- ss_data %>% 
  mutate(Date = mdy(Date),
         Species = factor(Species),
         Standing = factor(Standing,
                           levels = c(0, 1),
                           labels = c("Not standing", "Standing")),
         Resting = factor(Resting,
                          levels = c(0, 1),
                          labels = c("Not resting", "Resting")),
         Moving = factor(Moving,
                         levels = c(0, 1),
                         labels = c("Not moving", "Moving")),
         Eating = factor(Eating,
                         levels = c(0, 1),
                         labels = c("Not eating", "Eating")),
         Interacting = factor(Interacting,
                              levels = c(0, 1),
                              labels = c("Not interacting", "Interacting")),
         Babies = factor(Babies,
                         levels = c(0, 1),
                         labels = c("No babies", "Babies")),
         Habitat = factor(Habitat))




categorical_vars <- c("Babies",
                      "Eating",
                      "Habitat",
                      "Interacting",
                      "Moving",
                      "Resting",
                      "Standing")




ui <- navbarPage(theme = shinytheme("cosmo"), title = "Snapshot Serengeti Bar Plot",
                 tabPanel("Bar Plot",
                          sidebarLayout(
                            sidebarPanel(
                              selectInput(inputId = "x_input", 
                                          label = "x-axis", 
                                          choices = categorical_vars,
                                          selected = "Babies"
                              ),
                              selectInput(inputId = "y_input", 
                                          label = "y-axis", 
                                          choices = c("Count", "Frequency"),
                                          selected = "Count"
                              ),
                              checkboxGroupInput(inputId = "species_input", 
                                                 label = "Species (select up to five)", 
                                                 choices = levels(ss_data$Species),
                                                 selected = c("aardvark")
                              ),
                              checkboxGroupInput(inputId = "standing_input",
                                                 label = "Standing",
                                                 choices = levels(ss_data$Standing),
                                                 selected = c("Not standing", "Standing")
                              ),
                              checkboxGroupInput(inputId = "resting_input",
                                                 label = "Resting",
                                                 choices = levels(ss_data$Resting),
                                                 selected = c("Not resting", "Resting")
                              ),
                              checkboxGroupInput(inputId = "moving_input",
                                                 label = "Moving",
                                                 choices = levels(ss_data$Moving),
                                                 selected = c("Not moving", "Moving")
                              ),
                              checkboxGroupInput(inputId = "eating_input",
                                                 label = "Eating",
                                                 choices = levels(ss_data$Eating),
                                                 selected = c("Not eating", "Eating")
                              ),
                              checkboxGroupInput(inputId = "interacting_input",
                                                 label = "Interacting",
                                                 choices = levels(ss_data$Interacting),
                                                 selected = c("Not interacting", "Interacting")
                              ),
                              checkboxGroupInput(inputId = "babies_input",
                                                 label = "Babies",
                                                 choices = levels(ss_data$Babies),
                                                 selected = c("No babies", "Babies")
                              ),
                              checkboxGroupInput(inputId ="habitat_input",
                                                   label = "Habitat (select any or all)",
                                                   choices = levels(ss_data$Habitat),
                                                   selected = c("Dense Woodland",
                                                                "Grassland w/Trees",
                                                                "Open Grassland",
                                                                "Open Woodland/Shrubs")
                              ),
                              dateRangeInput(inputId ="date_input",
                                             label = "Select date range",
                                             start  = min(ss_data$Date),
                                             end    = max(ss_data$Date),
                                             min    = min(ss_data$Date),
                                             max    = max(ss_data$Date),
                                             format = "mm/dd/yy",
                                             separator = " - "
                              ),
                            ),
                            mainPanel(
                              plotOutput("bar_plot", height = 600)
                            )
                          ))
)




server <- function(input, output) {
  
  summarised_data <- reactive({
    
     ss_data %>% 
        filter(Species %in% input$species_input) %>%
        filter(Standing %in% input$standing_input) %>%
        filter(Resting %in% input$resting_input) %>%
        filter(Moving %in% input$moving_input) %>%
        filter(Eating %in% input$eating_input) %>%
        filter(Interacting %in% input$interacting_input) %>%
        filter(Babies %in% input$babies_input) %>%
        filter(Habitat %in% input$habitat_input) %>%
        filter(Date >= input$date_input[1] & Date <= input$date_input[2]) %>%
        group_by(Species, !!(as.symbol(input$x_input))) %>%
        count(name = "Count") %>%
        ungroup() %>%
        group_by(Species) %>%
        complete(!!(as.symbol(input$x_input)), fill = list(Count = 0)) %>%
        mutate(Frequency = round(Count/sum(Count), 3))
    
  })
  

    
  output$bar_plot <- renderPlot({
    summarised_data() %>% 
      ggplot(aes_string(x = input$x_input, y = input$y_input)) +
      geom_col(aes(fill = Species), position = position_dodge()) +
      geom_text(aes(group = Species, label = paste0("N = ", Count, "\nFreq = ", round(Frequency, 2))),
                vjust = -0.25, size = 4, fontface = 'bold', position = position_dodge(width = 0.9)) +
      scale_fill_manual(values = cbPalette) +
      theme_minimal() +
      scale_y_continuous(expand = expansion(mult = c(0, .1))) +
      scale_x_discrete(labels = label_wrap(10)) +
      theme(axis.title = element_text(face = "bold", size = 24),
            axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
            axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
            axis.text.x = element_text(size = 20),
            axis.text.y = element_text(size = 20),
            legend.text = element_text(size = 20),
            legend.title = element_text(face = "bold", size = 24),
            plot.margin = unit(c(1,1,1,1), "cm"))
  })
}




shinyApp(ui = ui, server = server)