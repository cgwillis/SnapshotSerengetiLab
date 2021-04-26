# rsconnect::deployApp('~/Documents/GitHub/SnapshotSerengetiLab/serengeti_dashboard/')
library(rsconnect)
library(raster)
library(sf)
library(rgdal)
library(mapview)
library(lattice)
library(leafpop)
library(RColorBrewer)
library(ggplot2)
library(ggridges)
library(tidyverse)
library(plyr)
library(rgeos)

source('barGenerate.R')
source('violinGenerate.R')
source('boxplotGenerate.R')
source('scatterGenerate.R')
source('annualGenerate.R')
source('histoGenerate.R')
source('spatialGenerate.R')

server <- function(input, output) {
  
  dataInput <- reactive({
    getSerengetiData('S1-11_filtered.csv')
  })
  
  kopjesInput <- reactive({
    readOGR("Kopjes/V3_Kopjes_ARC1960.shp")
  })
  
  # Spatial Distribution Output
  observe({
    output$spatialplotRender <- renderMapview({
      spatialplotCreate(
        dataInput(),
        kopjesInput(),
        input$species_spatial,
        input$standing_spatial,
        input$resting_spatial,
        input$moving_spatial,
        input$eating_spatial,
        input$interacting_spatial,
        input$babies_spatial,
        input$habitat_spatial,
        input$date_spatial,
        input$meta_spatial)
    })
  })
  
  
  # Annual Activity Plot Output
  output$annualplotRender <- renderPlot({ annualplotCreate(
    dataInput(),
    input$species_annual,
    input$standing_annual,
    input$resting_annual,
    input$moving_annual,
    input$eating_annual,
    input$interacting_annual,
    input$babies_annual,
    input$habitat_annual,
    input$year_annual,
    input$y_annual)
  })
  
  # Bar Plot Output
  output$barplotRender <- renderPlot({ barplotCreate(
    dataInput(),
    input$species_bar,
    input$standing_bar,
    input$resting_bar,
    input$moving_bar,
    input$eating_bar,
    input$interacting_bar,
    input$babies_bar,
    input$habitat_bar,
    input$date_bar,
    input$x_bar,
    input$y_bar)
  })
  
  # Histogram Plot Output
  output$histoplotRender <- renderPlot({ histoplotCreate(
    dataInput(),
    input$species_histo,
    input$standing_histo,
    input$resting_histo,
    input$moving_histo,
    input$eating_histo,
    input$interacting_histo,
    input$babies_histo,
    input$habitat_histo,
    input$date_histo,
    input$x_histo)
  })
  
  # Violin Plot Output
  output$violinplotRender <- renderPlot({ dailyViolinPlot(
    dataInput(),
    input$species_violin,
    input$standing_violin,
    input$resting_violin,
    input$moving_violin,
    input$eating_violin,
    input$interacting_violin,
    input$babies_violin,
    input$habitat_violin,
    input$date_violin)
    })
  
  # Box Plot Output
  output$boxplotRender <- renderPlot({ boxplotCreate(
    dataInput(),
    input$species_box,
    input$standing_box,
    input$resting_box,
    input$moving_box,
    input$eating_box,
    input$interacting_box,
    input$babies_box,
    input$habitat_box,
    input$date_box,
    input$y_box)
    })
  
  # Scatter Plot Output
  output$scatterplotRender <- renderPlot({ scatterplotCreate(
    dataInput(),
    input$species_scatter,
    input$standing_scatter,
    input$resting_scatter,
    input$moving_scatter,
    input$eating_scatter,
    input$interacting_scatter,
    input$babies_scatter,
    input$habitat_scatter,
    input$date_scatter,
    input$x_scatter,
    input$y_scatter)
  })
  
  # download handlers

  output$violinDownload <- downloadHandler(
    filename = function() {
      paste("Daily Activity", "pdf", sep=".")
    },
    content = function(file) {
      pdf(file)
      print(
        dailyViolinPlot(
        dataInput(),
        input$species_violin,
        input$standing_violin,
        input$resting_violin,
        input$moving_violin,
        input$eating_violin,
        input$interacting_violin,
        input$babies_violin,
        input$habitat_violin,
        input$date_violin)
        )
      
      dev.off()
    }
  )
  
  output$scatterDownload <- downloadHandler(
    filename = function() {
      paste("Continous Variable Comparison", "pdf", sep=".")
    },
    content = function(file) {
      pdf(file)
      print(
        scatterplotCreate(
          dataInput(),
          input$species_scatter,
          input$standing_scatter,
          input$resting_scatter,
          input$moving_scatter,
          input$eating_scatter,
          input$interacting_scatter,
          input$babies_scatter,
          input$habitat_scatter,
          input$date_scatter,
          input$x_scatter,
          input$y_scatter)
      )
      
      dev.off()
    }
  )
  
  output$histoDownload <- downloadHandler(
    filename = function() {
      paste("histogram", "pdf", sep=".")
    },
    content = function(file) {
      pdf(file)
      print(
        histoplotCreate(
          dataInput(),
          input$species_histo,
          input$standing_histo,
          input$resting_histo,
          input$moving_histo,
          input$eating_histo,
          input$interacting_histo,
          input$babies_histo,
          input$habitat_histo,
          input$date_histo,
          input$x_histo)
      )
      
      dev.off()
    }
  )
  
  output$annualDownload <- downloadHandler(
    filename = function() {
      paste("Annual Activity", "pdf", sep=".")
    },
    content = function(file) {
      pdf(file)
      print(
        annualplotCreate(
          dataInput(),
          input$species_annual,
          input$standing_annual,
          input$resting_annual,
          input$moving_annual,
          input$eating_annual,
          input$interacting_annual,
          input$babies_annual,
          input$habitat_annual,
          #input$year_annual,
          input$y_annual)
      )
      
      dev.off()
    }
  )
  
  output$boxDownload <- downloadHandler(
    filename = function() {
      paste("Boxplot", "pdf", sep=".")
    },
    content = function(file) {
      pdf(file)
      print(
        boxplotCreate(
          dataInput(),
          input$species_box,
          input$standing_box,
          input$resting_box,
          input$moving_box,
          input$eating_box,
          input$interacting_box,
          input$babies_box,
          input$habitat_box,
          input$date_box,
          input$y_box)
      )
      
      dev.off()
    }
  )
  
  output$barDownload <- downloadHandler(
    filename = function() {
      paste("Bar Charts", "pdf", sep=".")
    },
    content = function(file) {
      pdf(file)
      print(
        barplotCreate(
          dataInput(),
          input$species_bar,
          input$standing_bar,
          input$resting_bar,
          input$moving_bar,
          input$eating_bar,
          input$interacting_bar,
          input$babies_bar,
          input$habitat_bar,
          input$date_bar,
          input$x_bar,
          input$y_bar)
      )
      
      dev.off()
    }
  )
  
}