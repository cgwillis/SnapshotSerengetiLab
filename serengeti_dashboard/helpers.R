library(shiny)
library(shinythemes)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(scales)
library(dplyr)

getSerengetiData <- function(fp) {
  
  data <- read_csv(fp) 
  
  
  names(data) <- gsub(" ", "_", names(data))
  names(data) <- gsub("\\(", "", names(data))
  names(data) <- gsub("\\)", "", names(data))
  
  data$Number_Of_Animals <- as.numeric(data$Number_Of_Animals)
  
  data <- data %>%
    drop_na()
  
  data <- data %>% 
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
           Habitat = factor(Habitat),
           Log_Number_Of_Animals = log(Number_Of_Animals),
           Log_Longitude_m = log(Longitude_m),
           Log_Latitude_m = log(Latitude_m),
           Log_Amount_of_Shade = log(Amount_of_Shade),
           Log_Distance_to_River_m = log(Distance_to_River_m),
           Log_Distance_to_Confluence_m = log(Distance_to_Confluence_m),
           Log_Distance_to_Kopje_m = log(Distance_to_Kopje_m),
           Log_Tree_Density_Measure = log(Tree_Density_Measure),
           Log_Lion_Risk_Wet = log(Lion_Risk_Wet),
           Log_Lion_Risk_Dry = log(Lion_Risk_Dry),
           Log_Greeness_Wet = log(Greeness_Wet),
           Log_Greeness_Dry = log(Greeness_Dry))
  return(data)
}

filterSerengetiData <- function(
  data,
  species_input,
  standing_input,
  resting_input,
  moving_input,
  eating_input,
  interacting_input,
  babies_input,
  habitat_input,
  date_input) {
  
  data_filtered <- data %>% 
    filter(Species %in% species_input) %>%
    filter(Standing %in% standing_input) %>% 
    filter(Resting %in% resting_input) %>%
    filter(Moving %in% moving_input) %>%
    filter(Eating %in% eating_input) %>%
    filter(Interacting %in% interacting_input) %>%
    filter(Babies %in% babies_input) %>%
    filter(Habitat %in% habitat_input) %>%
    filter(Date >= date_input[1] & Date <= date_input[2])
  
  return(data_filtered)
}

summarize_data <- function(
  data,
  species_input,
  standing_input,
  resting_input,
  moving_input,
  eating_input,
  interacting_input,
  babies_input,
  habitat_input) {
  
  summary <- data %>% 
    filter(Species %in% species_input) %>%
    filter(Standing %in% standing_input) %>%
    filter(Resting %in% resting_input) %>%
    filter(Moving %in% moving_input) %>%
    filter(Eating %in% eating_input) %>%
    filter(Interacting %in% interacting_input) %>%
    filter(Babies %in% babies_input) %>%
    filter(Habitat %in% habitat_input)
  
  
  return(summary)
}


# A colorblind-friendly palette with grey:
cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#999999")

continuous_vars <- c("Amount_of_Shade",
                     "Distance_to_Confluence_m",
                     "Distance_to_Kopje_m",
                     "Distance_to_River_m",
                     "Greeness_Dry",
                     "Greeness_Wet",
                     "Latitude_m",
                     "Lion_Risk_Dry",
                     "Lion_Risk_Wet",
                     "Longitude_m",
                     "Number_Of_Animals",
                     "Tree_Density_Measure",
                     "Log_Amount_of_Shade",
                     "Log_Distance_to_Confluence_m",
                     "Log_Distance_to_Kopje_m",
                     "Log_Distance_to_River_m",
                     "Log_Greeness_Dry",
                     "Log_Greeness_Wet",
                     "Log_Latitude_m",
                     "Log_Lion_Risk_Dry",
                     "Log_Lion_Risk_Wet",
                     "Log_Longitude_m",
                     "Log_Number_Of_Animals",
                     "Log_Tree_Density_Measure")

continuous_vars_no_log <- c("Amount_of_Shade",
                            "Distance_to_Confluence_m",
                            "Distance_to_Kopje_m",
                            "Distance_to_River_m",
                            "Greeness_Dry",
                            "Greeness_Wet",
                            "Latitude_m",
                            "Lion_Risk_Dry",
                            "Lion_Risk_Wet",
                            "Longitude_m",
                            "Number_Of_Animals",
                            "Tree_Density_Measure")

categorical_vars <- c("Babies",
                      "Eating",
                      "Habitat",
                      "Interacting",
                      "Moving",
                      "Resting",
                      "Standing")

standing_list <- c("Not standing", "Standing")
resting_list <- c("Not resting", "Resting")
moving_list <- c("Not moving", "Moving")
eating_list <- c("Not eating", "Eating")
interacting_list <- c("Not interacting", "Interacting")
babies_list <- c("No babies", "Babies")
habitat_list <- c("Dense Woodland",
                  "Grassland w/Trees",
                  "Open Grassland",
                  "Open Woodland/Shrubs")
year_list <- c(2010, 2011, 2012, 2013)

species_list <- c("aardvark", "aardwolf", "baboon", "batEaredFox", "buffalo", "bushbuck",
                  "caracal", "cheetah", "civet", "dikDik", "eland", "elephant", "gazelleGrants",
                  "gazelleThomsons", "genet", "giraffe", "guineaFowl", "hare", "hartebeest", 
                  "hippopotamus", "honeyBadger", "human", "hyenaSpotted", "hyenaStriped",
                  "impala", "jackal", "koriBustard", "leopard", "lionFemale", "lionMale",
                  "mongoose", "ostrich", "otherBird", "porcupine", "reedbuck", "reptiles",
                  "rhinoceros", "rodents", "secretaryBird", "serval", "topi", "vervetMonkey",
                  "warthog", "waterbuck", "wildcat", "wildebeest", "zebra", "zorilla")

startDate <- as.Date('2010-07-16', format='%Y-%m-%d')
endDate <- as.Date('2013-04-26', format='%Y-%m-%d')

commonGrazerList <- c("buffalo","eland","elephant","gazelleGrants", "gazelleThomsons","giraffe","hartebeest","hippopotamus","impala", "reedbuck", "rhinoceros", "topi", "warthog", "wildebeest","zebra")
predList <- c("batEaredFox","jackal","hyenaSpotted","hyenaStriped","wildcat","serval","cheetah","leopard","lionMale","lionFemale")