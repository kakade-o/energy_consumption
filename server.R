library(Quandl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(tidyverse)
library(plotly)
library(stringr)

# Connecting all subfiles to this file
source("population.R")
source("energy.R")
source("emissions.R")
source("prediction.R")

# This is the top level function to initalize our data tables
setup_data_tables <- function() {
  init_state_population_tables()
  init_city_population_tables()
  init_emissions_tables()
  
  if(!exists("solar_data")) {
    fetch_energy_data()
  } 
}

server <- function(input, output) {
  setup_data_tables()
  run_prediction()
  
  output$choropleth <- renderPlotly(
    map_by_energy_and_year(input$energy, input$year))
  
  output$emit <- renderPlotly(
    map_emissions_by_year(input$emit_year)
  )
  
  output$corr <- renderPlotly(
    visualizeCorrelation(input$var_corr)
  )
  
  output$pred <- renderPlotly(
    visualizePrediction()
  )
  
  output$em <- renderPlotly(
    visualizeEmissions()
  )
}