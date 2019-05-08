init_emissions_tables <- function() {
  # units are in million metric tons
  state_emissions <- read.csv("resources/StateEmissions.csv") 
  colnames(state_emissions)[1] <- "state"
  
  state_emissions <- state_emissions %>%
    select(state, X2013, X2014, X2015) %>%
    rename(emit_2013 = X2013,
           emit_2014 = X2014,
           emit_2015 = X2015)
  
  state_emissions$state <- str_trim(state_emissions$state)
  state_emissions$state <- state.abb[match(state_emissions$state, state.name)]
  
  state_emissions <- state_emissions[c(1:51),][-9,]
  state_emissions <<- state_emissions
}

# To make the choropleth
map_emissions_by_year <- function(year) {
  
  state_populations <- state_populations
  colnames(state_populations) <- c('state', 'emit_2013', 'emit_2014', 'emit_2015')
  population_values <- state_populations %>% select(state, year)
  colnames(population_values) <- c('state', 'value')
  
  energy_values <- state_emissions %>%
    select(state, year)

  
  
  colnames(energy_values) <- c('state', 'value')
  energy_values$value <- energy_values$value * 1000000
  
  energy_values$value <- energy_values$value/population_values$value
  energy_values$hover <- "tons"
  
  plot_geo(energy_values, locationmode = 'USA-states') %>%
    add_trace(z = ~value, locations = ~state, text = ~hover,
              color = ~value, colors = 'Reds') %>%
    colorbar(title = "Emissions in Tons") %>%
    layout(
      title = sprintf("Emissions per State"),
      geo =  list(
        scope = 'usa',
        projection = list(type = 'albers usa'),
        showlakes = TRUE,
        lakecolor = toRGB('white')
      )
    )
}