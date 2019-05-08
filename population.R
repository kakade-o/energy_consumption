
init_state_population_tables <- function() {
  state_populations <- read.csv("resources/StatePopulations.csv", stringsAsFactors = FALSE)
  state_populations <- state_populations %>%
    select(NAME, POPESTIMATE2013, POPESTIMATE2014, POPESTIMATE2015) %>%
    rename(state = NAME,
           pop_2013 = POPESTIMATE2013,
           pop_2014 = POPESTIMATE2014,
           pop_2015 = POPESTIMATE2015)
  
  state_populations$pop_2013 <- as.numeric(state_populations$pop_2013)
  state_populations$pop_2014 <- as.numeric(state_populations$pop_2014)
  state_populations$pop_2015 <- as.numeric(state_populations$pop_2015)
  
  # Removing regional areas and D.C. and Puerto Rico
  state_populations <- tail(state_populations, -5)
  state_populations <- state_populations[-c(9, 52),]
  
  state_populations$state <- str_trim(state_populations$state)
  state_populations$state <- state.abb[match(state_populations$state, state.name)]
  
  
  # This puts the table into the global environment
  state_populations <<- state_populations
}


init_city_population_tables <- function() {
  city_data <- read.csv("resources/CityPopulations.csv", stringsAsFactors = FALSE)
  city_data <- city_data %>%
    select(GC_RANK.display.label.1, respop72013, respop72014, respop72015) %>%
    rename(geography = GC_RANK.display.label.1,
           pop_2013 = respop72013,
           pop_2014 = respop72014,
           pop_2015 = respop72015)
  
  city_data$pop_2013 <- as.numeric(city_data$pop_2013)
  city_data$pop_2014 <- as.numeric(city_data$pop_2014)
  city_data$pop_2015 <- as.numeric(city_data$pop_2015)
  
  # remove headers and d.c.
  city_data <- city_data[-c(1, 21),]
  
  
  city_data <- city_data %>%
    separate(geography, c("city", "state"), ",")
  
  city_data$state <- str_trim(city_data$state)
  city_data$state <- state.abb[match(city_data$state, state.name)]
  
  # This puts it into the global environment
  city_data <<- city_data
  city_populations <<- build_city_populations(city_data)
  urban_dist <<- build_urban_dist(city_data)
}

build_city_populations <- function(city_data) {
  city_data <- city_data %>%
    mutate(large_2013 = pop_2013 > 500000,
           medium_2013 = pop_2013 > 100000 & pop_2013 < 500000,
           small_2013 = pop_2013 > 50000 & pop_2013 < 100000,
           large_2014 = pop_2014 > 500000,
           medium_2014 = pop_2014 > 100000 & pop_2014 < 500000,
           small_2014 = pop_2014 > 50000 & pop_2014 < 100000,
           large_2015 = pop_2015 > 500000,
           medium_2015 = pop_2015 > 100000 & pop_2015 < 500000,
           small_2015 = pop_2015 > 50000 & pop_2015 < 100000) %>%
    group_by(state) %>%
    summarise(large_2013 = sum(large_2013),
              medium_2013 = sum(medium_2013),
              small_2013 = sum(small_2013),
              large_2014 = sum(large_2014),
              medium_2014 = sum(medium_2014),
              small_2014 = sum(small_2014),
              large_2015 = sum(large_2015),
              medium_2015 = sum(medium_2015),
              small_2015 = sum(small_2015))
  
  return(city_data)
}



build_urban_dist <- function(city_data) {
  city_data <-
    city_data %>% 
    select(-city) %>%
    group_by(state) %>%
    summarise_all(sum) %>%
    right_join(state_populations, by = "state") %>%
    mutate(urbanized_2013 = pop_2013.x / pop_2013.y,
           urbanized_2014 = pop_2014.x / pop_2014.y,
           urbanized_2015 = pop_2015.x / pop_2015.y) %>%
    select(state, urbanized_2013, urbanized_2014, urbanized_2015)
  
  city_data[is.na(city_data)] <- 0
  
  return(city_data)  
}


# # To make the choropleth
# map_population_by_year <- function(year) {
#   
#   energy_values <- state_populations %>% 
#     select(state, year)
#   View(state_populations)
#   View(energy_values)
#   #energy_values$hover <- "kwh/yr"
#   
#   plot_geo(energy_values, locationmode = 'USA-states') %>%
#     add_trace(z = ~get(year), locations = ~state,
#               color = ~get(year), colors = 'Blues') %>%
#     colorbar(title = "Population per year") %>%
#     layout(
#       title = sprintf("Population per State"),
#       geo =  list(
#         scope = 'usa',
#         projection = list(type = 'albers usa'),
#         showlakes = TRUE,
#         lakecolor = toRGB('white')
#       )
#     )
# }