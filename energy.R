
init_energy_tables <- function() {
  solar_data <<- data.frame(matrix(ncol=3, nrow=0))
  wind_data <<- data.frame(matrix(ncol=3, nrow=0))
  coal_data <<- data.frame(matrix(ncol=3, nrow=0))
  gdp_data <<- data.frame(matrix(ncol=3, nrow=0))
}

date_to_year <- function(df) {
  date_year_regex <- "-\\d{2}-\\d{2}"
  
  df$Date <- df$Date %>% 
    str_replace(date_year_regex, "") %>%
    str_trim %>%
    as.numeric()
  
  return(df)
}

spread_year <- function(df) {
  df <- df %>% spread(key = Date, value = Value)
  return(df)
}

energy_by_individual <- function(df) {
  for(i in seq(1:nrow(df))) {
    st <- df[i,]$state
    yr <- as.character(df[i,]$Date)
    pop <- state_populations %>%
      select(state, contains(yr)) %>%
      filter(state == st) %>%
      pull(2)
    
    df[i,]$Value <- df[i,]$Value / pop * 293071.07017
  }
  
  return(df)
}

tidy_table <- function(df) {
  df <- df %>%
    date_to_year() %>%
    energy_by_individual() %>%
    spread_year()
  
  df$state <- as.character(df$state)
  
  return(df)
}
# Function to collect, clean and organize the data
fetch_energy_data <- function () {
  init_energy_tables()
  
  solar_template <- "EIA/SEDS_SOTXB_%s_A"
  wind_template <- "EIA/SEDS_WYTXB_%s_A"
  coal_template <- "EIA/SEDS_CLTCB_%s_A"
  gdp_template <- "EIA/SEDS_GDPRX_%s_A"
  
  start <- "2013-12-31"
  end <- "2015-12-31"
  api <- read.csv("apikey.csv", header = F)
  api <- as.character(api$V1)
  
  for (state in state.abb) {
    query <- sprintf(solar_template, state)
    solar_data <- rbind(solar_data,
                        cbind(state,
                              Quandl(query,
                                     start_date=start,
                                     end_date=end,
                                     api_key=api)))
    
    query <- sprintf(wind_template, state)
    wind_data <- rbind(wind_data,
                       cbind(state,
                             Quandl(query,
                                    start_date=start,
                                    end_date=end,
                                    api_key=api)))
    
    query <- sprintf(coal_template, state)
    coal_data <- rbind(coal_data,
                       cbind(state,
                             Quandl(query,
                                    start_date=start,
                                    end_date=end,
                                    api_key=api)))
    
    query <- sprintf(gdp_template, state)
    gdp_data <- rbind(gdp_data,
                       cbind(state,
                             Quandl(query,
                                    start_date=start,
                                    end_date=end,
                                    api_key=api)))
  }
  
  solar_data <<- tidy_table(solar_data)
  wind_data <<- tidy_table(wind_data)
  coal_data <<- tidy_table(coal_data)

  gdp_data$state <- as.character(gdp_data$state)
  gdp_data <<- gdp_data %>% date_to_year() %>% spread_year()
}

string_to_energy_table <- function(input) {
  if (str_detect(input, regex("solar", ignore_case = T))) {
    return(solar_data)
  } else if (str_detect(input, regex("wind", ignore_case = T))) {
    return(wind_data)
  } else if (str_detect(input, regex("coal", ignore_case = T))) {
    return(coal_data)
  } else {
    stop("Unable to find energy table")
  }
}

# To make the choropleth
map_by_energy_and_year <- function(source, year) {
  energy_values <- string_to_energy_table(source) %>% 
    select(state, year)
  
  energy_values$hover <- "kwh/yr"
  
  plot_geo(energy_values, locationmode = 'USA-states') %>%
    add_trace(z = ~get(year), text= ~hover, locations = ~state,
              color = ~get(year), colors = 'Blues') %>%
    colorbar(title = "kWh per year") %>%
    layout(
      title = sprintf("US %s Usage Per Individual", source),
      geo =  list(
        scope = 'usa',
        projection = list(type = 'albers usa'),
        showlakes = TRUE,
        lakecolor = toRGB('white')
      )
    )
}
