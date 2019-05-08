normalization <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

z_score_standardization <- function(x) {
  return ((x - mean(x)) / sd(x))
}

build_data <- function(energy_data) {
  gdp_temp <- gdp_data
  emit_temp <- state_emissions
  pop_temp <- state_populations
  urban_temp <- urban_dist
  
  # large cities remained constant through all years in our dataset, so any is fine
  city_temp <- city_populations %>% select(state, contains("2015"))
  
  colnames(gdp_temp) <- c('state', 2013, 2014, 2015)
  colnames(emit_temp) <- c('state', 2013, 2014, 2015)
  colnames(pop_temp) <- c('state', 2013, 2014, 2015)
  colnames(urban_temp) <- c('state', 2013, 2014, 2015)
  
  df <- gather(energy_data, key = "year", value = "consumption", `2013`, `2014`, `2015`)
  gdp_temp <- gather(gdp_temp, key = "year", value = "gdp", `2013`, `2014`, `2015`)
  emit_temp <- gather(emit_temp, key="year", value="emissions", `2013`, `2014`, `2015`)
  pop_temp <- gather(pop_temp, key="year", value="population", `2013`, `2014`, `2015`)
  urban_temp <- gather(urban_temp, key="year", value="urban", `2013`, `2014`, `2015`)
  
  df <- inner_join(x = emit_temp,y = df, by = c('state', 'year'))
  df <- inner_join(x = gdp_temp,y = df, by = c('state', 'year'))
  df <- inner_join(x = pop_temp,y = df, by = c('state', 'year'))
  df <- inner_join(x = urban_temp,y = df, by = c('state', 'year'))
  df <- inner_join(x = city_temp, y = df, by = c('state'))
  
  df$consumption <- normalization(df$consumption)
  df$emissions <- normalization(df$emissions)
  df$population <- z_score_standardization(df$population)
  df$gdp <- normalization(df$gdp)
  df$urban <- normalization(df$urban)
  
  return(df)
}

split_data <- function(df) {
  split_data <- sample(0:1, size = nrow(df), replace = T, prob = c(0.7, 0.3))
  training <- df[split_data == 0,]
  testing <- df[split_data == 1,]
  
  return(list(training, testing))
}

build_model <- function(training) {
  model <- glm(formula = emissions ~ population + gdp + consumption, data = training)
  return(model)
}

build_prediction <- function(model, testing) {
  prediction <- predict(model, testing, type = "response")
}
 
analyze_prediction <- function(prediction, testing) {
  
  inaccurate <- 0
  for (i in seq(1, nrow(testing))) {
    emit <- testing$emissions[i]
    pred <- prediction[i]
    
    diff <- abs(emit - pred)
    
    if (diff > (emit * .25)) {
      inaccurate <- inaccurate + 1
    }
  }
  
  accuracy <- (nrow(testing) - inaccurate) / nrow(testing)
  return(accuracy)
}

run_prediction <- function () {
  datasets <<- split_data(build_data(coal_data))
  model <<- build_model(datasets[[1]])
  prediction <<- build_prediction(model, datasets[[2]])
  accuracy <<- analyze_prediction(prediction, datasets[[2]])
}

# Use to visualize correlation between columns
visualizeCorrelation <- function(input_var){
  data <- build_data(coal_data)
  h <- paste("Correlation between Emissions and", input_var)
  plot_ly(data, x = ~data[[input_var]], y = ~emissions) %>%
    layout(title=h,
          xaxis=list(title=input_var), 
           yaxis=list(title="emissions"))
}


visualizePrediction <- function() {
  temp <- data.frame(datasets[[2]], prediction)
  h <- "Prediced Emissions"
  plot_ly(temp, x = ~state, y = ~prediction) %>%
    layout(title=h,
           xaxis=list(title="state"), 
           yaxis=list(title="emissions"))
}

visualizeEmissions <- function() {
  temp <- data.frame(datasets[[2]], prediction)
  h <- "Actual Emissions"
  plot_ly(temp, x = ~state, y = ~emissions) %>%
    layout(title=h,
           xaxis=list(title="state"), 
           yaxis=list(title="emissions"))
}
