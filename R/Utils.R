# Function to filter marginal observed data frames as only KM estimates are needed
filterObservedData <- function(observedData){
  return(
    observedData %>% select(km_all,km_t_all,km_cs,km_t_cs) %>%
      unique()
      
  )
}

# Extract window size from slider components
getWindowSizes<-function(input){
  windows <- list()
  # Conditionally add 'standard' if present in input$PredictorMethod
  if ("standard" %in% input$PredictorMethod) {
    windows$standard <- TRUE
  } else {
    windows$standard <- FALSE
  }
  
  # Conditionally add 'tr' if 'tr' is in PredictorMethod and trWindowSlider is not NA
  if (is.na(input$trWindowSlider)) {
    windows$tr <- input$trWindowSlider
  } else {
    windows$tr <- FALSE # Empty string if condition not met
  }
  # Conditionally add 'pa' if 'pa' is in PredictorMethod and paWindowSlider is not NA
  if (is.na(input$paWindowSlider)) {
    windows$pa <- input$paWindowSlider
  } else {
    windows$pa <- FALSE # Empty string if condition not met
  }
  # Return the results list
  print(windows)
  return(windows)
}
