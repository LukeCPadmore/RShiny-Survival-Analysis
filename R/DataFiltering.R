library(tidyverse)
library(dplyr)

# NOTE: Always apply in following order filterWindow => filterPredictionType => filterWindowWidth

# Filters DF by Survival Analysis Framework Type
filterFramework <- function(data,framework){
  filteredData <- data %>%
    select(t0_10,contains(framework))
}

# Filters DF by model type (Full,Temporal Recalibration and or Period Analysis) and window size
filterPredictionType <- function(data,windowSize){
  # Column names into include in regExp
  colstring <- list("t0_10")
  if(windowSize$standard){
    colstring <- c(colstring,"standard")
  }
  if(isTruthy(windowSize$tr)){
    colstring <- c(colstring,paste0(c("tr",windowSize$tr)))
  }
  if(isTruthy(windowSize$pa)){
    colstring <- c(colstring,paste0(c("pa",windowSize$pa)))
  }
  # Concat column names in RegExp
  colRegExp <- paste(colstring,collapse="|")
  return(
    data %>% select(t0_10,contains(predictionType))
  )
}


