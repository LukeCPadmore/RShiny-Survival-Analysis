library(tidyverse)
library(haven)

loadObservedData <- function(file_path,cancerType){
  data <- read_dta(file_path) %>% 
    select(km_all,km_t_all,km_cs,km_t_cs) %>% 
    select(-km_t_cs) %>%
    rename(t = km_t_all) %>%
    pivot_longer(cols=!t,names_to = "Pred_Type", values_to = "S") %>%
    add_row(t = 0, S = 1, Pred_Type = "km_cs") %>%
    add_row(t = 0, S = 1, Pred_Type = "km_all") %>%
    mutate(Cancer = cancerType) %>%
    unique()
}

loadPredictedData <- function(file_path,cancerType){
  data <- read_dta(file_path) %>% 
    rename(t = t0_10) %>%
    pivot_longer(cols=!t,names_to = "Pred_Type", values_to = "S") %>%
    add_row(t = 0, S = 1, Pred_Type = "km_cs") %>%
    add_row(t = 0, S = 1, Pred_Type = "km_all") %>%
    mutate(Cancer = cancerType) %>%
    unique()
}

loadCombinedDF <- function(){
  return(
    rbind(
      loadObservedData("./RShiny-Survival-Analysis/data/colon_marginal_observed.dta","colon"),
      loadObservedData("./RShiny-Survival-Analysis/data/melanoma_marginal_observed.dta","melanoma"),
      loadPredictedData("./RShiny-Survival-Analysis/data/colon_marginal_predictions.dta","colon"),
      loadPredictedData("./RShiny-Survival-Analysis/data/melanoma_marginal_predictions.dta","melanoma")
    )
  )
}

filterCancer <- function(data,cancerType){
  return(
    data %>% filter(cancer == cancerType)
  )
}