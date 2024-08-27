library(tidyverse)
library(haven)

source("./R/Utils.R")
loadObservedData <- function(file_path,cancerType){
  data <- read_dta(file_path) %>%
    pivot_longer(
      cols = contains("_t_"),
      names_to = "time_column",
      values_to = "t"
    ) %>% 
    pivot_longer(
      cols = km_all:aj_other,
      names_to = "Pred_Ind",
      values_to = "S"
    ) %>% 
    mutate(base_name = str_replace(time_column, "_t_", "_")) %>%
    filter(base_name == Pred_Ind) %>%
    filter(!is.na(t)) %>%
    mutate(Pred_Ind = sub("^km_all","km_ac",Pred_Ind)) %>%
    # mutate(Pred_Ind = sub("^aj_","cif_pred_",Pred_Ind)) %>%
    select(-base_name,-time_column) %>%
    mutate(Cancer = cancerType) %>%
    unique() 
    return(
      data
    )
}

loadPredictedData <- function(file_path,cancerType){
  data <- read_dta(file_path) %>% 
    rename(t = t0_10) %>%
    pivot_longer(cols=!t,names_to = "Pred_Ind", values_to = "S") %>%
    add_row(t = 0, S = 1, Pred_Ind = "km_cs") %>%
    add_row(t = 0, S = 1, Pred_Ind = "km_all") %>%
    add_row(t = 0, S = 1, Pred_Ind = "cif") %>%
    mutate(Pred_Ind = str_replace(Pred_Ind,"_cancer_","_cs_")) %>%
    mutate(Pred_Ind = str_replace(Pred_Ind,"(?<!cr)_all_","_ac_")) %>%
    mutate(Cancer = cancerType) %>%
    unique()
}

loadSurvPlotDataCombined <- function(){
  return(
    rbind(
      loadObservedData("./data/colon_marginal_observed.dta","Colon"),
      loadObservedData("./data/melanoma_marginal_observed.dta","Melanoma"),
      loadPredictedData("./data/colon_marginal_predictions.dta","Colon"),
      loadPredictedData("./data/melanoma_marginal_predictions.dta","Melanoma")
    ) %>% mutate(Pred_Ind = sub("^km_all","km_ac",Pred_Ind)) %>%
      mutate(Framework = case_when(
        str_detect(Pred_Ind,"ac") ~ "All-Cause",
        str_detect(Pred_Ind,"cs") ~ "Cause Specific",
        str_detect(Pred_Ind,"cr|cif|aj") ~ "Competing Risks",
        TRUE ~ "Observed"
      )) %>%
      mutate(Model_Type = case_when(
        str_detect(Pred_Ind,"standard") ~ "Standard",
        str_detect(Pred_Ind,"tr") ~ "Temporal Recalibration",
        str_detect(Pred_Ind,"pa") ~ "Period Analysis",
        TRUE ~ "Observed"
      )) %>%
      #Extract window size between 2-5
      mutate(Window_Size = str_extract(Pred_Ind, "(?<=(tr|pa))[2-5]")) %>%
      mutate(CR_Type = case_when(
        str_detect(Pred_Ind,"cifcancer|aj_cancer") ~ "Cancer",
        str_detect(Pred_Ind,"cifother|aj_other") ~ "Other",
        str_detect(Pred_Ind,"cr_all") ~ "All"
      ))
  )
}

filterPlotData <- function(df,cancerType,framework,modelTypes,windowSizes,crType){
  return(
    df %>%
      # Filter by cancer and framework
      filter(Cancer == cancerType) %>%
      filter(Framework == framework) %>%
      # filter by model types selected 
      filter(!(Model_Type != "Observed") | Model_Type %in% modelTypes) %>% 
      # Use logical implication to select window Size and compeiting risks
      filter(!(Model_Type == "Period Analysis") | Window_Size %in% windowSizes$`Period Analysis`) %>%
      filter(!(Model_Type == "Temporal Recalibration") | Window_Size %in% windowSizes$`Temporal Recalibration`) %>%
      filter(!(Framework == "Competing Risks") | CR_Type %in% crType) 
  )
}


loadCalPlot <- function(filename){
  df <- read_dta(filename)
  return(
  df %>% mutate(Index = row_number()) %>%
    pivot_longer(
    cols = -Index,
    names_to = "Pred_Ind",
    values_to = "Pseudo Value" )%>%
    mutate(Cancer = case_when(
      str_detect(Pred_Ind,"^colon") ~ "Colon",
      str_detect(Pred_Ind,"^melanoma") ~ "Melanoma"
    )) %>%
    mutate(Framework = case_when(
      str_detect(Pred_Ind,"ac") ~ "All-Cause",
      str_detect(Pred_Ind,"cs|_cancer") ~ "Cause-Specific",
      str_detect(Pred_Ind,"cr|cif|aj") ~ "Competing Risks",
    )) %>%
    mutate(Model_Type = case_when(
      str_detect(Pred_Ind,"standard") ~ "Standard",
      str_detect(Pred_Ind,"pa") ~ "Period Analysis",
      str_detect(Pred_Ind,"tr") ~ "Temporal Recalibration",
      TRUE ~ "Pseudo"
    )) %>%
    mutate(Window_Size = str_extract(Pred_Ind, "(?<=(tr|pa))[2-5]")) %>%
    mutate(CR_Type = case_when(
      str_detect(Pred_Ind,"cifcancer|aj_cancer") ~ "Cancer",
      str_detect(Pred_Ind,"cifother|aj_other") ~ "Other",
      str_detect(Pred_Ind,"cr_all") ~ "All"
    )) %>%
    mutate(Est_Time = case_when(
      str_detect(Pred_Ind,"1$") ~ 1,
      str_detect(Pred_Ind,"5$") ~ 5,
      str_detect(Pred_Ind,"10$") ~ 10
    )) %>%
    mutate(Type = case_when(
      str_detect(Pred_Ind,"pseudo") ~ "Pseudo",
      TRUE ~ "Observed"
    ))
  )
}

loadCalPlotDataCombined <- function(){
  t1 <- rbind(loadCalPlot("./data/colon_calplot.dta"),loadCalPlot("./data/melanoma_calplot.dta")) 
  t2 <- t1 %>% filter(Type == "Pseudo")
  t1 <- t1 %>% filter(Type == "Observed")
  return(
    t1 %>% inner_join(t2,by=c("Index","Cancer","Framework","CR_Type","Est_Time"))%>%
      select(Index,Pred_Ind.x,Cancer,Model_Type.x,Framework,Window_Size.x,CR_Type,Est_Time,`Pseudo Value.x`,`Pseudo Value.y`)%>%
      rename(Model_Type = Model_Type.x,Predicted = `Pseudo Value.x`, Observed = `Pseudo Value.y`,Window_Size = Window_Size.x,Pred_Ind = Pred_Ind.x)
  )
}

filterCalPlotData <- function(df,cancerType,framework,modelTypes,windowSizes,crType,Est_time){
  return(
    df %>% filter(Cancer == cancerType) %>%
           filter(Framework == framework) %>%
           filter(!(Model_Type != "Observed") | Model_Type %in% modelTypes) %>%
           filter(!(Model_Type == "Period Analysis") | Window_Size %in% windowSizes$`Period Analysis`) %>%
           filter(!(Model_Type == "Temporal Recalibration") | Window_Size %in% windowSizes$`Temporal Recalibration`) %>%
           filter(!(Framework == "Competing Risks") | CR_Type %in% crType) %>%
           filter(Est_Time == Est_time)
  )
}

