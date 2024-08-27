library(shiny)
library(tidyverse)
library(dplyr)
library(haven)
library(rsconnect)
# rsconnect::deployApp('./ ')

source("./R/KMplot.R")
source("./R/Utils.R")
source("./R/LoadingData.R")

km_data <- readRDS("./data/SurvPlotData.rds")

cal_data <- readRDS("./data/CalPlotData.rds")
 
# temp_pred <- melanoma_marginal_predictions %>%
#   select(t0_10,melanoma_standard_ac_0_10,melanoma_pa5_ac_0_10,melanoma_tr5_ac_0_10) %>%
#   pivot_longer(cols=!t0_10,names_to = "Pred_Type",values_to = "S") %>%
#   rename(t = t0_10)
# 
# temp_observed <- colon_marginal_observed %>%
#   filter(Pred_Type == "km_cs")
# 
# combined_df <- rbind(temp_pred,temp_observed)
  