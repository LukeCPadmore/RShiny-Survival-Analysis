plotKMCurve <- function(km_data){
  km <- km_data %>% filter(Model_Type == "Observed")
  pred <- km_data %>% filter(Model_Type != "Observed")
  return(
  ggplot(data = km_data, aes(x = t, y = S, colour = Model_Type, group = Model_Type)) +
    geom_step(data = km %>% filter(!is.na(CR_Type)), 
              aes(x = t, 
                  y = S,
                  colour = interaction(Model_Type,CR_Type, sep = " - "),
                  group = interaction(Model_Type,CR_Type, sep = " - "))) +
    geom_step(data = km %>% filter(is.na(CR_Type)), 
              aes(x = t, 
                  y = S, 
                  colour = Model_Type)) +     
    geom_line(data = pred %>% filter(Model_Type == "Standard" & is.na(CR_Type)), 
                                            aes(x = t,
                                            y = S, 
                                            colour = Model_Type,
    )) +
    geom_line(data = pred %>% filter(!is.na(CR_Type) & Model_Type == "Standard"), 
              aes(x = t,
                  y = S,
                  group = interaction(Model_Type,CR_Type, sep = " - "),
                  colour = interaction(Model_Type,CR_Type, sep = " - "),
              )) +
    geom_line(data = pred %>% filter(!is.na(CR_Type) & Model_Type != "Standard"), 
              aes(x = t,
                  y = S,
                  group = interaction(Model_Type,CR_Type,Window_Size, sep = " - "),
                  colour = interaction(Model_Type,CR_Type,sep = " - "),
                  linetype = Window_Size
              )) +
    geom_line(data = pred %>% filter(Model_Type != "Standard" & is.na(CR_Type)), aes(x = t,
                               y = S, 
                               group = interaction(Model_Type, Window_Size,sep = " - "),
                               colour = Model_Type,
                               linetype = as.factor(Window_Size)
                               )) +
    labs(x = "Time (Years)", y = ifelse("Competing Risks" %in% km$Framework,"Cumulative Incidence Function [CIF(t)]","Survival Probability [S(t)]"),
         colour = "Prediction Method", linetype = "Window Size") +
    theme_minimal() +
    theme(legend.position = "bottom",
          legend.box = "vertical"))
}

plotCal <- function(cal_data,confint){
  cal_data <- cal_data %>% mutate(Window_Size = as.factor(Window_Size))
  return(
  ggplot(data = cal_data, aes(x = Predicted, y = Observed,colour = Model_Type,group = Model_Type)) +
  geom_smooth(data = cal_data %>% filter(Model_Type == "Standard" & is.na(CR_Type)),
              method = "auto",
              se = confint,
              fullrange=  TRUE,
              aes(x = Predicted,
                  y = Observed,
                  color = Model_Type)) +
  geom_smooth(data = cal_data %>% filter(Model_Type == "Standard" & !is.na(CR_Type)),
              method = "auto",
              se = confint,
              fullrange=TRUE,
              aes(x = Predicted,
                  y = Observed,
                  group = interaction(Model_Type,CR_Type, sep = " - "),
                  color = interaction(Model_Type,CR_Type, sep = " - "),
              )) +
  geom_smooth(data = cal_data %>% filter(Model_Type != "Standard" & is.na(CR_Type)),
              method = "auto",
              se = confint, 
              fullrange=TRUE,
              aes(x = Predicted,
                  y = Observed,
                  colour = Model_Type,
                  group = interaction(Model_Type,Window_Size, sep = " - "),
                  linetype = Window_Size)) +
    geom_smooth(data = cal_data %>% filter(Model_Type != "Standard" & !is.na(CR_Type)),
                method = "auto",
                se = confint, 
                fullrange = TRUE,
                aes(x = Predicted,
                    y = Observed,
                    colour = interaction(Model_Type,CR_Type, sep = " - "),
                    group = interaction(Model_Type,Window_Size,CR_Type, sep = " - "),
                    linetype = Window_Size)) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +
    coord_equal() +
    labs(colour = "Prediction Method", linetype = "Window Size") +
    theme_minimal() + 
    theme(legend.position = "bottom",
          legend.box = "vertical")) 
}
