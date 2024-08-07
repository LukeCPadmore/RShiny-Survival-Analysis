plotKMCurve <- function(km_data){
  return(
  ggplot(data = km_data, aes(x = t, y = S, group = Pred_Type)) +
    geom_step(aes(colour = Pred_Type)) +
    labs(x = "Time (Years)", y = "Survival Probability") + 
    theme_minimal()
  )
}