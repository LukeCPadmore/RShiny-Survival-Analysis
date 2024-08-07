library(shiny)
# Define server logic required to draw a histogram
function(input, output, session) {
  # Render Temporal Recalibration Slider if checked
  output$trWindowSlider <- renderUI({
    selected_options <- input$PredictorMethod
    if("tr" %in% selected_options){
      sliderInput("trWindowSlider",
                  "Select Temporal Recalibration Window (Years):",
                  min = 2,
                  max = 5,
                  value = 5)
    }
  })

  output$paWindowSlider <- renderUI({
    selected_options <- input$PredictorMethod
    if("pa" %in% selected_options){
      sliderInput("paWindowSlider",
                  "Select Period Analysis Window (Years):",
                  min = 2,
                  max = 5,
                  value = 5)
    } 
  })
  # Standard does not have a window size so we just store if it has been selected
  # windowSize <- reactive({
  #   temp <- getWindowSizes(input)
  #   print(temp)
  #   return(temp)
  #  })
  # output$text <- renderText({paste(names(windowSize()),windowSize(), sep = ": ", collapse = ", ")})
  output$Test <- DT::renderDT({
    combined_df %>% filter(Cancer == input$CancerType)
  })
}
