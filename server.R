library(shiny)
# Define server logic required to draw a histogram
function(input, output, session) {
  #Refilter data when plot button clicked
  filteredKMData <- eventReactive(input$UpdateFilter,{
     km_data %>% filterPlotData(
                      cancerType = input$CancerType,
                       framework = input$Framework,
                       modelTypes = input$PredictorMethod,
                       windowSizes = windowSizes(),
                       crType = input$crType)
  })
  filteredCalData <- eventReactive(input$UpdateFilter,{
    cal_data %>% filterCalPlotData(cancerType = input$CancerType,
                          framework = input$Framework,
                          modelTypes = input$PredictorMethod,
                          windowSizes = windowSizes(),
                          crType = input$crType,
                          Est_time = input$CalPlotWindow)
  })
  
  KMPlot  <- eventReactive(input$UpdateFilter,{
    req(filteredKMData())
    return(
      plotKMCurve(filteredKMData())
    )
  })
  CalPlot <- eventReactive(input$UpdateFilter,{
    req(filteredCalData())
    return(
    plotCal(filteredCalData(),input$confint)
    )
  })                       
  output$crType <- renderUI({
    selected_options <- input$Framework
    if("Competing Risks" %in% selected_options){
      checkboxGroupInput("crType",
                         "Select Competiting Risk Type:",
                         choices = c("Cancer","Other"),
                         selected = c("Cancer"))
      
    }
  })
  
  # Render Temporal Recalibration Slider if checked
  output$trWindowSlider <- renderUI({
    selected_options <- input$PredictorMethod
    if("Temporal Recalibration" %in% selected_options){
      checkboxGroupInput("trWindowSlider",
                  "Select Temporal Recalibration Window (Years):",
                  choices = c(2,3,4,5),
                  selected = c(2))
    }
  })

  output$paWindowSlider <- renderUI({
    selected_options <- input$PredictorMethod
    if("Period Analysis" %in% selected_options){
      checkboxGroupInput("paWindowSlider",
                  "Select Period Analysis Window (Years):",
                  choices = c(2,3,4,5),
                  selected = c(2))
    } 
  })

  windowSizes <-eventReactive(input$UpdateFilter,{
    temp <- c()
    if("Standard" %in% input$PredictorMethod){
      temp$Standard <- 0
    }
    if("Temporal Recalibration" %in% input$PredictorMethod){
      temp$`Temporal Recalibration` <- as.numeric(input$trWindowSlider)
    }
    if("Period Analysis" %in% input$PredictorMethod){
      temp$`Period Analysis` <- as.numeric(input$paWindowSlider)
    }
    return(
      temp
    )
  })
  output$text <- renderText({
    if(length(input$cr_Type)+length(input$PredictorMethod)+length(input$trWindowSlider) + length(input$paWindowSlider) > 1 & input$confint){
      return(
        "Warning: confidence intervals on Calibration plot may become difficult to read."
      )
    }
  })
  output$TestKM <- DT::renderDT({
    req(filteredKMData())
    filteredKMData()
  })
  output$TestCal <- DT::renderDT({
    req(filteredCalData())
    filteredCalData()
  })
  output$KM <- renderPlot({
    KMPlot()
  })

  output$CalPlot <- renderPlot({
    CalPlot()
  })
}
