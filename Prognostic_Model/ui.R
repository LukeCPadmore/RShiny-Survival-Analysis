library(shiny)
library(bslib)
# Define UI for application that draws a histogram
fluidPage(
    theme = bs_theme(preset = "shiny"),
    # Application title
    titlePanel("Temporal Recalibration Analysis"),
    sidebarLayout(
        sidebarPanel(
          selectInput(
            inputId = "CancerType",
            label = span("Type of Cancer",
                    tooltip(
                    bsicons::bs_icon("question-circle"),
                    "A message")
            ),
            choices = c("Colon" = "colon",
                        "Melanoma" = "melanoma")
            ),
          selectInput(
            inputId = "Framework",
            label = span("Model Framework",
                         tooltip(
                           bsicons::bs_icon("question-circle"),
                           "A message")),
            choices = c("All-Cause" = "ac" ,
                        "Cause Specific" =  "cancer",
                        "Competing Risks" = "cif")
            ),
          checkboxGroupInput(inputId = "PredictorMethod",
                             label = span("Predictor Method",
                                          tooltip(
                                            bsicons::bs_icon("question-circle"),
                                            "A message")),
                             choiceNames = c("Full Cohort","Temporal Recalibration","Period Analysis"),
                             choiceValues = c("standard","tr","pa")
          ),
        # Conditional sliders for temporal recalibration and period analysis window length
        # render dynamically if checked
        uiOutput("trWindowSlider"),
        uiOutput("paWindowSlider"),
        textOutput("text")
        ),
        mainPanel(
            card(plotOutput("FlexKM")),
            card(plotOutput("CoxPHKM")),
            card(DT::DTOutput("Test"))
     )
)
)


