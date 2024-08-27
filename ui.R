library(shiny)
library(bslib)
library(shinycssloaders)

# Define UI for application that draws a histogram
navbarPage(
  theme = bs_theme(bootswatch = "spacelab"),
  title = "Temporal Recalibration Analysis",
  tabPanel("Info",
           page_fluid(
             tags$body(
               style = "position: relative; min-height: 100vh; padding-bottom: 6rem;",
               h2("Introduction"),
               p("The purpose of this RShiny app is to showcase 
                 differing prognostic methods in surivival analysis. Prognostic models are
                 often developed to measure the likelihood of survival on long-term studies.
                 Improvments to medical care over time have seen an increased probability of 
                 survival thus leading to miscalibrated predictions. Full details of the method used in this app can be found here:"),
               tags$ul(
                 tags$li(tags$a(href="https://academic.oup.com/ije/article/49/4/1316/5815624#211061527",
                                "Temporal recalibration for improving prognostic model development and risk predictions in settings where survival is improving over time")),
                 tags$li(tags$a(href="https://onlinelibrary.wiley.com/doi/10.1002/sim.9898",
                                "Using temporal recalibration to improve the calibration of risk prediction models in competing risk settings when there are trends in survival over time"))
               ),
               h2("Data"),
               h2("Model Framework"),
               h2("Predictor Methods"),
               h3("Period Analysis"),
               h3("Temporal Recallibration"),
               h2("Callibration Plots"),
             ),
             tags$footer(
               style = "position: absolute; bottom: 0; width: 100%; height: 60px; text-align: center;",
               "This tool was created by Luke Padmore during an internship funded by the NIHR at 
                 the University of Leicester under the supervision of Dr. Sarah Booth.")
           )
  ),
  tabPanel("Tool",
           page_fluid(
             sidebarLayout(
               sidebarPanel(
                 selectInput(
                   inputId = "CancerType",
                   label = "Type of Cancer",
                   choices = c("Colon", "Melanoma")
                 ),
                 selectInput(
                   inputId = "Framework",
                   label = span("Model Framework",
                                tooltip(
                                  bsicons::bs_icon("question-circle"),
                                  "A message")),
                   choices = c("All-Cause", "Cause Specific", "Competing Risks")
                 ),
                 uiOutput("crType"),
                 checkboxGroupInput(inputId = "PredictorMethod",
                                    label = span("Predictor Method",
                                                 tooltip(
                                                   bsicons::bs_icon("question-circle"),
                                                   "A message")),
                                    choices = c("Standard", "Temporal Recalibration", "Period Analysis")),
                 # Conditional sliders for temporal recalibration and period analysis window length
                 # render dynamically if checked
                 uiOutput("trWindowSlider"),
                 uiOutput("paWindowSlider"),
                 checkboxInput(
                   inputId = "confint",
                   label = "Toggle Calibration Plot Confidence Interval",
                   value = FALSE
                 ),
                 radioButtons(
                   inputId = "CalPlotWindow",
                   label = span("Calibration Plot Follow Up Time (Years)",
                                tooltip(
                                  bsicons::bs_icon("question-circle"),
                                  "A message")),
                   choices = c(1, 5, 10)
                 ),
                 span(textOutput("text"),style = "color:red"),
                 actionButton("UpdateFilter", "Plot")
               ),
               mainPanel = mainPanel(layout_columns(
                 card(withSpinner(plotOutput("KM"),
                                  type = 5)),
                 card(withSpinner(plotOutput("CalPlot"),
                                  type = 5)),
                 #card(DT::DTOutput("TestKM")),
                 #card(DT::DTOutput("TestCal")),
                 col_widths = c(6, 6))
               )
             )
           )
  )
)