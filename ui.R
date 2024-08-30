library(shiny)
library(bslib)
library(shinycssloaders)

# Define UI for application that draws a histogram
navbarPage(
  theme = bs_theme(bootswatch = "yeti"),
  title = "Temporal Recalibration Analysis",
  tabPanel("Info",
           page_fluid(
             tags$body(
               style = "position: relative; min-height: 100vh; padding-bottom: 6rem;",
               h2("Introduction"),
               p("The purpose of this RShiny app is to showcase 
                 differing prognostic methods in survival analysis. Prognostic models are
                 often developed on long-term studies to measure the likelihood of survival.
                 Improvments to medical care over time have seen an increased probability of 
                 survival, thus leading to miscalibrated predictions. Full details 
                 of the methods used in this app can be found here:"),
               tags$ul(
                 tags$li(tags$a(href="https://academic.oup.com/ije/article/49/4/1316/5815624#211061527",
                                "Temporal recalibration for improving prognostic 
                                model development and risk predictions in settings where survival is improving over time")),
                 tags$li(tags$a(href="https://onlinelibrary.wiley.com/doi/10.1002/sim.9898",
                                "Using temporal recalibration to improve the calibration 
                                of risk prediction models in competing risk settings when there are trends in survival over time"))
               ),
               h2("Data"),
               p("The data used to create this tool is from a cancer registry from a northern
                 european country and considers Melanoma and Colon cancer. The 
                 data spans patients observed between 1975-1994.
                 Data from which patients are in the risk set between 1975-84 
                 was used to fit each model patients in the risk set
                 between 1985-94 reserved to validate each model."),
               h2("Predictor Methods"),
               p("To resolve the issues of miscalibrated predictions over time
                 we considered different predictor methods:"),
               h3("Standard Analysis"),
               p("In standard analysis we consider the full cohort to be part of the
                 risk set."),
               h3("Period Analysis"),
               p("In period analysis, segment our follow-up time into windows. We
                 estimate the hazard rate within each window while only considering 
                 patients within the risk set during this window. This reduces our sample
                 size, which can lead to predictions with larger variance, however should
                 produce more up-to-date estimates for our hazard function. We can also 
                 vary our window size, with smaller window, in theory, more calibrated estimates."),
               div(
               img(src = "period_analysis.jpeg",height = "rem", width = "1300rem"),
               style = "text-align: center;"),
               div(
                 p(HTML("Contribution of follow-up time from four hypothetical participants (diagnosed 1 January) to a 2-year period window of 2004–05. 
                   <br>Patients A, C and D are are in the risk set during this window and for example patient A only contributes year 9 and 10 of their 
                   follow up period to the model predictions to this window."), style = "text-align: center; font-style: italic; color: gray;")
               ),
               h3("Temporal Recalibration"),
               p("Temporal Recalibration is a hybrid approach combining the benefits, 
                  of larger sample size from the full cohort with more up-to-date 
                  estimates of our survival function. We initially estimate our hazard
                  ratios for our predictor effects considering the full cohort and fix
                  these effects.  We then segment our follow-up time into windows 
                  and restimate our  baseline (log cumulative) hazard function 
                  considering only patients during this window."),
               h2("Predictor Frameworks"),
               p("Within survival analysis there are different frameworks 
                 we can consider in order to decide if an event occurred to a patient 
                 in the risk set:"),
               tags$ul(
                 tags$li("All-Cause - If a patient dies by any cause while in the risk set
                         we consider this to be an event under this framework."),
                 tags$li("Cause-Specific - We only consider deaths where patients
                         die of a given cancer while in the risk set. This is analgous to
                         a reality where the only possible way is to die of cancer.
                         Patients who die from causes other than cancer are censored."),
                 tags$li("Competing Risk - We consider deaths from cancer and deaths
                         from other causes to be mutually exclusive events i.e. 
                         if a patient dies from cancer they are then unable to die from
                         other causes.")
               ),
               p("For the All-Cause and Cause-Specific frameworks we compare the 
               marginal surivival function (obtained by averaging the survival curves of
               each patient in the cohort) to the Kaplan-Meier curve which is a 
               non-parametric estimator for the true survival function to assess 
               model performance. However, for the Competing Risk framework we focus 
               on the probability of the event of interest occurring (Cumulative Incidence Function)
               instead of the survival function. We use a corresponding non-parametric method to 
               estimate the Cumulative Incidence Function called the Aalen-Johansen Estimator."),
               h2("Calibration Plots"),
               p("While comparison between marginal survival/cumulative incidence curves and their
                 respective ground truth estimators allows a comparison as to how well each model performs on
                 average, we are unable to see how well each model performs on individuals
                 We can use calibration plots where for each patient we plot their predicted
                 probability of surival against their pseudo-value. We then plot a smoother
                 to assess overall calibration of each model. More information about pseudo-values
                 can be found",tags$a(href = "https://journals.sagepub.com/doi/10.1177/0962280209105020","here."))
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
                 # Tables to test data input to graphs
                 #card(DT::DTOutput("TestKM")),
                 #card(DT::DTOutput("TestCal")),
                 col_widths = c(6, 6))
               )
             )
           )
  )
)