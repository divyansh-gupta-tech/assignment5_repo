#########################
#   DIG TRIAL DASHBOARD
#########################

library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(tidyverse)
library(plotly)
library(DT)

# ---------------------------
# Load and clean data
# ---------------------------

dig <- read_csv("DIG-1.csv", show_col_types = FALSE)

dig <- dig %>%
  mutate(
    TRTMT = factor(TRTMT, levels = c(0, 1), labels = c("Placebo", "Treatment")),
    SEX   = factor(SEX,   levels = c(1, 2), labels = c("Male", "Female")),
    RACE  = factor(RACE,  levels = c(1, 2), labels = c("White", "Nonwhite")),
    DEATH = factor(DEATH, levels = c(0, 1), labels = c("Alive", "Death"))
  )

# Slider-safe ranges
age_min  <- min(dig$AGE, na.rm = TRUE)
age_max  <- max(dig$AGE, na.rm = TRUE)

ef_min   <- min(dig$EJF_PER, na.rm = TRUE)
ef_max   <- max(dig$EJF_PER, na.rm = TRUE)

bmi_min  <- min(dig$BMI, na.rm = TRUE)
bmi_max  <- max(dig$BMI, na.rm = TRUE)

# ---------------------------
# UI
# ---------------------------

ui <- dashboardPage(
  
  dashboardHeader(title = "DIG Trial Explorer"),
  
  dashboardSidebar(
    width = 300,
    sidebarMenu(
      menuItem("Info", tabName = "info", icon = icon("info-circle")),
      menuItem("Overview", tabName = "overview", icon = icon("chart-bar")),
      menuItem("Outcomes", tabName = "outcomes", icon = icon("hospital-user")),
      menuItem("Mortality", tabName = "mortality", icon = icon("skull")),
      menuItem("Data", tabName = "data", icon = icon("table")),
      hr(),
      
      pickerInput("trtmt", "Treatment",
                  choices = levels(dig$TRTMT),
                  selected = levels(dig$TRTMT),
                  multiple = TRUE
      ),
      
      pickerInput("sex", "Sex",
                  choices = levels(dig$SEX),
                  selected = levels(dig$SEX),
                  multiple = TRUE
      ),
      
      sliderInput("age", "Age",
                  min = age_min, max = age_max,
                  value = c(age_min, age_max)
      ),
      
      sliderInput("ef", "Ejection Fraction (%)",
                  min = ef_min, max = ef_max,
                  value = c(ef_min, ef_max)
      ),
      
      sliderInput("bmi", "BMI",
                  min = bmi_min, max = bmi_max,
                  value = c(bmi_min, bmi_max)
      ),
      
      downloadButton("downloadData", "Download filtered CSV")
    )
  ),
  
  dashboardBody(
    tags$head(tags$style(HTML("
      .box {border-radius: 8px;}
      .content {padding: 20px;}
    "))),
    
  tabItems(
      
# ---------------------------
# Info tab
# ---------------------------
tabItem(tabName = "info",
        fluidRow(
          box(width = 12, title = "About the DIG Trial", status = "primary", solidHeader = TRUE,
          p("The Digitalis Investigation Group (DIG) Trial studied how Digoxin affects mortality and hospitalizations among patients with heart failure."),
          p("This dashboard lets you explore the DIG dataset interactively, compare treatment groups, examine clinical features, and summarise mortality risk over time."))
              ),
              
        fluidRow(
          box(width = 6, title = "Dataset Overview", status = "info",
          p("The dataset contains demographic, clinical, and outcome variables for more than 6800 patients."),
          p("Key variables include:"),
          tags$ul(
          tags$li("AGE — Patient age"),
          tags$li("TRTMT — Treatment group (Placebo, Digoxin)"),
          tags$li("EJF_PER — Ejection Fraction (%)"),
          tags$li("BMI — Body Mass Index"),
          tags$li("KLEVEL — Potassium level"),
          tags$li("CREAT — Creatinine"),
          tags$li("NHOSP — Number of hospitalizations"),
          tags$li("DEATH — Mortality status"),
          tags$li("DEATHDAY — Follow-up time (days)")
          ),
          tags$a(href = "https://github.com/divyansh-gupta-tech/assignment5_repo/blob/main/DIG_code_book-1.pdf", target = "_blank",
                 "Click here to open the DIG codebook")
          ),
          
          box(width = 6, title = "How to Use the Dashboard", status = "info",
          p("Use the sidebar filters to narrow down the dataset by treatment group, sex, age, 
               ejection fraction, and BMI."),
          tags$ul(
          tags$li("Overview of demographics and treatment patterns"),
          tags$li("Hospitalization outcomes"),
          tags$li("Monthly mortality risk summaries"),
          tags$li("Interactive full dataset viewer")
              )
          )
        )
),

# ---------------------------
# Overview tab
# ---------------------------
tabItem(tabName = "overview",
        fluidRow(
          box(width = 6, title = "Age distribution by Treatment", status = "primary",
              solidHeader = TRUE, plotlyOutput("ageHist")),
          box(width = 6, title = "Ejection Fraction vs BMI", status = "primary",
              solidHeader = TRUE, plotlyOutput("efScatter"))
        ),
        fluidRow(
          box(width = 4, title = "Summary Statistics", tableOutput("demoTable")),
          box(width = 8, title = "Mortality by Treatment", plotlyOutput("treatmentOutcome"))
        )
),

# ---------------------------
# Outcomes tab
# ---------------------------
tabItem(tabName = "outcomes",
        fluidRow(
          box(width = 6, title = "Mean Hospitalizations", status = "primary",
              solidHeader = TRUE, plotlyOutput("hospBar")),
          box(width = 6, title = "NHOSP Distribution", status = "primary",
              solidHeader = TRUE, plotlyOutput("nhospBox"))
        ),
        hr(),
        fluidRow(
          box(width = 6, title = "Creatinine vs Ejection Fraction", status = "info",
              solidHeader = TRUE, plotlyOutput("creatEfPlot")),
          box(width = 6, title = "Hospitalization Rate (%)", status = "info",
              solidHeader = TRUE, plotlyOutput("hospRatePlot"))
        )
),


                      
