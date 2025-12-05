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
    
