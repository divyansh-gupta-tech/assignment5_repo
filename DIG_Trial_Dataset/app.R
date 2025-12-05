#   DIG TRIAL DASHBOARD

library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(tidyverse)
library(plotly)
library(DT)

# Load and clean data

dig <- read_csv("DIG-1.csv", show_col_types = FALSE)

dig <- dig %>%
  mutate(
    TRTMT = factor(TRTMT, levels = c(0, 1), labels = c("Placebo", "Digoxin")),
    SEX   = factor(SEX,   levels = c(1, 2), labels = c("Male", "Female")),
    RACE  = factor(RACE,  levels = c(1, 2), labels = c("White", "Nonwhite")),
    DEATH = factor(DEATH, levels = c(0, 1), labels = c("Alive", "Death"))
  )

# Slider ranges
age_min  <- min(dig$AGE, na.rm = TRUE)
age_max  <- max(dig$AGE, na.rm = TRUE)

bmi_min  <- min(dig$BMI, na.rm = TRUE)
bmi_max  <- max(dig$BMI, na.rm = TRUE)

# UI

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
      

# Sidebar Filters

pickerInput("trtmt", "Treatment",
            choices = levels(dig$TRTMT),
            selected = levels(dig$TRTMT),
            multiple = TRUE
            ),
      
sliderInput("age", "Age",
            min = age_min, max = age_max,
            value = c(age_min, age_max)
            ),
      
pickerInput("race", "Race",
            choices = levels(dig$RACE),
            selected = levels(dig$RACE),
            multiple = TRUE
            ),
      
pickerInput("sex", "Sex",
            choices = levels(dig$SEX),
            selected = levels(dig$SEX),
            multiple = TRUE
           ),
      
sliderInput("bmi", "BMI",
            min = bmi_min, max = bmi_max,
            value = c(bmi_min, bmi_max)
           ),
      
downloadButton("downloadData", "Download filtered CSV")
)
  ),
  

# Body

dashboardBody(
tags$head(tags$style(HTML(".box {border-radius: 8px;}
.content {padding: 20px;}
    "))),
    
tabItems(
      

# INFO TAB

tabItem(tabName = "info",
        fluidRow(
          box(width = 12, title = "About the DIG Trial", status = "primary", solidHeader = TRUE,
          p("The Digitalis Investigation Group (DIG) Trial studied how Digoxin affects mortality and hospitalizations among heart failure patients."))
                ),
        fluidRow(
          box(width = 6, title = "Dataset Overview", status = "info",
          p("The dataset contains demographic, clinical, and outcome variables for 6800+ patients.")
              ),
          box(width = 6, title = "How to Use the Dashboard", status = "info",
              tags$ul(
              tags$li("Use sidebar filters to adjust sample."),
              tags$li("Navigate tabs for different analyses."),
              tags$li("Download filtered dataset anytime.")
                     )
              )
                )
        ),
      

# OVERVIEW TAB

tabItem(tabName = "overview",

# 1. Dataset Summary

box(width = 12, title = "Dataset Summary", status = "primary", solidHeader = TRUE,
    tableOutput("datasetSummary")
              ),
              
# 2. Demographics

box(width = 12, title = "Demographics", status = "info", solidHeader = TRUE,
    fluidRow(
      column(6, plotlyOutput("ageDist")),
      column(6, plotlyOutput("sexDist"))
            ),
    fluidRow(
      column(6, plotlyOutput("raceDist")),
      column(6, plotlyOutput("bmiBox"))
            )
    ),
              
# 3. Clinical Baseline

box(width = 12, title = "Clinical Baseline", status = "warning", solidHeader = TRUE,
    fluidRow(
      column(6, plotlyOutput("efHist")),
      column(6, plotlyOutput("creatBox"))
            ),
    fluidRow(
      column(6, plotlyOutput("klevelBox"))
                  )
    ),
              
# 4. Quick Outcomes Snapshot

box(width = 12, title = "Quick Outcomes Snapshot", status = "danger", solidHeader = TRUE,
    fluidRow(
      column(6, plotlyOutput("deathPie")),
      column(6, plotlyOutput("hospBarOverview"))
            )
    )
        ),
      
# OUTCOMES TAB

tabItem(tabName = "outcomes",
        box(width = 12, title = "1) Patients in Each Treatment Group", status = "primary",
            solidHeader = TRUE, plotlyOutput("patientsByTreatment")),
              
        box(width = 12, title = "2) Comparison of Treatment Groups", status = "info",
            solidHeader = TRUE, tableOutput("comparisonTable")),
              
        box(width = 12, title = "3) Cardiovascular Disease vs Mortality", status = "warning",
            solidHeader = TRUE, plotlyOutput("cvdMortality")),
              
        box(width = 12, title = "4) Hospitalization Rate by Treatment", status = "primary",
            solidHeader = TRUE, plotlyOutput("hospTreatment")),
        
        box(width = 12, title = "5) Mortality Rate by Treatment", status = "danger",
            solidHeader = TRUE, plotlyOutput("mortalityTreatment")),
              
        box(width = 12, title = "6) Relationship Between Systolic & Diastolic BP", status = "info",
            solidHeader = TRUE, plotlyOutput("bpRelation"))
       ),
      
# MORTALITY TAB 

tabItem(tabName = "mortality",
        fluidRow(
          box(width = 12, title = "Monthly Mortality Summary", status = "primary",
              solidHeader = TRUE, DTOutput("monthlyMortalityTable"))
                ),
        
        fluidRow(
          box(width = 12, title = "Deaths per Month", status = "primary",
              solidHeader = TRUE, plotlyOutput("monthlyMortalityPlot"))
                ),
              
        hr(),
              
        fluidRow(
          box(width = 12, title = "Monthly Deaths by Treatment Group", status = "primary",
              solidHeader = TRUE, plotlyOutput("monthlyMortalityByTrtPlot"))
                ),
              
        fluidRow(
          box(width = 12, title = "Monthly Mortality by Treatment Table", status = "primary",
              solidHeader = TRUE, DTOutput("monthlyMortalityByTrtTable"))
                )
        ),
      
# DATA TAB 

tabItem(tabName = "data",
        box(width = 12, title = "Filtered Dataset", status = "primary", solidHeader = TRUE,
            DTOutput("dataTable"))
      )
    )
  )
)


# --------------------------------------------------------
# SERVER
# --------------------------------------------------------

server <- function(input, output, session) {
  
  filtered <- reactive({
    dig %>%
      filter(
        TRTMT %in% input$trtmt,
        SEX %in% input$sex,
        RACE %in% input$race,
        AGE >= input$age[1], AGE <= input$age[2],
        EJF_PER >= input$ef[1], EJF_PER <= input$ef[2],
        BMI >= input$bmi[1], BMI <= input$bmi[2],
        CREAT >= input$creat[1], CREAT <= input$creat[2],
        KLEVEL >= input$klevel[1], KLEVEL <= input$klevel[2],
        HEARTRTE >= input$heartrate[1], HEARTRTE <= input$heartrate[2]
      )
  })
  
# ---------------------------
# Overview plots
# ---------------------------

  # No decimal ages
  output$ageHist <- renderPlotly({
    p <- ggplot(filtered(), aes(x = AGE, fill = TRTMT)) +
      geom_histogram(
        binwidth = 5,
        position = "dodge",
        alpha = 0.8,
        color = "black"
      ) +
      scale_x_continuous(breaks = seq(age_min, age_max, 5)) +
      labs(x = "Age", y = "Count")
    
    ggplotly(p, tooltip = c("x", "y", "fill"))
  })
  
  output$efScatter <- renderPlotly({
    p <- ggplot(filtered(), aes(EJF_PER, BMI, color = TRTMT)) +
      geom_point(alpha = 0.5, size = 1.8) +
      labs(x = "Ejection Fraction (%)", y = "BMI")
    ggplotly(p)
  })
  
  output$demoTable <- renderTable({
    df <- filtered()
    tibble(
      N            = nrow(df),
      Mean_Age     = round(mean(df$AGE, na.rm = TRUE), 2),
      Median_EF    = round(median(df$EJF_PER, na.rm = TRUE), 2),
      Deaths       = sum(df$DEATH == "Death", na.rm = TRUE)
    )
  })
  
# Adding: 
    output$coreParamsTable <- renderTable({
      df <- filtered()
      tibble(
        Mean_Age   = round(mean(df$AGE, na.rm = TRUE), 1),
        Mean_BMI   = round(mean(df$BMI, na.rm = TRUE), 1),
        Mean_EF    = round(mean(df$EJF_PER, na.rm = TRUE), 1),
        Mean_Creat = round(mean(df$CREAT, na.rm = TRUE), 2),
        Mean_K     = round(mean(df$KLEVEL, na.rm = TRUE), 2),
        Mean_HR    = round(mean(df$HEARTRTE, na.rm = TRUE), 1),
        Mortality  = sum(df$DEATH == "Death")
      )
    })
    
    output$treatmentOutcome <- renderPlotly({
      df <- filtered() %>%
        group_by(TRTMT, DEATH) %>%
        summarise(n = n(), .groups = "drop")
      
      p <- ggplot(df, aes(TRTMT, n, fill = DEATH)) +
        geom_col() +
        labs(y = "Count")
      
      ggplotly(p)
    })
    
    # ---------------------------
    # Outcomes visualizations
    # ---------------------------
    
    output$hospBar <- renderPlotly({
      df <- filtered() %>%
        group_by(TRTMT) %>%
        summarise(mean_nhosp = mean(NHOSP, na.rm = TRUE))
      
      p <- ggplot(df, aes(TRTMT, mean_nhosp)) +
        geom_col(fill = "steelblue") +
        labs(y = "Mean Hospitalizations")
      
      ggplotly(p)
    })
    
    output$nhospBox <- renderPlotly({
      p <- ggplot(filtered(), aes(TRTMT, NHOSP)) +
        geom_boxplot(fill = "steelblue") +
        labs(y = "Hospitalizations")
      
      ggplotly(p)
    })
    
    output$creatEfPlot <- renderPlotly({
      p <- ggplot(filtered(), aes(CREAT, EJF_PER, color = TRTMT)) +
        geom_point(alpha = 0.5, size = 1.8)
      ggplotly(p)
    })
    
    output$hospRatePlot <- renderPlotly({
      df <- filtered() %>%
        group_by(TRTMT) %>%
        summarise(Rate = round(mean(HOSP == 1, na.rm = TRUE) * 100, 1))
      
      p <- ggplot(df, aes(TRTMT, Rate)) +
        geom_col(fill = "purple") +
        labs(y = "Hospitalization Rate (%)")
      
      ggplotly(p)
    })
    
    # ---------------------------
    # Monthly Mortality
    # ---------------------------
    
    monthly_data <- reactive({
      filtered() %>%
        mutate(
          Month = floor(DEATHDAY / 30),
          Died = ifelse(DEATH == "Death", 1, 0)
        )
    })
    
    output$monthlyMortalityTable <- renderDT({
      df <- monthly_data() %>%
        group_by(Month) %>%
        summarise(
          Deaths = sum(Died, na.rm = TRUE),
          Total = n(),
          Mortality_Risk = round(Deaths / Total, 3)
        )
      datatable(df)
    })
    
    output$monthlyMortalityPlot <- renderPlotly({
      df <- monthly_data() %>%
        group_by(Month) %>%
        summarise(Deaths = sum(Died, na.rm = TRUE))
      
      p <- ggplot(df, aes(Month, Deaths)) +
        geom_col(fill = "brown") +
        labs(x = "Month", y = "Deaths")
      
      ggplotly(p)
    })
    
    output$monthlyMortalityByTrtTable <- renderDT({
      df <- monthly_data() %>%
        group_by(Month, TRTMT) %>%
        summarise(
          Deaths = sum(Died, na.rm = TRUE),
          Total = n(),
          Mortality_Risk = round(Deaths / Total, 3),
          .groups = "drop"
        )
      datatable(df)
    })
    
    output$monthlyMortalityByTrtPlot <- renderPlotly({
      df <- monthly_data() %>%
        group_by(Month, TRTMT) %>%
        summarise(Deaths = sum(Died, na.rm = TRUE), .groups = "drop")
      
      p <- ggplot(df, aes(Month, Deaths, fill = TRTMT)) +
        geom_col(position = "dodge") +
        labs(x = "Month", y = "Deaths")
      
      ggplotly(p)
    })
    
    # ---------------------------
    # Data Table
    # ---------------------------
    
    output$dataTable <- renderDT({
      datatable(filtered(), filter = "top")
    })
    
    output$downloadData <- downloadHandler(
      filename = function() paste0("DIG_filtered_", Sys.Date(), ".csv"),
      content = function(file) write_csv(filtered(), file)
    )
  }
  
  shinyApp(ui, server)  