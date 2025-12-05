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

# SERVER


server <- function(input, output, session) {
  
# Filtered Data

filtered <- reactive({
  dig %>%
    filter(
      TRTMT %in% input$trtmt,
      AGE >= input$age[1], AGE <= input$age[2],
      RACE %in% input$race,
      SEX %in% input$sex,
      BMI >= input$bmi[1], BMI <= input$bmi[2]
           )
                     })

# OVERVIEW TAB

# Dataset summary
output$datasetSummary <- renderTable({
  df <- filtered()
  tibble(
    Total_Patients = nrow(df),
    Placebo = sum(df$TRTMT == "Placebo"),
    Digoxin = sum(df$TRTMT == "Digoxin"),
    Avg_Followup = round(mean(df$DEATHDAY, na.rm = TRUE), 1)
        )
                                      })
  
# Demographics
output$ageDist <- renderPlotly({
  ggplotly(
    ggplot(filtered(), aes(AGE)) +
      geom_histogram(fill = "steelblue", bins = 30, color = "white") +
      labs(x = "Age", y = "Count")
           )
                                })
  
output$sexDist <- renderPlotly({
  df <- filtered() %>% count(SEX)
  plot_ly(df, labels = ~SEX, values = ~n, type = "pie")
                                })
  
output$raceDist <- renderPlotly({
  df <- filtered() %>% count(RACE)
  ggplotly(
    ggplot(df, aes(RACE, n, fill = RACE)) +
      geom_col() +
      labs(x = "Race", y = "Count")
           )
                                 })
  
output$bmiBox <- renderPlotly({
  ggplotly(
    ggplot(filtered(), aes(y = BMI)) +
      geom_boxplot(fill = "#66C2A5") +
      labs(y = "BMI")
           )
                               })
  
# Clinical Baseline
output$efHist <- renderPlotly({
  ggplotly(
    ggplot(filtered(), aes(EJF_PER)) +
      geom_histogram(fill = "orange", bins = 30, color = "white") +
      labs(x = "Ejection Fraction (%)", y = "Count")
          )
                              })
  
output$creatBox <- renderPlotly({
  ggplotly(
    ggplot(filtered(), aes(y = CREAT)) +
      geom_boxplot(fill = "#8DA0CB") +
      labs(y = "Creatinine")
          )
                                 })
  
output$klevelBox <- renderPlotly({
  ggplotly(
    ggplot(filtered(), aes(y = KLEVEL)) +
      geom_boxplot(fill = "pink2") +
      labs(y = "Potassium Level")
           )
                                  })

# Quick Outcomes
output$deathPie <- renderPlotly({
  df <- filtered() %>% count(DEATH)
  plot_ly(df, labels = ~DEATH, values = ~n, type = "pie")
                                 })
  
output$hospBarOverview <- renderPlotly({
  df <- filtered() %>% count(HOSP)
  ggplotly(
    ggplot(df, aes(factor(HOSP), n)) +
      geom_col(fill = "green3") +
      labs(x = "Hospitalized?", y = "Count")
           )
                                        })
  

# OUTCOMES TAB

output$patientsByTreatment <- renderPlotly({
  df <- filtered() %>% count(TRTMT)
  ggplotly(
    ggplot(df, aes(TRTMT, n, fill = TRTMT)) +
      geom_col() +
      labs(y = "Number of Patients")
          )
                                           })
  
output$comparisonTable <- renderTable({
  filtered() %>%
    group_by(TRTMT) %>%
    summarise(
      N = n(),
      Mean_Age = round(mean(AGE, na.rm = TRUE), 1),
      Mean_BMI = round(mean(BMI, na.rm = TRUE), 1),
      Mortality = sum(DEATH == "Death")
             )
                                       })
  
output$cvdMortality <- renderPlotly({
  ggplotly(
    ggplot(filtered(), aes(CVD, fill = DEATH)) +
      geom_bar(position = "fill") +
      labs(y = "Proportion")
          )
                                    })
  
output$hospTreatment <- renderPlotly({
  df <- filtered() %>%
    group_by(TRTMT) %>%
    summarise(HospRate = mean(HOSP == 1))
    
  ggplotly(
    ggplot(df, aes(TRTMT, HospRate, fill = TRTMT)) +
      geom_col()
          )
                                     })
  
output$mortalityTreatment <- renderPlotly({
  df <- filtered() %>%
    group_by(TRTMT) %>%
    summarise(MortRate = mean(DEATH == "Death"))
    
  ggplotly(
    ggplot(df, aes(TRTMT, MortRate, fill = TRTMT)) +
      geom_col()
          )
                                         })
  
output$bpRelation <- renderPlotly({
  ggplotly(
    ggplot(filtered(), aes(SYSBP, DIABP, color = TRTMT)) +
      geom_point(alpha = 0.4) +
      labs(x = "Systolic BP", y = "Diastolic BP")
          )
                                   })

# MORTALITY TAB (RESTORED)

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
        Deaths = sum(Died),
        Total = n(),
        Mortality_Risk = round(Deaths / Total, 3)
      )
    datatable(df)
  })
  
  output$monthlyMortalityPlot <- renderPlotly({
    df <- monthly_data() %>%
      group_by(Month) %>%
      summarise(Deaths = sum(Died))
    
    ggplotly(
      ggplot(df, aes(Month, Deaths)) +
        geom_col(fill = "red3") +
        labs(x = "Month", y = "Deaths")
    )
  })
  
  output$monthlyMortalityByTrtTable <- renderDT({
    df <- monthly_data() %>%
      group_by(Month, TRTMT) %>%
      summarise(
        Deaths = sum(Died),
        Total = n(),
        Mortality_Risk = round(Deaths / Total, 3)
      )
    datatable(df)
  })
  
  output$monthlyMortalityByTrtPlot <- renderPlotly({
    df <- monthly_data() %>%
      group_by(Month, TRTMT) %>%
      summarise(Deaths = sum(Died))
    
    ggplotly(
      ggplot(df, aes(Month, Deaths, fill = TRTMT)) +
        geom_col(position = "dodge")
    )
  })
  
# DATA TABLE

  output$dataTable <- renderDT({
    datatable(
      filtered(),
      options = list(scrollX = TRUE),
      filter = "top"
    )
  })
  
  output$downloadData <- downloadHandler(
    filename = function() paste0("DIG_filtered_", Sys.Date(), ".csv"),
    content = function(file) write_csv(filtered(), file)
  )
}

shinyApp(ui, server)
