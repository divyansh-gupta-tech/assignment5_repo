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


# --------------------------------------------------------
# UI
# --------------------------------------------------------

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
      
      # -------------------
      # Core Filters
      # -------------------
      
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
      
      pickerInput("race", "Race",
                  choices = levels(dig$RACE),
                  selected = levels(dig$RACE),
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
      
      sliderInput("creat", "Creatinine",
                  min = min(dig$CREAT, na.rm = TRUE),
                  max = max(dig$CREAT, na.rm = TRUE),
                  value = c(min(dig$CREAT, na.rm = TRUE), max(dig$CREAT, na.rm = TRUE))
      ),
      
      sliderInput("klevel", "Potassium Level",
                  min = min(dig$KLEVEL, na.rm = TRUE),
                  max = max(dig$KLEVEL, na.rm = TRUE),
                  value = c(min(dig$KLEVEL, na.rm = TRUE), max(dig$KLEVEL, na.rm = TRUE))
      ),
      
      sliderInput("heartrate", "Heart Rate",
                  min = min(dig$HEARTRTE, na.rm = TRUE),
                  max = max(dig$HEARTRTE, na.rm = TRUE),
                  value = c(min(dig$HEARTRTE, na.rm = TRUE), max(dig$HEARTRTE, na.rm = TRUE))
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
                    p("The Digitalis Investigation Group (DIG) Trial studied how Digoxin affects 
              mortality and hospitalizations among patients with heart failure.")
                )
              ),
              
              fluidRow(
                box(width = 6, title = "Dataset Overview", status = "info",
                    p("The dataset contains demographic, clinical, and outcome variables for >6800 patients.")
                ),
                
                box(width = 6, title = "How to Use the Dashboard", status = "info",
                    tags$ul(
                      tags$li("Adjust sidebar filters"),
                      tags$li("Explore treatment differences"),
                      tags$li("Analyze outcomes and mortality patterns")
                    )
                )
              ),
              
              # ----------------------
              # Recommended Parameters
              # ----------------------
              fluidRow(
                box(width = 12, title = "Recommended Core Parameters", status = "warning", solidHeader = TRUE,
                    tags$ul(
                      tags$li(tags$b("Demographics:"), " AGE, SEX, RACE, BMI"),
                      tags$li(tags$b("Treatment:"), " TRTMT, DIGDOSE, DIGUSE, ACEINHIB, DIURETICS"),
                      tags$li(tags$b("Clinical status:"), " EJF_PER, FUNCTCLS, NSYM, CREAT, KLEVEL, HEARTRTE, SYSBP"),
                      tags$li(tags$b("Outcomes:"), " DEATH, DEATHDAY, DWHF, HOSP, NHOSP")
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
                box(width = 4, title = "Core Parameters Summary", tableOutput("coreParamsTable")),
                box(width = 4, title = "Mortality by Treatment", plotlyOutput("treatmentOutcome"))
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
      
      # ---------------------------
      # Mortality tab
      # ---------------------------
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
      
      # ---------------------------
      # Data tab
      # ---------------------------
      tabItem(tabName = "data",
              box(width = 12, title = "Filtered Dataset", status = "primary",
                  DTOutput("dataTable"))
      )
    )
  )
)

# ---------------------------
# SERVER
# ---------------------------
server <- function(input, output, session) {
  
  filtered <- reactive({
    dig %>%
      filter(
        TRTMT %in% input$trtmt,
        SEX %in% input$sex,
        AGE >= input$age[1], AGE <= input$age[2],
        EJF_PER >= input$ef[1], EJF_PER <= input$ef[2],
        BMI >= input$bmi[1], BMI <= input$bmi[2]
      )
  })
  
# ---------------------------
# Overview plots
# ---------------------------
  
# Histogram:
output$ageHist <- renderPlotly({
  p <- ggplot(filtered(), aes(AGE, fill = TRTMT)) +
    geom_histogram(
      bins = 20,
      position = "dodge",
      alpha = 0.8,
      color = "black") +
      labs(x = "Age", y = "Count")
    ggplotly(p)
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

# Scatter Plot:
output$efScatter <- renderPlotly({
  p <- ggplot(filtered(), aes(EJF_PER, BMI, color = TRTMT)) +
    geom_point(alpha = 0.5, size = 1.8) +
    scale_color_manual(values = c("Placebo" = "deepskyblue", "Treatment" = "red")) +
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
    geom_col(fill = "deepskyblue2") +
    labs(y = "Mean Hospitalizations")
  
  ggplotly(p)
})

output$nhospBox <- renderPlotly({
  p <- ggplot(filtered(), aes(TRTMT, NHOSP)) +
    geom_boxplot(fill = "steelblue") +
    labs(y = "Hospitalizations")
  
  ggplotly(p)
})

# Plot 1
output$creatEfPlot <- renderPlotly({
  p <- ggplot(filtered(), aes(CREAT, EJF_PER, color = TRTMT)) +
    geom_point(alpha = 0.5, size = 1.8) +
    labs(x = "Creatinine", y = "Ejection Fraction (%)")
  ggplotly(p)
})

# Plot 2 
output$hospRatePlot <- renderPlotly({
  df <- filtered() %>%
    group_by(TRTMT) %>%
    summarise(
      Rate = round(mean(HOSP == 1, na.rm = TRUE) * 100, 1)
    )
  
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
    geom_col(fill = "brown4") +
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

shinyApp(ui,server)  
  
  


                      
