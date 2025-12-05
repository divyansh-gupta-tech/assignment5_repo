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
