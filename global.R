
# global.R

# PACKAGES

library(shiny)
library(shinydashboard)
library(DT)
library(MASS)
library(dplyr)
library(tidyr)
library(ggplot2)
library(leaflet)
library(sf)


# DATA LOADING

dat <- read.csv("CAS_Data_public.csv",
  stringsAsFactors = TRUE,
  na.strings = c("", "Null")
)


# DATA INFORMATION

# No: of rows, columns
total_rows <- nrow(dat)
total_columns <- ncol(dat)

# Data types
data_types <- data.frame(
  Variable = names(dat),
  Data_Type = sapply(dat, class)
)

# Missing Values
missing_data <- data.frame(
  Variable = names(dat),
  Missing_Count = colSums(is.na(dat)),
  Missing_Percent = round(colMeans(is.na(dat)) * 100, 2)
)

# First highest missing percentage
missing_data <- missing_data %>%
  arrange(desc(Missing_Percent))





# CRASH SEVERITY

severity_colours <- c(
  "Fatal Crash" = "#D7191C",   
  "Minor Crash" = "#FFD700", 
  "Non-Injury Crash" = "#2CA25F",
  "Serious Crash" = "#FF7F00"   
        
  
)



# MODELLING


# variables 
model_data <- dat %>%
  select(
    crashSeverity,
    speedLimit,
    light,
    weatherA,
    NumberOfLanes
  ) %>%
  drop_na()

#  Lowest -> highest
model_data$crashSeverity <- ordered(
  model_data$crashSeverity,
  levels = c(
    "Non-Injury Crash",
    "Minor Crash",
    "Serious Crash",
    "Fatal Crash"
  )
)

# ordinal logistic regression
severity_model <- polr(
  crashSeverity ~ speedLimit + light + weatherA + NumberOfLanes,
  data = model_data
)