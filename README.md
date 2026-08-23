# NZ Crash Analysis

This project analyses New Zealand crash data using **R** and **Shiny**.

## Features

* Crash data overview
* Crash statistics by region, TLA, and year
* Crash severity analysis
* Interactive crash location map
* Top crash locations
* Ordinal logistic regression for crash severity

## Files

* `global.R` - Loads data, packages, and prepares the model
* `ui.R` - Defines the Shiny app interface
* `server.R` - Contains the server logic and visualisations
* `CAS_Data_public.csv` - Crash dataset

## Run the App

Open the project in RStudio and run:

```r
shiny::runApp()
```

## Data

The analysis uses New Zealand open crash data from the Crash Analysis System (CAS).
