
# ui.R

ui <- dashboardPage(
  
  dashboardHeader(title = "Crash Analysis"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem(text = "Data Overview", tabName = "overview", icon = icon("table")),
      menuItem(text = "Crash Insights", tabName = "insights", icon = icon("chart-bar")),
      menuItem(text = "Modelling", tabName = "modelling", icon = icon("chart-line"))
    )
  ),
  
  dashboardBody(
    tabItems(
      
      # OVERVIEW
      tabItem(
        tabName = "overview",
        h2("Crash Data Overview"),
        fluidRow(
          valueBoxOutput("total_rows", width = 6),
          valueBoxOutput("total_columns", width = 6)
        ),
        
        tabBox(
          width = 16,
          tabPanel("Dataset", DTOutput("data_table")),
          tabPanel("Year Distribution",
            column(width = 6, plotOutput("year_distribution", height = "400px"))),
          tabPanel("Data Types", DTOutput("data_types_table")),
          tabPanel("Summary", verbatimTextOutput("data_summary")),
          tabPanel("Missing Values",
            DTOutput("missing_table"),
            plotOutput("missing_plot", height = "600px")
          )
        )
      ),
      
      
      # CRASH INSIGHTS
      tabItem(
        tabName = "insights",
        h2("New Zealand Crash Analysis"),
        
        fluidRow(
          column(width = 4, selectInput("region", "Region", NULL)),
          column(width = 4, selectInput("tla", "TLA", NULL)),
          column(width = 4, selectInput("year", "Year", NULL))
        ),
        
        fluidRow(
          valueBoxOutput("total_crashes", width = 3),
          valueBoxOutput("fatal_crashes", width = 3),
          valueBoxOutput("serious_crashes", width = 3),
          valueBoxOutput("minor_crashes", width = 3)
        ),
        
        fluidRow(
          box(title = "Crash Location Map", width = 6, leafletOutput("crash_map", height = "500px")),
          box(title = "Top Five Crash Locations", width = 6, plotOutput("top_locations", height = "500px"))
        ),
        
        box(title = "Crash Severity Analysis", width = 16,
          fluidRow(
            column(width = 6, selectInput("analyse_by", "Analyse by",
                c(
                  "Weather",
                  "Speed Limit",
                  "Lanes",
                  "Light",
                  "Holiday"
                )
              )
            )
            ),
            
          
          fluidRow(
            column(width = 6, plotOutput("severity_analysis", height = "450px")),
            column(width = 6, DTOutput("severity_table"))
          )
        )
      ),
      
      
      # MODELLING
      tabItem(
        tabName = "modelling",
        h2("Crash Severity Modelling"),
        h4("Ordinal Logistic Regression"),
        p(
          "The model examines how road and environmental factors are associated with crash severity."
        ),
        # summary stat
        verbatimTextOutput("model_results")
      )
    )
  )
)