
# server.R

server <- function(input, output, session) {

  
  # OVERVIEW
  
  output$total_rows <- renderValueBox({
    valueBox(value = total_rows, subtitle = "Total Rows", icon = icon(name = "database"))
  })
  
  output$total_columns <- renderValueBox({
    valueBox(value = total_columns, subtitle = "Total Columns", icon = icon(name = "columns"))
  })
  
  output$data_table <- renderDT({
    datatable(data = dat, options = list(pageLength = 10, scrollX = TRUE))
  })
  
  output$data_types_table <- renderDT({
    datatable(data = data_types, options = list(pageLength = 15))
  })
  
  output$data_summary <- renderPrint({
    summary(dat)
  })
  
  output$missing_table <- renderDT({
    datatable(data = missing_data, options = list(pageLength = 15))
  })
  
  output$missing_plot <- renderPlot({
    ggplot(missing_data, aes(x = reorder(Variable, Missing_Percent), y = Missing_Percent)) +
      geom_col(fill = "steelblue") +
      coord_flip() +
      labs(x = "Variable", y = "Missing Percentage") +
      theme_minimal() +
      theme(
        axis.text = element_text(size = 9, face = "bold")
      )
  })
  
  # YEAR DISTRIBUTION
  
  output$year_distribution <- renderPlot({
    year_data <- dat %>%
      count(crashYear)
    ggplot(year_data, aes(x = crashYear, y = n)) +
      geom_col(fill = "steelblue") +
      geom_text(aes(label = n), vjust = -0.5) +
      labs(x = "Year", y = "Number of Crashes") +
      theme_minimal()
  })

  
  
  # FILTERS
  
  # Region
  observe({
    regions <- sort(unique(na.omit(dat$region)))
    updateSelectInput(
      session,
      "region",
      choices = regions,
      selected = "Auckland Region"
    )
  })
  
  
  # TLA depends on Region
  observeEvent(input$region, {
    req(input$region)
    tla_names <- dat %>%
      filter(region == input$region) %>%
      pull(tlaName) %>%
      na.omit() %>%
      unique() %>%
      sort()
    updateSelectInput(
      session,
      "tla",
      choices = tla_names,
      
    )
  })
  
  
  # Year depends on Region and Tla
  observeEvent(c(input$region, input$tla), {
    req(input$region, input$tla)
    years <- dat %>% filter(
        region == input$region,
        tlaName == input$tla
      ) %>%
      pull(crashYear) %>%
      na.omit() %>%
      unique() %>%
      sort()
    updateSelectInput(
      session,
      "year",
      choices = years
    )
  })
  
  
  # Filter data using Region, TLA and Year
  filtered_data <- reactive({
    req(input$region, input$tla, input$year)
    data <- dat %>%
      filter(
        region == input$region,
        tlaName == input$tla,
        as.character(crashYear) == input$year
      )
    
    req(nrow(data) > 0)
    
    data
  })
  
  # CRASH TOTALS
  
  output$total_crashes <- renderValueBox({
    valueBox(
      nrow(filtered_data()),
      "Total Crashes",
      icon = icon("car"),
      color = "blue"
    )
  })
  
  
  output$fatal_crashes <- renderValueBox({
    number <- sum(filtered_data()$crashSeverity == "Fatal Crash",na.rm = TRUE)
    valueBox(
      number,
      "Fatal Crashes",
      color = "red"
    )
  })
  
  
  output$serious_crashes <- renderValueBox({
    number <- sum(filtered_data()$crashSeverity == "Serious Crash", na.rm = TRUE)
    valueBox(
      number,
      "Serious Crashes",
      color = "orange"
    )
  })
  
  
  output$minor_crashes <- renderValueBox({
    number <- sum(filtered_data()$crashSeverity == "Minor Crash", na.rm = TRUE)
    valueBox(
      number,
      "Minor Crashes",
      color = "yellow"
    )
  })
  

  # TOP FIVE LOCATIONS
  
  output$top_locations <- renderPlot({
    # For top 5 locations
    top_names <- filtered_data() %>%
      count(crashLocation1, sort = TRUE) %>%
      slice_head(n = 5) %>%
      pull(crashLocation1)
    
    
    # Count crash by severity
    top_data <- filtered_data() %>%
      filter(crashLocation1 %in% top_names) %>%
      count(crashLocation1, crashSeverity)
    
    
    ggplot(
      top_data,
      aes(x = crashLocation1, y = n, fill = crashSeverity)) +
      geom_col(position = "dodge") +
      geom_text(aes(label = n),
        position = position_dodge(width = 0.9),
        hjust = -0.2
      ) +
      scale_fill_manual(values = severity_colours) +
      coord_flip() +
      labs(x = NULL, y = "Number of Crashes", fill = "Crash Severity") +
      theme_minimal() +
      theme(
        axis.text = element_text(size = 12, face = "bold"),
        axis.title = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12, face = "bold")
      )
  })
  

  # CRASH MAP
  
  output$crash_map <- renderLeaflet({
    map_data <- filtered_data() %>%
      filter(!is.na(X), !is.na(Y))
    
    req(nrow(map_data) > 0)
    
    
    # Convert NZTM coord
    map_data <- st_as_sf(
      map_data, coords = c("X", "Y"), crs = 2193)
    
    # Convert to LONG and LAT
    map_data <- st_transform(map_data, 4326)
    coordinates <- st_coordinates(map_data)
    map_data$longitude <- coordinates[, 1]
    map_data$latitude <- coordinates[, 2]
    
    
    # Severity colours
    colours <- colorFactor(
      palette = severity_colours,
      domain = names(severity_colours)
    )
    
    
    leaflet(map_data) %>%
      addProviderTiles(providers$OpenStreetMap) %>%
      addCircleMarkers(
        lng = ~longitude,
        lat = ~latitude,
        radius = 10,
        color = "black",
        weight = 3, 
        fillColor = ~colours(crashSeverity),
        fillOpacity = 0.8,
        
        popup = ~paste0(
          "Location: ", crashLocation1,
          "<br>Severity: ", crashSeverity,
          "<br>Speed Limit: ", speedLimit
        ),
    
        clusterOptions = markerClusterOptions()
        ) %>%
      
      addLegend(position = "bottomleft",
        colors = severity_colours,
        labels = names(severity_colours),
        title = "Crash Severity"
      )
  })
  
  # ANALYSIS DATA
  analysis_data <- reactive({
    data <- filtered_data()
    
    # VAR selection
    variable <- switch(
      input$analyse_by,
      "Weather" = "weatherA",
      "Speed Limit" = "speedLimit",
      "Lanes" = "NumberOfLanes",
      "Light" = "light",
      "Holiday" = "holiday"
    )
    
    # Count crashes by CATEGORY and SEVERITY
    result <- data %>%
      filter(
        !is.na(.data[[variable]]),
        !is.na(crashSeverity)
      ) %>%
      count(Category = .data[[variable]], crashSeverity, name = "Count")
    result
  })
  
  
  # GRAPH
  output$severity_analysis <- renderPlot({
    data <- analysis_data()
    ggplot(data,
      aes(x = reorder(Category, Count, sum), y = Count, fill = crashSeverity)) +
      geom_col() +
      coord_flip() +
      scale_fill_manual(values = severity_colours) +
      labs(title = paste("Crash Severity -", input$analyse_by),
        x = NULL, y = "Number of Crashes", fill = "Crash Severity") +
      theme_minimal() +
      theme(
        axis.text = element_text(size = 12, face = "bold"),
        axis.title = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12, face = "bold")
      )
  })
  
  
  # TABLE
  output$severity_table <- renderDT({
    table_data <- analysis_data() %>%
      select(Category, crashSeverity, Count) %>%
      pivot_wider(
        names_from = crashSeverity,
        values_from = Count,
        values_fill = 0
      )
    
    # Add count for get Total
    table_data$Total <- rowSums(table_data[, -1])
    datatable(table_data, rownames = FALSE)
  })
  
  
  # MODEL RESULTS
  
  output$model_results <- renderPrint({
    summary(severity_model)
    
  })
}