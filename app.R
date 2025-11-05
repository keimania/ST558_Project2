# 1. SETUP
library(shiny)
library(tidyverse)
library(lubridate)
library(DT)
library(shinycssloaders)
library(janitor)


# Load and prepare the dataset
# NOTE: Assumes "SeoulBikeData.csv" is in a folder named "data" (./data/SeoulBikeData.csv)
bike_data_raw <- read_csv("./data/SeoulBikeData.csv", locale = locale(encoding = "latin1"))

# Clean and prepare data
bike_data <- bike_data_raw |>
  janitor::clean_names() |>
  mutate(
    date = dmy(date),
    seasons = factor(seasons, levels = c("Spring", "Summer", "Autumn", "Winter")),
    holiday = factor(holiday, levels = c("No Holiday", "Holiday")),
    functioning_day = factor(functioning_day, levels = c("Yes", "No")),
    hour = factor(hour)
  )

# Identify categorical and numeric variables for selectors
categorical_vars <- c("seasons", "holiday", "functioning_day", "hour")
numeric_vars <- bike_data |> select(where(is.numeric)) |> names()
# Exclude 'hour' from numeric_vars if it's already categorical
numeric_vars <- setdiff(numeric_vars, "hour")


# 2. USER INTERFACE (UI)
ui <- fluidPage(
  titlePanel("Seoul Bike Sharing Data Explorer"),
  
  sidebarLayout(
    # --- Sidebar Panel for Inputs ---
    sidebarPanel(
      h3("Data Filtering Controls"),
      p("Use the options below to filter the data. Click 'Apply Filters' to see changes."),
      
      # Categorical Variable Selectors
      selectInput("season_select", "Filter by Season:",
                  choices = c("All", unique(as.character(bike_data$seasons)))),
      
      selectInput("holiday_select", "Filter by Holiday:",
                  choices = c("All", unique(as.character(bike_data$holiday)))),
      
      # Dynamic Numeric Variable Selectors & Sliders
      selectInput("num_var1_select", "Select First Numeric Variable to Filter:",
                  choices = numeric_vars, selected = "temperature_c"),
      uiOutput("num_slider1_ui"), # Dynamic slider UI
      
      selectInput("num_var2_select", "Select Second Numeric Variable to Filter:",
                  choices = numeric_vars, selected = "humidity_percent"),
      uiOutput("num_slider2_ui"), # Dynamic slider UI
      
      # Action Button to apply filters
      actionButton("update_btn", "Apply Filters", icon = icon("check"), class = "btn-primary")
    ),
    
    # --- Main Panel for Outputs ---
    mainPanel(
      # Use tabsetPanel to create the required tabs
      tabsetPanel(
        id = "main_tabs",
        
        # --- Tab 1: About ---
        tabPanel("About",
                 h3("About This Application"),
                 p("This application allows you to interactively explore data about the public bicycle sharing system in Seoul, South Korea."),
                 h4("Data Source"),
                 p("The dataset used in this app is 'the Seoul Bike Sharing Demand Prediction' in Kaggle. This dataset is originally from 'Seoul Bike Sharing Demand Data Set' from the UCI Machine Learning Repository. It contains hourly counts of rented bikes from 2017 to 2018, along with corresponding weather data and holiday information."),
                 a(href="https://www.kaggle.com/datasets/saurabhshahane/seoul-bike-sharing-demand-prediction", "Click here for more data information.", target="_blank"),
                 
                 h4("The purpose of the sidebar and each tab"),
                 p(strong("Filtering Controls (Sidebar):"), " Use the dropdown menus and sliders on the left to subset the data. You must click the 'Apply Filters' button to update the visualizations and tables."),
                 p(strong("Data Download Tab:"), " View the filtered data in a table and download it as a CSV file."),
                 p(strong("Data Exploration Tab:"), " Generate custom plots and numerical summaries from the filtered data."),
                 img(src = "https://www.bikeseoul.com/img/main/main_visual.png",
                     alt = "Seoul Bike Main Visual",
                     style = "width: 100%; max-width: 600px; 
                              display: block; margin-left: auto; 
                              margin-right: auto; margin-bottom: 20px;
                              border-radius: 8px;")
        ),
        
        # --- Tab 2: Data Download ---
        tabPanel("Data Download",
                 h4("Filtered Data"),
                 # Download button
                 downloadButton("download_data", "Download Filtered Data (.csv)"),
                 hr(),
                 # Data table output
                 withSpinner(
                   dataTableOutput("data_table")
                 )
        ),
        
        # --- Tab 3: Data Exploration ---
        tabPanel("Data Exploration",
                 h3("Explore Summaries and Visualizations"),
                 tabsetPanel(
                   # --- Plotting Sub-Tab ---
                   tabPanel("Plots",
                            fluidRow(
                              column(4, 
                                     selectInput("plot_type", "Choose Plot Type:", 
                                                 choices = c("Scatter Plot", "Boxplot", "Line Plot (by Hour)", "Density Plot", "Correlation Heatmap"))
                              ),
                              # Dynamic UI for plot controls
                              uiOutput("plot_controls_ui")
                            ),
                            plotOutput("explore_plot") |> withSpinner(color="#0dc5c1")
                   ),
                   # --- Numerical Summaries Sub-Tab ---
                   tabPanel("Numerical Summaries",
                            selectInput("summary_type", "Choose Summary Type:", 
                                        choices = c("One-Way Contingency Table", "Two-Way Contingency Table", "Summary Statistics by Group")),
                            # UI that appears conditionally based on summary type
                            uiOutput("summary_controls_ui"),
                            h4("Summary Output"),
                            verbatimTextOutput("summary_output") |> withSpinner(color="#0dc5c1")
                   )
                 )
        )
      )
    )
  )
)


# 3. SERVER LOGIC
server <- function(input, output, session) {
  
  # --- Dynamic UI for Numeric Sliders ---
  output$num_slider1_ui <- renderUI({
    req(input$num_var1_select)
    var_data <- bike_data[[input$num_var1_select]]
    var_range <- range(var_data, na.rm = TRUE)
    sliderInput("num_slider1", 
                label = paste("Filter range for", input$num_var1_select),
                min = var_range[1], max = var_range[2], value = var_range)
  })
  
  output$num_slider2_ui <- renderUI({
    req(input$num_var2_select)
    var_data <- bike_data[[input$num_var2_select]]
    var_range <- range(var_data, na.rm = TRUE)
    sliderInput("num_slider2", 
                label = paste("Filter range for", input$num_var2_select),
                min = var_range[1], max = var_range[2], value = var_range)
  })
  
  # --- Reactive Data Subsetting ---
  # This reactive expression filters the data ONLY when the action button is clicked.
  filtered_data <- eventReactive(input$update_btn, {
    
    # Wait for the sliders to be created before trying to filter
    req(input$num_slider1, input$num_slider2) 
    
    data <- bike_data
    
    # Apply categorical filters (if "All" is not selected)
    if (input$season_select != "All") {
      data <- data |> filter(seasons == input$season_select)
    }
    if (input$holiday_select != "All") {
      data <- data |> filter(holiday == input$holiday_select)
    }
    
    # Apply numeric filters using the .data pronoun for dynamic variable names
    data <- data |>
      filter(between(.data[[input$num_var1_select]], input$num_slider1[1], input$num_slider1[2])) |>
      filter(between(.data[[input$num_var2_select]], input$num_slider2[1], input$num_slider2[2]))
    
    return(data)
    
  }, ignoreNULL = FALSE)
  
  # --- Data Download Tab Outputs ---
  
  # Render the data table
  output$data_table <- DT::renderDataTable({
    DT::datatable(filtered_data(), options = list(scrollX = TRUE, pageLength = 10))
  })
  
  # Handle the data download
  output$download_data <- downloadHandler(
    filename = function() {
      paste("filtered_bike_data-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(filtered_data(), file, row.names = FALSE)
    }
  )
  
  # --- Data Exploration Tab Outputs ---
  output$plot_controls_ui <- renderUI({
    req(input$plot_type)
    
    if (input$plot_type == "Scatter Plot") {
      # For plots like Plot 2 & 4
      tagList(
        column(4, selectInput("plot_x_var_num", "X-axis (Numeric):", choices = numeric_vars, selected = "temperature_c")),
        column(4, selectInput("plot_y_var_num", "Y-axis (Numeric):", choices = numeric_vars, selected = "rented_bike_count")),
        column(4, selectInput("plot_color_cat", "Color (Categorical):", choices = c("None", categorical_vars), selected = "seasons")),
        column(4, selectInput("plot_facet_cat", "Facet (Categorical):", choices = c("None", categorical_vars), selected = "None"))
      )
    } else if (input$plot_type == "Boxplot") {
      # For plots like Plot 3
      tagList(
        column(4, selectInput("plot_x_var_cat", "X-axis (Categorical):", choices = categorical_vars, selected = "hour")),
        column(4, selectInput("plot_y_var_num", "Y-axis (Numeric):", choices = numeric_vars, selected = "rented_bike_count")),
        column(4, selectInput("plot_fill_cat", "Fill (Categorical):", choices = c("None", categorical_vars), selected = "holiday"))
      )
    } else if (input$plot_type == "Line Plot (by Hour)") {
      # For plots like Plot 1
      tagList(
        column(4, selectInput("plot_y_var_num", "Y-axis (Numeric):", choices = numeric_vars, selected = "rented_bike_count")),
        column(4, selectInput("plot_color_cat", "Color (Categorical):", choices = c("None", categorical_vars), selected = "seasons"))
      )
    } else if (input$plot_type == "Density Plot") {
      # For plots like Plot 5
      tagList(
        column(4, selectInput("plot_x_var_num", "X-axis (Numeric):", choices = numeric_vars, selected = "humidity_percent")),
        column(4, selectInput("plot_fill_cat", "Fill (Categorical):", choices = c("None", categorical_vars), selected = "functioning_day"))
      )
    } else if (input$plot_type == "Correlation Heatmap") {
      # For plots like Plot 6 - No additional controls needed
      NULL
    }
  })
  
  # --- Plotting Sub-Tab (Plot Output) ---
  output$explore_plot <- renderPlot({
    data_to_plot <- filtered_data()
    req(input$plot_type)
    
    p <- NULL # Initialize plot object
    
    if (input$plot_type == "Scatter Plot") {
      req(input$plot_x_var_num, input$plot_y_var_num, input$plot_color_cat, input$plot_facet_cat)
      
      p <- ggplot(data_to_plot, aes(x = .data[[input$plot_x_var_num]], y = .data[[input$plot_y_var_num]])) +
        geom_point(alpha = 0.4) +
        labs(title = paste(input$plot_y_var_num, "vs.", input$plot_x_var_num),
             x = input$plot_x_var_num, y = input$plot_y_var_num)
      
      if (input$plot_color_cat != "None") {
        p <- p + aes(color = .data[[input$plot_color_cat]]) + labs(color = input$plot_color_cat)
      }
      if (input$plot_facet_cat != "None") {
        p <- p + facet_wrap(vars(.data[[input$plot_facet_cat]]))
      }
      
    } else if (input$plot_type == "Boxplot") {
      req(input$plot_x_var_cat, input$plot_y_var_num, input$plot_fill_cat)
      
      p <- ggplot(data_to_plot, aes(x = .data[[input$plot_x_var_cat]], y = .data[[input$plot_y_var_num]])) +
        geom_boxplot() +
        labs(title = paste("Distribution of", input$plot_y_var_num, "by", input$plot_x_var_cat),
             x = input$plot_x_var_cat, y = input$plot_y_var_num)
      
      if (input$plot_fill_cat != "None") {
        p <- p + aes(fill = .data[[input$plot_fill_cat]]) + labs(fill = input$plot_fill_cat)
      }
      
    } else if (input$plot_type == "Line Plot (by Hour)") {
      req(input$plot_y_var_num, input$plot_color_cat)
      
      # Summarize data just like in the static file
      plot_data_summarized <- data_to_plot |>
        mutate(hour_numeric = as.numeric(as.character(hour))) |>
        group_by(hour_numeric)
      
      if (input$plot_color_cat != "None") {
        plot_data_summarized <- plot_data_summarized |> group_by(.data[[input$plot_color_cat]], .add = TRUE)
      }
      
      plot_data_summarized <- plot_data_summarized |>
        summarise(avg_y = mean(.data[[input$plot_y_var_num]], na.rm = TRUE), .groups = "drop")
      
      p <- ggplot(plot_data_summarized, aes(x = hour_numeric, y = avg_y)) +
        geom_line(linewidth = 1) +
        labs(title = paste("Average", input$plot_y_var_num, "by Hour"),
             x = "Hour of Day", y = paste("Average", input$plot_y_var_num))
      
      if (input$plot_color_cat != "None") {
        p <- p + aes(color = .data[[input$plot_color_cat]], group = .data[[input$plot_color_cat]]) +
          labs(color = input$plot_color_cat)
      } else {
        p <- p + aes(group = 1) # Add group=1 for a single line
      }
      
    } else if (input$plot_type == "Density Plot") {
      req(input$plot_x_var_num, input$plot_fill_cat)
      
      p <- ggplot(data_to_plot, aes(x = .data[[input$plot_x_var_num]])) +
        geom_density(alpha = 0.5) +
        labs(title = paste("Density of", input$plot_x_var_num), x = input$plot_x_var_num)
      
      if (input$plot_fill_cat != "None") {
        p <- p + aes(fill = .data[[input$plot_fill_cat]]) + labs(fill = input$plot_fill_cat)
      }
      
    } else if (input$plot_type == "Correlation Heatmap") {
      # Prepare data for heatmap
      numeric_vars_filtered <- data_to_plot |> select(where(is.numeric))
      cor_matrix <- cor(numeric_vars_filtered, use = "complete.obs")
      cor_data <- as.data.frame(cor_matrix) |>
        rownames_to_column("var1") |>
        pivot_longer(-var1, names_to = "var2", values_to = "correlation")
      
      p <- ggplot(cor_data, aes(x = var1, y = var2, fill = correlation)) +
        geom_tile() +
        scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(-1, 1), na.value = "grey50") +
        labs(title = "Correlation Heatmap of Numeric Variables", x = "", y = "", fill = "Correlation") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8))
    }
    
    if (!is.null(p)) {
      print(p + theme_minimal()) # Apply theme and print
    }
    
  })
  
  # --- Numerical Summaries Sub-Tab ---
  output$summary_controls_ui <- renderUI({
    req(input$summary_type)
    
    if (input$summary_type == "One-Way Contingency Table") {
      selectInput("summary_var1", "Select Categorical Variable:", choices = categorical_vars)
      
    } else if (input$summary_type == "Two-Way Contingency Table") {
      tagList(
        selectInput("summary_var1", "First Categorical Variable:", choices = categorical_vars, selected = "seasons"),
        # Add "None" option
        selectInput("summary_var2", "Second Categorical Variable:", choices = c("None", categorical_vars), selected = "holiday")
      )
      
    } else if (input$summary_type == "Summary Statistics by Group") {
      tagList(
        selectInput("summary_group_var1", "First Grouping Variable:", choices = categorical_vars, selected = "seasons"),
        # Add "None" option
        selectInput("summary_group_var2", "Second Grouping Variable:", choices = c("None", categorical_vars), selected = "holiday"),
        selectInput("summary_numeric_var", "Numeric Variable to Summarize:", choices = numeric_vars, selected = "rented_bike_count")
      )
    }
  })
  
  # --- Numerical Summaries Sub-Tab (Output) ---
  output$summary_output <- renderPrint({
    data_to_summarize <- filtered_data()
    req(input$summary_type)
    
    if (input$summary_type == "One-Way Contingency Table") {
      req(input$summary_var1)
      print(table(data_to_summarize[[input$summary_var1]]))
      
    } else if (input$summary_type == "Two-Way Contingency Table") {
      req(input$summary_var1, input$summary_var2)
      
      if (input$summary_var2 == "None") {
        # If "None" is selected, just do a one-way table
        print(table(data_to_summarize[[input$summary_var1]]))
      } else {
        print(table(data_to_summarize[[input$summary_var1]], data_to_summarize[[input$summary_var2]]))
      }
      
    } else if (input$summary_type == "Summary Statistics by Group") {
      req(input$summary_group_var1, input$summary_numeric_var)
      
      grouped_data <- data_to_summarize
      
      # Add first group
      grouped_data <- grouped_data |> group_by(.data[[input$summary_group_var1]])
      
      # Add second group if selected
      if (input$summary_group_var2 != "None") {
        grouped_data <- grouped_data |> group_by(.data[[input$summary_group_var2]], .add = TRUE)
      }
      
      # Summarize
      grouped_data |>
        summarise(
          count = n(),
          mean = mean(.data[[input$summary_numeric_var]], na.rm = TRUE),
          sd = sd(.data[[input$summary_numeric_var]], na.rm = TRUE),
          min = min(.data[[input$summary_numeric_var]], na.rm = TRUE),
          median = median(.data[[input$summary_numeric_var]], na.rm = TRUE),
          max = max(.data[[input$summary_numeric_var]], na.rm = TRUE),
          .groups = "drop"
        ) |>
        print()
    }
  })
}

# 4. RUN THE APPLICATION
shinyApp(ui = ui, server = server)