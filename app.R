# 1. SETUP
library(shiny)
library(tidyverse)
library(lubridate)
library(DT)
library(shinycssloaders)
library(janitor)

# Load and prepare the dataset
# NOTE: Assumes "SeoulBikeData.csv" is in a folder named "data"
# (./data/SeoulBikeData.csv)
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
                 p("Create plots and tables from the filtered data."),
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
}

# 4. RUN THE APPLICATION
shinyApp(ui = ui, server = server)