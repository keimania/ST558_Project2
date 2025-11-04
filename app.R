# 1. SETUP

library(shiny)
library(tidyverse)
library(lubridate)
library(DT)
library(shinycssloaders)
library(janitor)

# Load and prepare the dataset

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
categorical_vars <- c("seasons", "holiday", "functioning_day")
numeric_vars <- bike_data |> select(where(is.numeric)) |> names()


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
                 shinycssloaders::withSpinner(
                   DT::dataTableOutput("data_table")
                 )
        ),
        
        # --- Tab 3: Data Exploration ---
        # --- Data Exploration Tab ---
        tabPanel("Data Exploration"
        )
      )
    )
  )
)


# 3. SERVER LOGIC
server <- function(input, output, session) {
}


# 4. RUN THE APPLICATION
shinyApp(ui = ui, server = server)