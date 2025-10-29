# 1. SETUP

library(shiny)
library(tidyverse)
library(lubridate)
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
    )
  )
)


# 3. SERVER LOGIC
server <- function(input, output, session) {
}


# 4. RUN THE APPLICATION
shinyApp(ui = ui, server = server)