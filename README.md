# ST558, Jamin Goo, Project2

# Seoul Bike Sharing Data Explorer

This is a Shiny application built for a class project (Project 2). It allows users to interactively filter and explore the **Seoul Bike Sharing Demand** dataset.

The project is composed of two main parts: a static data exploration report (`Project2.qmd`) and the interactive Shiny application (`app.R`) that is based on it.

## Project Components

-   **`Project2.qmd` (Static Exploration):** A Quarto file containing the preparatory "static" analysis of the bike sharing data. This report includes:
    -   Data loading, cleaning, and preparation.
    -   **Numerical Summaries:** One-way contingency tables, two-way contingency tables, and grouped summary statistics (mean, median, sd) for all numeric variables.
    -   **Graphical Summaries:** Six distinct plots to visualize data patterns (Average Rentals by Hour, Rentals vs. Temperature, Rental Distribution by Holiday, Rentals vs. Wind Speed, Humidity Density, and a Correlation Heatmap).
-   **`app.R` (Interactive App):** The final interactive Shiny application. The features in the app, particularly the "Data Exploration" tab, are based directly on the analyses and plots developed in the `Project2.qmd` file, allowing users to recreate them on a dynamically filtered dataset.

## 🚀 App Features

-   **Interactive Filtering:** Filter the dataset by:
    -   Season
    -   Holiday Status
    -   A custom range for two selected numeric variables (e.g., Temperature, Humidity).
-   **Apply Button:** All filters are applied only when the "Apply Filters" button is clicked.
-   **About Tab:** Provides a description of the app, its purpose, and a link to the original data source on Kaggle.
-   **Data Download Tab:**
    -   View the filtered data in an interactive `DT` table.
    -   Download the currently filtered data as a `.csv` file.
-   **Data Exploration Tab:**
    -   **Dynamic Plots:** A powerful plotting interface where users can select the plot type (Scatter, Boxplot, Line, Density, Heatmap) and dynamically map variables to axes, colors, and facets, replicating all 6 plots from the static report.
    -   **Dynamic Summaries:** A tool to generate numerical summaries, including one-way/two-way contingency tables and detailed summary statistics grouped by one or two categorical variables.
