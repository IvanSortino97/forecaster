# Forecaster

A comprehensive financial analysis application for stock market data analysis and volatility forecasting.

## Table of Contents

- [Features](#features)
- [Tech Stack](#tech-stack)
- [Installation and Setup](#installation-and-setup)
- [Running the Application](#running-the-application)
- [Project Structure](#project-structure)
- [Contributing](#contributing)
- [License](#license)

## Features

-   **Real-time Stock Data**: Fetches and displays up-to-date stock market data.
-   **SP500 Filtering**: Easily filter and search for stocks within the S&P 500 index.
-   **Company Insights**: Access detailed company information and key market metrics.
-   **Advanced Statistical Analysis**: Conduct in-depth statistical tests and analyses.
-   **GARCH Modeling**: Fit, backtest, and forecast volatility using GARCH models.
-   **Automated Model Optimization**: Automatically find the best model parameters.
-   **Interactive Visualizations**: Explore data with interactive financial charts.
-   **Performance Metrics**: Review a comprehensive set of stock performance metrics.
-   **Multi-Model Comparison**: Compare the performance and forecasts of different models.

## Tech Stack

This project is built with a modern R and Shiny stack, leveraging the `rhino` framework for robustness and scalability.

-   **Backend**: R, Shiny
-   **Framework**: Rhino
-   **Dependency Management**: renv
-   **Frontend**: HTML, SCSS, JavaScript
-   **Core Packages**:
    -   `shiny`: Web application framework
    -   `rhino`: Application structure and tooling
    -   `echarts4r`, `ggplot2`: Interactive charting
    -   `quantmod`, `TTR`: Quantitative financial modeling and analysis
    -   `rugarch`: GARCH model implementation
    -   `prophet`: Time series forecasting
    -   `data.table`, `dplyr`: Data manipulation
    -   `bslib`, `bsicons`: Custom Bootstrap themes and icons

## Installation and Setup

Follow these steps to set up the project locally.

1.  **Clone the repository:**
    ```bash
    git clone https://github.com/IvanSortino97/forecaster.git
    cd forecaster
    ```

2.  **Set up the R Environment:**
    This project uses `renv` to manage dependencies. When you open the project in RStudio, `renv` should automatically start restoring the environment.

    If it doesn't, or if you are using a different IDE, run the following command in the R console to restore the project's dependencies from the `renv.lock` file:
    ```r
    renv::restore()
    ```

## Running the Application

To run the Shiny application, execute the following command in the R console from the project's root directory:

```r
shiny::runApp()
```

Alternatively, if you are using RStudio, you can click the "Run App" button in the editor toolbar when `app.R` is open.

## Project Structure

The project follows the `rhino` framework structure, which organizes the Shiny application into logical components:

```
forecaster/
├── app/
│   ├── js/         # Frontend JavaScript
│   ├── logic/      # Business logic (R modules)
│   ├── static/     # Static assets (images, CSS)
│   ├── styles/     # SCSS stylesheets
│   └── view/       # Shiny UI modules
├── tests/          # Unit and end-to-end tests
├── app.R           # Main application entry point
├── dependencies.R  # Project dependencies
├── renv.lock       # R environment lockfile
└── rhino.yml       # Rhino configuration
```

-   `app/logic`: Contains the core business logic of the application, such as data retrieval, statistical modeling, and forecasting.
-   `app/view`: Holds the Shiny UI modules that define the application's user interface.
-   `app.R`: The main entry point that launches the Shiny application.
-   `dependencies.R`: Declares all required R packages for the project.
-   `renv.lock`: Records the exact versions of all R packages used, ensuring reproducibility.

## Contributing

Contributions are welcome! If you have suggestions for improvements or want to report a bug, please open an issue.

## License

This project is open source.
