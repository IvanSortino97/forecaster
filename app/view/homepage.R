# app/view/

box::use(
  shiny[div, moduleServer, NS, tagList, tags, hr],
  shiny.router[route_link],
  bslib[card,card_header,card_body,card_title, card_footer],
)
box::use(
  app / logic / general_utils[...],
)


#' @export
ui <- function(id) {
  ns <- NS(id)

  tagList(
    # App header section
    div(class = "jumbotron text-center",
        tags$h1("Forecaster", class = "display-4"),
        tags$p("Financial Time Series Analysis & Forecasting Tool", class = "lead"),
        hr(class = "my-4")
    ),
    
    # App description and introduction section
    div(class = "container mb-5",
        tags$h2("Introduction", class = "mb-3"),
        tags$p("Forecaster is a comprehensive tool for analyzing stock market data, 
               fitting various time series models, and generating forecasts for financial time series."),
        tags$p("This application allows you to:"),
        tags$ul(
          tags$li("Select and visualize stock data from various markets"),
          tags$li("Perform detailed statistical analysis on financial time series"),
          tags$li("Fit different forecasting models including GARCH and Prophet"),
          tags$li("Backtest models to evaluate forecasting performance"),
          tags$li("Generate forecasts for future stock prices")
        ),
        hr(class = "my-4")
    ),
    
    # Navigation sections
    div(class = "container",
        
        # Info Section
        tags$h3("Data Selection and Analysis", class = "mb-3 text-secondary"),
        div(class = "row mb-4",
            div(class = "col-md-6",
                card(
                    full_screen = FALSE,
                    height = "300px",
                    card_header(
                        "Stock Selection",
                        class = "bg-secondary text-white"
                    ),
                    card_body(
                        "Browse and select stocks for analysis"
                    ),
                    card_footer(
                        tags$a(href = route_link("stockInfo"),
                              class = "btn btn-outline-secondary w-100",
                              "Go to Stock Selection")
                    )
                )
            ),
            div(class = "col-md-6",
                card(
                    full_screen = FALSE,
                    height = "300px",
                    card_header(
                        "Stock Analysis",
                        class = "bg-secondary text-white"
                    ),
                    card_body(
                        "Perform detailed statistical analysis on selected stocks"
                    ),
                    card_footer(
                        tags$a(href = route_link("stockAnalysis"),
                              class = "btn btn-outline-secondary w-100",
                              "Go to Stock Analysis")
                    )
                )
            )
        ),
        
        # Prophet Section
        tags$h3("Prophet Modeling", class = "mb-3 text-primary"),
        div(class = "row mb-4",
            div(class = "col-md-12",
                card(
                    full_screen = FALSE,
                    height = "300px",
                    card_header(
                        "Prophet Model",
                        class = "bg-primary text-white"
                    ),
                    card_body(
                        "Fit Prophet models to forecast stock prices"
                    ),
                    card_footer(
                        tags$a(href = route_link("prophetForecast"),
                              class = "btn btn-outline-primary w-100",
                              "Go to Prophet Model")
                    )
                )
            )
        ),
        
        # GARCH Section
        tags$h3("GARCH Analysis", class = "mb-3 text-warning"),
        div(class = "row mb-4",
            div(class = "col-md-4",
                card(
                    full_screen = FALSE,
                    height = "300px",
                    card_header(
                        "Fit GARCH Model",
                        class = "bg-warning"
                    ),
                    card_body(
                        "Fit GARCH models to capture volatility patterns"
                    ),
                    card_footer(
                        tags$a(href = route_link("garchFit"),
                              class = "btn btn-outline-warning w-100",
                              "Go to GARCH Fitting")
                    )
                )
            ),
            div(class = "col-md-4",
                card(
                    full_screen = FALSE,
                    height = "300px",
                    card_header(
                        "Backtest GARCH",
                        class = "bg-warning"
                    ),
                    card_body(
                        "Evaluate GARCH model performance through backtesting"
                    ),
                    card_footer(
                        tags$a(href = route_link("garchBacktest"),
                              class = "btn btn-outline-warning w-100",
                              "Go to GARCH Backtesting")
                    )
                )
            ),
            div(class = "col-md-4",
                card(
                    full_screen = FALSE,
                    height = "300px",
                    card_header(
                        "GARCH Forecast",
                        class = "bg-warning"
                    ),
                    card_body(
                        "Generate forecasts using fitted GARCH models"
                    ),
                    card_footer(
                        tags$a(href = route_link("garchForecast"),
                              class = "btn btn-outline-warning w-100",
                              "Go to GARCH Forecast")
                    )
                )
            )
        ),
        
        # ARIMA Section
        tags$h3("ARIMA Analysis", class = "mb-3 text-secondary"),
        div(class = "row mb-4",
            div(class = "col-12",
                card(
                    full_screen = FALSE,
                    height = "200px",
                    card_header(
                        "ARIMA Section",
                        class = "bg-secondary text-white"
                    ),
                    card_body(
                        "ARIMA (AutoRegressive Integrated Moving Average) models are used for time series forecasting.",
                        div(class = "alert alert-info mt-3",
                            tags$i(class = "fa fa-info-circle mr-2"),
                            "Work in progress - ARIMA modeling capabilities will be available in a future update."
                        )
                    )
                )
            )
        )
    )
  )
}



#' @export
server <- function(id, ...) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns


  })
}
