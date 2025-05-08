# app/view/

box::use(
  shiny[div, moduleServer, NS, tagList, tags, hr],
  shiny.router[route_link]
)
box::use(
  app / logic / general_utils[...],
)

page_links <- list(
  list(title = "Stock Selection", page = "stockInfo"),
  list(title = "Stock Analysis", page = "stockAnalysis"),
  list(title = "Prophet Forecast", page = "prophetForecast"),
  list(title = "GARCH Fit model", page = "garchFit"),
  list(title = "GARCH backtest model", page = "garchBacktest"),
  list(title = "GARCH Forecast", page = "garchForeecast")

)

# Helper function to get descriptions for each section
get_section_description <- function(page) {
  switch(page,
         "stockInfo" = "Browse and select stocks for analysis",
         "stockAnalysis" = "Perform detailed statistical analysis on selected stocks",
         "garchFit" = "Fit GARCH models to capture volatility patterns",
         "garchBacktest" = "Evaluate model performance through backtesting",
         "Unknown section")
}

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
               fitting GARCH models, and generating forecasts for financial time series."),
        tags$p("This application allows you to:"),
        tags$ul(
          tags$li("Select and visualize stock data from various markets"),
          tags$li("Perform detailed statistical analysis on financial time series"),
          tags$li("Fit GARCH models to capture volatility patterns"),
          tags$li("Backtest models to evaluate forecasting performance"),
          tags$li("Generate forecasts for future stock prices")
        )
    ),
    
    # Quick navigation section
    div(class = "container",
        tags$h2("Quick Navigation", class = "mb-3"),
        div(class = "row",
            lapply(page_links, function(link) {
              div(class = "col-md-6 col-lg-3 mb-4",
                  div(class = "card h-100",
                      div(class = "card-body text-center",
                          tags$h5(class = "card-title", link$title),
                          tags$p(class = "card-text", get_section_description(link$page)),
                          tags$a(href = route_link(link$page),
                                 class = "btn btn-primary",
                                 "Go to section")
                      )
                  )
              )
            })
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
