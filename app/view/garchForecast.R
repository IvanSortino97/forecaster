# app/view/

box::use(
  shiny[div, moduleServer, NS, tags, observeEvent, tagList, observe, req, reactiveValues, reactiveVal],
  shiny.router[is_page],
  bslib[page_fillable, card, card_header, card_body, card_title],
  data.table[rbindlist],
  utils[tail],
  rugarch[ugarchforecast, sigma, fitted],
  reactable[renderReactable],
  echarts4r[renderEcharts4r],
  shinytoastr[toastr_info, toastr_clear_all],
  shinyjs[hide],
)
box::use(
  app / logic / general_utils[conditional_page_fillable, make_spinner, show_condition, page_footer],
  app / logic / garchFit_utils[models],
  app / logic / garchForecast_utils[...],
)

#' @export
ui <- function(id) {
  ns <- NS(id)

  conditional_page_fillable(ns = ns,

                            title = "GARCH Model forecast",
                            subtitle = "Subtitle",
                            condition_page = "garchFit",
                            body = div(
                              tagList(lapply(models, function(x) conditionalForecastCard(ns = ns, x))),

                                                                      page_footer(
                                          hrefPageNext = "arimaFit",
                                          textPageNext = "Arima Model",
                                          hrefPagePrecedent = "garchBacktest",
                                          textPagePrecedent = "Backtest Model"
                                        )
                            )
  )
}

#' @export
server <- function(id, stockInfo, garchFit) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    spinner <- make_spinner("titleLoader")

    btSpinners <- lapply(models, function(x) make_spinner(paste0(x, "loader")))
    names(btSpinners) <- models

    hide("conditionalPanel")
    observeEvent(garchFit()$fitResults(),{
      show_condition(garchFit()$fitResults())
    })

    previousParams <- reactiveValues()
    forecastResults <- reactiveValues()
    forecastDf <- reactiveValues()

    once <- reactiveVal(T)

    #general observe to fetch stockInfo
    observeEvent(stockInfo()$returns(), {

      # If ticker changes, reset models and backtest again
      previousParams <<- reactiveValues()
      forecastResults <<- reactiveValues()
      forecastResults <<- reactiveValues()

    })

    observe({
      req(garchFit()$fitResults(), is_page("garchForecast"))
      spinner$show()

      lapply(garchFit()$selectedModels(), function(x){

        # get current param
        currentParams <- list(
          spec = garchFit()$fitResults()[[x]]$spec,
          n.ahead = input[[paste0(x,"nahead")]]
        )

        # Initialize previousParams if it's not already set for the current model
        if (is.null(previousParams[[x]])) {
          previousParams[[x]] <- list(
            spec = NULL,
            n.ahead = NULL
          )
        }

        if (!identical(currentParams, previousParams[[x]])) {
          btSpinners[[x]]$show()

            toastr_info(
              title = "Please wait",
              message = sprintf("Forecasting %s model", x),
              position = "top-center",
              timeOut = 0,
              hideDuration = 1800
            )

          forecastResults[[x]] <- ugarchforecast(garchFit()$fitResults()[[x]]$fit,
                                                 n.ahead = input[[paste0(x, "nahead")]])

          previousParams[[x]] <- currentParams
          print(paste0(stockInfo()$ticker(), " - Forwarded model: ", x))

                  # Forecast table
                  forecastDf[[x]] <- create_forecast_df(
                    fit = garchFit()$fitResults()[[x]]$fit,
                    returns = stockInfo()$returns(),
                    forecasted_sd = as.numeric(sigma(forecastResults[[x]])), # Forecasted volatility
                    n_ahead = input[[paste0(x,"nahead")]]
                  )
          
                  # Render Plot
                  output[[paste0(x,"forecastPlot")]] <- renderEcharts4r({
                    make_forecast_plot(forecastDf[[x]], 
                    x,
                    input[[paste0(x,"nahead")]])
                  })


          output[[paste0(x,"forecastRaw")]] <- renderReactable(make_raw_table(forecastDf[[x]]))
          # output[[paste0(x,"tableExceed")]] <- renderReactable(make_report(backtestResults[[x]], type = "exceed"))
          # output[[paste0(x,"tableUc")]] <- renderReactable(make_report(backtestResults[[x]], type = "uc"))
          # output[[paste0(x,"tableCc")]] <- renderReactable(make_report(backtestResults[[x]], type = "cc"))

          toastr_clear_all(TRUE)
          btSpinners[[x]]$hide()
        }
      })

      #once(F) # uncomment to notify only first backtesting
      spinner$hide()

    })

  })
}
