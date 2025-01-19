# app/view/

box::use(
  shiny[div, moduleServer, NS, tags, observeEvent, tagList, observe, req, reactiveValues, reactiveVal],
  shiny.router[is_page],
  bslib[page_fillable, card, card_header, card_body, card_title],
  rugarch[ugarchforecast, sigma, fitted],
  echarts4r[renderEcharts4r],
  quantmod[Cl],
  zoo[index],
  shinytoastr[toastr_info, toastr_clear_all],
  shinyjs[hide],
)
box::use(
  app / logic / general_utils[conditional_page_fillable, make_spinner, show_condition],
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
                              tagList(lapply(models, function(x) conditionalForecastCard(ns = ns, x)))
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
    once <- reactiveVal(T)

    #general observe to fetch stockInfo
    observeEvent(stockInfo()$returns(), {

      # If ticker changes, reset models and backtest again
      previousParams <<- reactiveValues()
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
              message = sprintf("Forcasting %s model", x),
              position = "top-center",
              timeOut = 0,
              hideDuration = 1800
            )

          forecastResults[[x]] <- ugarchforecast(garchFit()$fitResults()[[x]]$fit,
                                                 n.ahead = input[[paste0(x, "nahead")]])


          previousParams[[x]] <- currentParams
          print(paste0(stockInfo()$ticker(), " - Forwarded model: ", x))


          forecasted_sd <- sigma(forecastResults[[x]])  # Forecasted volatility
          forecasted_mean <- fitted(forecastResults[[x]])  # Forecasted mean returns

          # Extract the forecasted returns (mean)
          forecast_values <- as.numeric(fitted(forecastResults[[x]]))

          # Create a data frame for plotting
          n_obs <- length(stockInfo()$returns())  # Number of observations

          # NOT WORKING

forecast_df <- data.table(
  Time = seq_along(forecasted_mean),  # Time index for forecast
  Mean = forecasted_mean,            # Forecasted conditional mean
  Upper95 = forecasted_mean + 1.96 * forecasted_sd,  # Upper 95% CI
  Lower95 = forecasted_mean - 1.96 * forecasted_sd,   # Lower 95% CI
  Upper99 = forecasted_mean + 2.576 * forecasted_sd,  # Upper 99% CI
  Lower99 = forecasted_mean - 2.576 * forecasted_sd   # Lower 99% CI
)
          browser()

# Create the plot output
output[[paste0(x,"forecastPlot")]] <- renderEcharts4r({
  plot(forecast_dates, forecast_values,
       type = "l",
       col = "blue",
       xlab = "Date",
       ylab = "Forecasted Returns",
       main = paste("GARCH Forecast for", stockInfo()$ticker()))
  
  # Add confidence intervals using forecast_sigma
  lines(forecast_dates, forecast_values + 1.96 * forecast_sigma, lty = 2, col = "red")
  lines(forecast_dates, forecast_values - 1.96 * forecast_sigma, lty = 2, col = "red")
  
  legend("topright", 
         legend = c("Forecast", "95% Confidence Interval"),
         col = c("blue", "red"),
         lty = c(1, 2))
})


          # forecast_df <- data.frame(
          #   Date = index(Cl( stockInfo()$data_xts()) )[(n_obs + 1):(n_obs + length(forecast_values))],
          #   Forecast = forecast_values
          # )

          # Plot using echarts4r
          # forecast_df |>
          #   e_charts(Date) |>
          #   e_line(Forecast, name = "Forecasted Returns") |>
          #   e_title("Forecasted Returns with GARCH") |>
          #   e_tooltip(trigger = "axis") |>
          #   e_x_axis(type = "category") |>
          #   e_y_axis(name = "Forecasted Returns")




          # output[[paste0(x,"forecastPlot")]] <- renderEcharts4r(make_BacktestPlot(backtestResults[[x]],
          #                                                                         ticker = stockInfo()$ticker(),
          #                                                                         model = x))

          # output[[paste0(x,"tableInfo")]] <- renderReactable(make_report(backtestResults[[x]], info = T))
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
