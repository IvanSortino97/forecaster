# app/view/

box::use(
  shiny[... , div, moduleServer, NS, tags, observeEvent, observe],
  bslib[page_fillable, card, card_header, card_body, card_title],
  shinyjs[hide],
  shinybrowser[is_device_mobile],
  shiny.router[is_page],
  shinytoastr[toastr_info, toastr_clear_all],
  rugarch[ugarchroll],
  echarts4r[renderEcharts4r],
  reactable[renderReactable],
)
box::use(
  app / logic / general_utils[in_card_subtitle_style, conditional_page_fillable, make_spinner, show_condition],
  app / logic / garchFit_utils[models],
  app / logic / garchBacktest_utils[...]
)

#' @export
ui <- function(id) {
  ns <- NS(id)

  conditional_page_fillable(ns = ns,

                            title = "GARCH Model backtesting",
                            subtitle = "Configure window size and backtesting methods to evaluate model performance.",
                            condition_page = "garchFit",
                            body = div(
                                tagList(lapply(models, function(x) conditionalBacktestCard(ns = ns, x)))
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
    backtestResults <- reactiveValues()
    once <- reactiveVal(T)

    #general observe to fetch stockInfo
    observeEvent(stockInfo()$returns(),{
      req(stockInfo()$returns())
      totObs <- length(stockInfo()$returns())
      outSample <- floor(totObs/3)
      inSample <- totObs - outSample

      # If ticker changes, reset models and backtest again
      previousParams <<- reactiveValues()
      backtestResults <<- reactiveValues()

      # update sliders and info
      lapply(models, function(x){
        updateSliderInput(inputId = paste0(x,"forecastLength"), max = totObs-1, value = outSample)
        obs <- if(is_device_mobile()) "obs." else "observations"

        output[[paste0(x,"sliderInfo")]] <- renderUI({
          slider_info(totObs, inSample, obs)
        })
      })
    })


    observe({
      req(garchFit()$fitResults(), is_page("garchBacktest"))
      spinner$show()

      lapply(garchFit()$selectedModels(), function(x){

        # get current param
        currentParams <- list(
          spec = garchFit()$fitResults()[[x]]$spec,
          forecast.length = input[[paste0(x,"forecastLength")]],
          refit.every = input[[paste0(x,"refitEvery")]],
          refit.window = input[[paste0(x,"refitWindow")]],
          solver = input[[paste0(x,"solver")]]
        )

        # Initialize previousParams if it's not already set for the current model
        if (is.null(previousParams[[x]])) {
          previousParams[[x]] <- list(
            spec = NULL,
            forecast.length = NULL,
            refit.every = NULL,
            refit.window = NULL,
            solver = NULL
          )
        }

        if (!identical(currentParams, previousParams[[x]])) {
          btSpinners[[x]]$show()
          if(once()){
            toastr_info(
              title = "Please wait",
              message = sprintf("Backetsting %s model", x),
              position = "top-center",
              timeOut = 0,
              hideDuration = 1800
            )
          }

          backtestResults[[x]] <- ugarchroll(spec = garchFit()$fitResults()[[x]]$spec,
                                             data = stockInfo()$returns(),
                                             forecast.length = input[[paste0(x, "forecastLength")]],
                                             refit.every = input[[paste0(x, "refitEvery")]],
                                             refit.window = input[[paste0(x, "refitWindow")]],
                                             solver = input[[paste0(x, "solver")]] )

        previousParams[[x]] <- currentParams
        print(paste0(stockInfo()$ticker(), " - Backtested model: ", x))

        output[[paste0(x,"backtestPlot")]] <- renderEcharts4r(make_BacktestPlot(backtestResults[[x]],
                                                                                ticker = stockInfo()$ticker(),
                                                                                model = x))

        output[[paste0(x,"tableInfo")]] <- renderReactable(make_report(backtestResults[[x]], info = T))
        output[[paste0(x,"tableExceed")]] <- renderReactable(make_report(backtestResults[[x]], type = "exceed"))
        output[[paste0(x,"tableUc")]] <- renderReactable(make_report(backtestResults[[x]], type = "uc"))
        output[[paste0(x,"tableCc")]] <- renderReactable(make_report(backtestResults[[x]], type = "cc"))

        toastr_clear_all(TRUE)
        btSpinners[[x]]$hide()
        }
      })

      #once(F) # uncomment to notify only first backtesting
      spinner$hide()

    })

  })
}
