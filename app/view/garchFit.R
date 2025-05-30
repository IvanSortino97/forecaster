# app/view/

box::use(
  shiny[updateNumericInput,updateSelectizeInput, reactiveValues , tags, tagList , reactiveValuesToList, renderPlot, renderPrint, observeEvent, observe, req, reactive, reactiveVal, div, moduleServer, NS],
  bslib[page_fillable, card, card_header, card_body, card_title],
  shinyjs[show, hide, disable, enable],
  reactable[reactable, renderReactable, colDef],
  shiny.router[is_page],
  shinybrowser[is_device_mobile],
  spsComps[addLoader],
)
box::use(
  app / logic / general_utils[page_footer, in_card_subtitle_style, conditional_page_fillable, make_spinner, show_condition],
  app / logic / garchFit_utils[...]
)


#' @export
ui <- function(id) {
  ns <- NS(id)

  conditional_page_fillable(ns = ns,
                            title = "Fit Models",
                            subtitle = "Specify parameters to fit models. Auto mode provide the best parameter minimizin AIC statistic",
                            body = div(
                              card(
                                tags$h5("Select model to fit", style = paste0(in_card_subtitle_style, "font-size: 1rem;")),
                                model_checkbox(ns("modelCheckbox"))
                              ),
                              card_title("Specify parameters and check results"),
                              tagList(lapply(models, function(x) conditional_model(ns = ns,
                                                                                   model = x))),
                              page_footer(hrefPageNext = "garchBacktest",
                                          textPageNext = "Backtest Model",
                                          hrefPagePrecedent = "prophetForecast",
                                          textPagePrecedent = "Prophet Model")
                            )
  )
}

#' @export
server <- function(id, stockInfo) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    spinner <- make_spinner("titleLoader")

    fitSpinners <- lapply(models, function(x) make_spinner(make_id(x, "loader")))
    names(fitSpinners) <- models

    hide("conditionalPanel")
    observeEvent(stockInfo()$data_xts(), {
      show_condition(stockInfo()$data_xts())

      # If ticker changes, reset models and refit
      toComputeAuto <<- reactiveValues()
      previousParams <<- reactiveValues()
      fitResults <<- reactiveValues()
    })

    toComputeAuto <- reactiveValues()
    previousParams <- reactiveValues()
    fitResults <- reactiveValues()

    observe({
      req(stockInfo()$returns(), is_page("garchFit"))


      # Iterate over the models in the checkbox input
      lapply(input$modelCheckbox, function(x) {

        idSwitch <- make_id(x, "switch")
        idDist <- make_id(x, "dist")
        idP <- make_id(x, "p")
        idQ <- make_id(x, "q")
        idAR <- make_id(x, "ar")
        idMA <- make_id(x, "ma")
        idMean <- make_id(x, "includeMean")
        idAutoTable <- make_id(x, "autoTable")

        # Get current parameters
        currentParams <- list(
          dist = input[[idDist]],
          submodel = input[[make_id(x,"submodel")]],
          p = input[[idP]],
          q = input[[idQ]],
          ar = input[[idAR]],
          ma = input[[idMA]],
          ma = input[[idMean]]
        )

        # Initialize previousParams if it's not already set for the current model
        if (is.null(previousParams[[x]])) {
          previousParams[[x]] <- list(
            dist = NULL,
            p = NULL,
            q = NULL,
            ar = NULL,
            ma = NULL,
            mean = NULL
          )
        }

        # Check if the switch is TRUE and the computation hasn't been done yet
        if (input[[idSwitch]] && is.null(toComputeAuto[[x]])) {

          fitSpinners[[x]]$show()

          best_fit <- get_best_fit(model = x,
                                   returns = stockInfo()$returns(),
                                   input = input,
                                   garchRange = input[[make_id(x,"garchRange")]],
                                   armaRange = input[[make_id(x,"armaRange")]])
          criteria <- "AIC"
          param <- get_param(best_fit, criteria)

          updateSelectizeInput(inputId = idDist, selected = param$dist)
          updateNumericInput(inputId = idP, value = param$p)
          updateNumericInput(inputId = idQ, value = param$q)
          updateNumericInput(inputId = idAR, value = param$ar)
          updateNumericInput(inputId = idMA, value = param$ma)

          disable(idDist)
          disable(idP)
          disable(idQ)
          disable(idAR)
          disable(idMA)

          # Results in Table
          output[[idAutoTable]] <- renderReactable({
            make_autoTable(best_fit$results, param$index, criteria)
          })

          toComputeAuto[[x]] <- TRUE
          currentParams <- list(
            dist = input[[idDist]],
            p = input[[idP]],
            q = input[[idQ]],
            ar = input[[idAR]],
            ma = input[[idMA]],
            mean = input[[idMean]]
          )

        } else if (!input[[idSwitch]]) {
          # If the switch is off, re-enable the inputs and allow computation again
          enable(idDist)
          enable(idP)
          enable(idQ)
          enable(idAR)
          enable(idMA)

          # Allow the computation to be triggered again if the switch is turned back on
          toComputeAuto[[x]] <- NULL
        }

        # Fit the model only if the parameters have changed
        if (!identical(currentParams, previousParams[[x]])) {

          fitSpinners[[x]]$show()

          # Fit the model with the current parameters
          fitResults[[x]] <- fit_garch(model = x,
                                       p = not_null(input[[idP]], 1),
                                       q = not_null(input[[idQ]], 1),
                                       ar = not_null(input[[idAR]]),
                                       ma = not_null(input[[idMA]]),
                                       dist = input[[idDist]],
                                       data = stockInfo()$returns(),
                                       mean = input[[idMean]],
                                       info = FALSE,
                                       submodel = if(x == "FIGARCH") input[[make_id(x,"submodel")]]
                                       )

          previousParams[[x]] <- currentParams
          print(paste0(stockInfo()$ticker(), " - fitted model: ", x))
        }

        req(fitResults[[x]]$fit)

        output[[make_id(x,"cvdTable")]] <- renderReactable(makeCvdTable(fitResults[[x]]$fit))
        output[[make_id(x,"opTable")]] <- renderReactable(makeOpRseTable(fitResults[[x]]$fit, "op"))
        output[[make_id(x,"rseTable")]] <- renderReactable(makeOpRseTable(fitResults[[x]]$fit, "rse"))
        output[[make_id(x,"results")]] <- renderPrint(fitResults[[x]]$fit)
        output[[make_id(x,"fitPlot")]] <- renderPlot(makeFitPlot(fitResults[[x]]$fit, input[[make_id(x, "selectPlot")]]),
                                                     height = if(is_device_mobile()) 200 else 400)

        fitSpinners[[x]]$hide()

        }) # end lapply

      fitOutput(reactiveValuesToList(fitResults))

    }) # end observe

    fitOutput <- reactiveVal()

    return(
      list(
        selectedModels = reactive(input$modelCheckbox),
        fitResults = reactive(fitOutput())
      )
    )

  })
}
