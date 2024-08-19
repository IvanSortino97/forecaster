# app/view/

box::use(
  shiny[..., conditionalPanel, observeEvent, reactiveVal, checkboxGroupInput, div, moduleServer, NS],
  bslib[page_fillable, card, card_header, card_body, card_title],
  shinyjs[show, hide, disable, enable],
  reactable[reactable, renderReactable, colDef],
  shiny.router[is_page],
)
box::use(
  app / logic / general_utils[in_card_subtitle_style, conditional_page_fillable, make_spinner, select_stock_condition],
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
                                tags$h5("Select model to fit", style = in_card_subtitle_style),
                                model_checkbox(ns("modelCheckbox"))
                              ),
                              card_title("Specify parameters"),
                              conditional_model(ns = ns,
                                                model = "GARCH"
                              ),
                              conditional_model(ns = ns,
                                                model = "eGARCH"
                              ),
                              conditional_model(ns = ns,
                                                model = "GJR - GARCH"
                              ),
                              conditional_model(ns = ns,
                                                model = "APARCH"
                              ),
                              conditional_model(ns = ns,
                                                model = "IGARCH"
                              ),
                              conditional_model(ns = ns,
                                                model = "FIGARCH"
                              )
                            )
  )
}

#' @export
server <- function(id, stockInfo) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    spinner <- make_spinner("titleLoader")

    hide("conditionalPanel")
    observeEvent(stockInfo()$data_xts(), {
      select_stock_condition(stockInfo()$data_xts())

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

          best_fit <- get_best_fit(model = x, stockInfo()$returns())
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

          # Fit the model with the current parameters
          fitResults[[x]] <- fit_garch(model = x,
                                       p = not_null(input[[idP]], 1),
                                       q = not_null(input[[idQ]], 1),
                                       ar = not_null(input[[idAR]]),
                                       ma = not_null(input[[idMA]]),
                                       dist = input[[idDist]],
                                       data = stockInfo()$returns(),
                                       mean = input[[idMean]],
                                       info = FALSE)

          previousParams[[x]] <- currentParams
          print(paste0(stockInfo()$ticker(), " - fitted model: ", x))
        }

        req(fitResults[[x]])

        output[[make_id(x,"results")]] <- renderPrint(fitResults[[x]])

      }) # end lapply
    }) # end observe
  })
}
