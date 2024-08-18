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
    observeEvent(stockInfo()$data_xts() , {
      select_stock_condition(stockInfo()$data_xts())
    })

    once <- reactiveVal(TRUE)
    toComputeAuto <- reactiveValues()


    observe({
      req(stockInfo()$returns(), is_page("garchFit"))

      # if(once()){
      # once(FALSE)
      # }

      print("outside lapply")
      print(toComputeAuto)

    lapply(input$modelCheckbox, function(x){
      print(paste0("begin lapply, x = ", x))

    idSwitch <- make_id(x,"switch")
    idDist <- make_id(x,"dist")
    idP <- make_id(x,"p")
    idQ <- make_id(x,"q")
    idAR <- make_id(x,"ar")
    idMA <- make_id(x,"ma")
    idAutoTable <- make_id(x,"autoTable")

      if(!input[[idSwitch]]){
      print(paste0("if not begin, ", toComputeAuto[[x]]))

        enable(idDist)
        enable(idP)
        enable(idQ)
        enable(idAR)
        enable(idMA)

        toComputeAuto[[x]] <- TRUE
      print(paste0("if not after, ", toComputeAuto[[x]]))

      } else {
      print(paste0("else begin, ", toComputeAuto[[x]]))

        req(toComputeAuto[[x]])

        best_fit <- get_best_fit(model = x, stockInfo()$returns())
        criteria = "AIC"
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

        output[[idAutoTable]] <- renderReactable({
          make_autoTable(best_fit$results, param$index, criteria)
        })

        toComputeAuto[[x]] <- FALSE
      print(paste0("else after, ", toComputeAuto[[x]]))

      }
    }) #end lapply

    }) #end observe




  })
}
