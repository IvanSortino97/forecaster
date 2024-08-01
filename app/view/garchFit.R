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
                                                model = "TGARCH"
                              ),
                              conditional_model(ns = ns,
                                                model = "APARCH"
                              ),
                              conditional_model(ns = ns,
                                                model = "IGARCH"
                              ),
                              conditional_model(ns = ns,
                                                model = "FIGARCH"
                              ),
                              conditional_model(ns = ns,
                                                model = "QGARCH"
                              ),
                              conditional_model(ns = ns,
                                                model = "NGARCH"
                              ),
                              conditional_model(ns = ns,
                                                model = "VGARCH"
                              ),
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
    selected <- reactiveVal(NULL)

    observe({
      req(stockInfo()$returns(), is_page("garchFit"))

      if(once()){
      selected(input$modelCheckbox)
      print(selected()) # compute selected
      once(FALSE)
      }

    lapply(input$modelCheckbox, function(x){

    idSwitch <- make_id(x,"switch")
    idDist <- make_id(x,"dist")
    idP <- make_id(x,"p")
    idQ <- make_id(x,"q")
    idAutoTable <- make_id(x,"autoTable")


      req(stockInfo()$returns())

      if(!input[[idSwitch]]){

        enable(idDist)
        enable(idP)
        enable(idQ)

      } else {

        best_fit <- get_best_fit(model = x, stockInfo()$returns())
        criteria = "AIC"
        param <- get_param(best_fit, criteria)

        updateSelectizeInput(inputId = idDist, selected = param$dist)
        updateNumericInput(inputId = idP, value = param$p)
        updateNumericInput(inputId = idQ, value = param$q)

        disable(idDist)
        disable(idP)
        disable(idQ)

        output[[idAutoTable]] <- renderReactable({
          make_autoTable(best_fit$results, param$index, criteria)
        })

      }
    }) #end lapply

    }) #end observe




  })
}
