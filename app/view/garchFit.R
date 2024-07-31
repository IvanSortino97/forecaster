# app/view/

box::use(
  shiny[..., conditionalPanel, observeEvent, reactiveVal, checkboxGroupInput, div, moduleServer, NS],
  bslib[page_fillable, card, card_header, card_body, card_title],
  shinyjs[show, hide],
)
box::use(
  app / logic / general_utils[conditional_page_fillable, make_spinner, select_stock_condition],
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
                                tags$h5("Select model to fit", style = "font-weight: 500; line-height: 1.375rem;font-size: 1rem; color: #464646;"),
                                model_checkbox(ns("modelCheckbox"))
                              ),
                              card_title("Specify parameters"),
                              conditional_model(ns = ns,
                                                model = "GARCH",
                                                body = p("This is the GARCH model.")
                              ),
                              conditional_model(ns = ns,
                                                model = "eGARCH",
                                                body = p("This is the eGARCH model.")
                              ),
                              conditional_model(ns = ns,
                                                model = "GJR - GARCH",
                                                body = p("This is the gjr GARCH model.")
                              ),
                              conditional_model(ns = ns,
                                                model = "TGARCH",
                                                body = p("This is the GARCH model.")
                              ),
                              conditional_model(ns = ns,
                                                model = "APARCH",
                                                body = p("This is the GARCH model.")
                              ),
                              conditional_model(ns = ns,
                                                model = "IGARCH",
                                                body = p("This is the GARCH model.")
                              ),
                              conditional_model(ns = ns,
                                                model = "FIGARCH",
                                                body = p("This is the GARCH model.")
                              ),
                              conditional_model(ns = ns,
                                                model = "QGARCH",
                                                body = p("This is the GARCH model.")
                              ),
                              conditional_model(ns = ns,
                                                model = "NGARCH",
                                                body = p("This is the GARCH model.")
                              ),
                              conditional_model(ns = ns,
                                                model = "VGARCH",
                                                body = p("This is the GARCH model.")
                              ),
                            )
  )
}

#' @export
server <- function(id, stockInfo) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    spinner <- make_spinner("titleLoader")

    # Example of a function call that might be used in the server logic
    # hide("conditionalPanel")
    # observeEvent(stockInfo()$data_xts() , {
    #   select_stock_condition(stockInfo()$data_xts())
    # })
  })
}
