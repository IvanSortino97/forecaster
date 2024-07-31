# app/view/

box::use(
  shiny[..., checkboxGroupInput, div, moduleServer, NS],
  bslib[page_fillable, card, card_header, card_body, card_title],
  shinyjs[show, hide]
)
box::use(
  app / logic / general_utils[conditional_page_fillable, make_spinner, select_stock_condition],
)

#' @export
ui <- function(id) {
  ns <- NS(id)

  conditional_page_fillable(ns = ns,

    title = "Fit Models",
    subtitle = "Select models to fit and specify parameters",
    body = div(


    checkboxGroupInput(ns("checkModels"),
                       label = NULL,
                       choices = c(
                         "GARCH" = "GARCH",
                         "eGARCH" = "eGARCH",
                         "GJR - GARCH" = "GJR - GARCH",
                         "TGARCH" = "TGARCH",
                         "APARCH" = "APARCH",
                         "IGARCH" = "IGARCH",
                         "FIGARCH" = "FIGARCH",
                         "HYGARCH" = "HYGARCH",
                         "QGARCH" = "QGARCH",
                         "NGARCH" = "NGARCH",
                         "VGARCH" = "VGARCH"
                       ))



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
      select_stock_condition(stockInfo()$data_xts())})


  })
}
