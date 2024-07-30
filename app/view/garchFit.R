# app/view/

box::use(
  shiny[..., checkboxGroupInput, div, moduleServer, NS],
  bslib[page_fillable, card, card_header, card_body, card_title]
)
box::use(
  app / logic / general_utils[title, subtitle, make_spinner, no_stock_message],
)

#' @export
ui <- function(id) {
  ns <- NS(id)

  page_fillable(

    title("Fit Models",
          id = ns("titleLoader")),
    subtitle("Select models to fit and specify parameters"),

    div(id = ns("conditionalMessage"), no_stock_message()),
    div(id = ns("conditionalPanel"),

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
server <- function(id, ...) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    spinner <- make_spinner("titleLoader")

    # hide("conditionalPanel")
    # observeEvent(stockInfo()$data_xts() , {
    #   select_stock_condition(stockInfo()$data_xts(),
    #                          "conditionalMessage","conditionalPanel")})


  })
}
