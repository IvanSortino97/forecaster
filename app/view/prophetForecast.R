# app/view/

box::use(
  shiny[div, moduleServer, NS, tags, observeEvent],
  bslib[page_fillable, card, card_header, card_body, card_title],
  shinyjs[hide],
)
box::use(
  app / logic / general_utils[conditional_page_fillable, make_spinner, show_condition],
)

#' @export
ui <- function(id) {
  ns <- NS(id)

  conditional_page_fillable(ns = ns,

      title = "Prophet Model",
      subtitle = "Subtitle",
      condition_page = "stockInfo",
      body = div(
        "div content"
      )
  )
}

#' @export
server <- function(id, stockInfo) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    #spinner <- make_spinner("titleLoader")

    hide("conditionalPanel")
    observeEvent(stockInfo()$data_xts() , {
      show_condition(stockInfo()$data_xts())})

  })
}
