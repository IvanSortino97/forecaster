# app/view/

box::use(
  shiny[div, moduleServer, NS, tags],
  bslib[page_fillable, , card, card_header, card_body, card_title]
)
box::use(
  app / logic / general_utils[title, subtitle, make_spinner, no_stock_message],
)

#' @export
ui <- function(id) {
  ns <- NS(id)

  page_fillable(

    title("New Module",
          id = "titleLoader"),

    subtitle("Subtitle"),

    div(id = ns("conditionalMessage"), no_stock_message()),
    div(id = ns("conditionalPanel"),




    )
  )


}

#' @export
server <- function(id, ...) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns


  })
}
