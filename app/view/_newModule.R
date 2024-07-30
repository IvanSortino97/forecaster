# app/view/

box::use(
  shiny[div, moduleServer, NS, tags],
  bslib[page_fillable, , card, card_header, card_body, card_title]
)
box::use(
  app / logic / general_utils[title, subtitle, ...],
)

#' @export
ui <- function(id) {
  ns <- NS(id)

  page_fillable(
    div("New module")
  )


}

#' @export
server <- function(id, ...) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns


  })
}
