# app/view/

box::use(
  shiny[div, moduleServer, NS],
)
box::use(
  app / logic / general_utils[...],
)

#' @export
ui <- function(id) {
  ns <- NS(id)



}

#' @export
server <- function(id, ...) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns


  })
}
