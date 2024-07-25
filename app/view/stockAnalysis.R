# app/view/

box::use(
  shiny[div, moduleServer, NS],
  bslib[page_fillable],
)
box::use(
  app / logic / general_utils[title, subtitle ],
)

#' @export
ui <- function(id) {
  ns <- NS(id)

  page_fillable(

    title("Stock Analysis"),
    subtitle("Perform analysis on returns, ACF, PACF e other things ... ")

)
}

#' @export
server <- function(id, ...) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns


  })
}
