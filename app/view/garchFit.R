# app/view/

box::use(
  shiny[..., checkboxGroupInput, div, moduleServer, NS],
  bslib[page_fillable, card, card_header, card_body, card_title]
)
box::use(
  app / logic / general_utils[title, subtitle, ...],
)

#' @export
ui <- function(id) {
  ns <- NS(id)

  page_fillable(

    title("Fit Models"),
    subtitle("Select models to fit and specify parameters"),

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


}

#' @export
server <- function(id, ...) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns


  })
}
