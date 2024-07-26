# app/view/

box::use(
  shiny[div, moduleServer, NS, ...],
  bslib[page_fillable],
  shinytoastr[toastr_warning, useToastr],
)
box::use(
  app / logic / general_utils[title, subtitle ],
)

#' @export
ui <- function(id) {
  ns <- NS(id)

  page_fillable(
    shinytoastr::useToastr(),

    title("Stock Analysis"),
    subtitle("Perform analysis on returns, ACF, PACF e other things ... "),
    actionButton(ns("button"), "test toaster")

)
}

#' @export
server <- function(id, ...) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    observeEvent(input$button, {
      shinytoastr::toastr_warning(
        title = "Warning",
        message = sprintf("Warning: %s", 'warning$message'),
        position = "top-center",
        timeOut = 0,
        closeButton = TRUE
      )
    })


  })
}
