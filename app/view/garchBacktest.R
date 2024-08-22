# app/view/

box::use(
  shiny[... ,reactiveValuesToList,div, moduleServer, NS, tags, observeEvent, observe],
  bslib[page_fillable, card, card_header, card_body, card_title],
  shinyjs[hide],
  shiny.router[is_page],
)
box::use(
  app / logic / general_utils[conditional_page_fillable, make_spinner, show_condition],
)

#' @export
ui <- function(id) {
  ns <- NS(id)

  conditional_page_fillable(ns = ns,

                            title = "New Module",
                            subtitle = "Subtitle",
                            condition_page = "garchFit",
                            body = div(
                              "div content",
                              verbatimTextOutput(ns("test"))
                            )
  )
}

#' @export
server <- function(id, stockInfo, garchFit) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    spinner <- make_spinner("titleLoader")

    hide("conditionalPanel")
    observe({

      show_condition(garchFit()$fitResults())

      })


    output$test <- renderPrint({
      garchFit()$selectedModels()
    })

  })
}
