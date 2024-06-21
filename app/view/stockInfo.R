box::use(shiny[div, moduleServer, NS, selectizeInput, ...],
         bslib[page_fillable],
         echarts4r[echarts4rOutput],
         reactable[reactableOutput, renderReactable, reactable],)
box::use(app / logic / stockInfo_utils[get_symbols])


#' @export
ui <- function(id) {
  ns <- NS(id)

  page_fillable(
    selectizeInput(ns("selectStock"), "Select stock", choices = NULL),
    reactableOutput(ns("stockTable")

  ),
  echarts4rOutput(ns("stockPlot")))
}

#' @export
server <- function(id, ...) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    symbols_dt <- get_symbols()

    output$stockTable <- renderReactable({
      symbols_dt |> reactable(
        compact = TRUE
        )
    })

   updateSelectizeInput(inputId = "selectStock"  , choices = symbols_dt$Symbol, server = TRUE)


  })
}
