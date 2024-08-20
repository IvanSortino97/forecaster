# app/view/

box::use(
  shiny[div, moduleServer, NS, tagList, tags, hr],
  shiny.router[route_link]
)
box::use(
  app / logic / general_utils[...],
)

page_links <- list(
  list(title = "Home", page = "/"),
  list(title = "Stock Selection", page = "stockInfo"),
  list(title = "Stock Analysis", page = "stockAnalysis"),
  list(title = "Fit model", page = "garchFit"),
  list(title = "Backtest model", page = "garchBacktest")
)

#' @export
ui <- function(id) {
  ns <- NS(id)

tagList(
div("TODO: Homepage"),
hr(),
lapply(page_links, function(link) {
  tags$a(href = route_link(link$page),
         class = "custom-accordion-section",
         link$title, tags$br())
})
)

}

#' @export
server <- function(id, ...) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns


  })
}
