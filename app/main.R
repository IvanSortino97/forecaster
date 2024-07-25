box::use(shiny[..., bootstrapPage, div, moduleServer, NS, renderUI, tags, uiOutput],
         bslib[...],
         shiny.router[router_ui, route, router_server],
         bsicons[bs_icon])

box::use(app / view[...],
         app / logic / general_utils[onStart, onEnd, header])




#' @export
ui <- function(id) {
  ns <- NS(id)

  page_sidebar(fillable_mobile = TRUE,

    sidebar$head,

    title = header("Forcaster"),

    sidebar = sidebar$sidebar_component,

    page_fillable(router_ui(
      route("/", homepage$ui(ns("homepage"))),
      route("stockInfo", stockInfo$ui(ns("stockInfo"))),
      route("stockAnalysis", stockAnalysis$ui(ns("stockAnalysis"))  )
    ))
  )

}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    onStart()
    onEnd()

    router_server("stockAnalysis")

    homepage$server("homepage")
    stockInfo$server("stockInfo")
    stockAnalysis$server("stockAnalysis")

  })
}
