box::use(shiny[..., bootstrapPage, div, moduleServer, NS, renderUI, tags, uiOutput],
         bslib[...],
         shiny.router[router_ui, route, router_server],
         bsicons[bs_icon])

box::use(app / view[...],
         app / logic / general_utils[onStart, onEnd, header])




#' @export
ui <- function(id) {
  ns <- NS(id)

  page_sidebar(
    fillable_mobile = TRUE,
    sidebar$head,

    title = header("Forcaster"),

    sidebar = sidebar$sidebar_component,

    page_fillable(router_ui(
      route("/", div("root_page")),
      route("page1", stockInfo$ui(ns("stockInfo"))),
      route("page2", div("page2"))
    ))
  )

}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    onStart()
    onEnd()

    router_server("page1")

    stockInfo$server("stockInfo")

  })
}
