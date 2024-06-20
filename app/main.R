box::use(shiny[..., bootstrapPage, div, moduleServer, NS, renderUI, tags, uiOutput],
         bslib[...],
         shiny.router[...],
         bsicons[bs_icon])

box::use(app / view[...],
         app / logic / general_utils[header])



root_page <- div(h2("Root page"))
other_page <- div(h3("Other page"))
page1 <- div(h3("Page 1"))
page2 <- div(h3("Page 2"))



#' @export
ui <- function(id) {
  ns <- NS(id)

  page_sidebar(
    sidebar$head,

    title = header("Forcaster"),


    sidebar = sidebar$sidebar_component,

    page_fillable(title = "Router demo",
                  router_ui(
                    route("/", root_page),
                    route("other", other_page),
                    route("page1", page1),
                    route("page2", page2)
                  ))
  )

}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    router_server("/")

  })
}
