box::use(shiny[observe, reactive, reactiveVal, moduleServer, NS, tags, renderText, req, tagList],
         bslib[..., page_sidebar, page_fillable], # do not remove "..."
         shinybrowser[get_device, detect],
         shinyjs[useShinyjs],
         waiter[useWaiter,waiterPreloader, waiterShowOnLoad, ...],
         shinytoastr[toastr_warning, useToastr],
         shiny.router[get_page, router_ui, route, router_server])

box::use(app / view[...],
         app / logic / general_utils[onStart, onEnd, header, start_spinner],
         app / styles / colors[custom_blue])


#' @export
ui <- function(id) {
  ns <- NS(id)

  page_sidebar(fillable_mobile = TRUE,
               useToastr(),
               detect(), # shinybrowser
               head$html, # head script and style
               useShinyjs(),
               useWaiter(),

               start_spinner(color = custom_blue,
               text = "Forcaster is loading...",
               text_style = sprintf("color:%s;", custom_blue)),

    title = header("Forcaster", idtextOutput = ns("stockTicker")),


    sidebar = sidebar$sidebar_component(ns("sidebarId")),

    page_fillable(router_ui(
      route("/", homepage$ui(ns("homepage"))),
      route("stockInfo", stockInfo$ui(ns("stockInfo"))),
      route("stockAnalysis", stockAnalysis$ui(ns("stockAnalysis"))),

      route("garchFit", garchFit$ui(ns("garchFit"))),
      route("garchBacktest", garchBacktest$ui(ns("garchBacktest"))),
      route("garchForecast", garchForecast$ui(ns("garchForecast")))

    ))
  )

}
#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    onStart()
    onEnd()

    start_page <- "/"
    router_server(start_page)

    homepage$server("homepage")
    op_stockInfo <- stockInfo$server("stockInfo")
    stockAnalysis$server("stockAnalysis", reactive(op_stockInfo))

    op_garchFit <- garchFit$server("garchFit", reactive(op_stockInfo))
    garchBacktest$server("garchBacktest", reactive(op_stockInfo), reactive(op_garchFit))
    garchForecast$server("garchForecast", reactive(op_stockInfo), reactive(op_garchFit))


    # -------------------------------------------------------------------------

    # Close sidebar when in Mobile mode + scroll on top when pages are changed
    active_page <- reactiveVal(start_page)
    observe({
      req(get_page() != active_page())
      session$sendCustomMessage("scrollToTop", list())
      active_page(get_page())
      req(get_device() != "Desktop")
      sidebar_toggle(id = "sidebarId", open = FALSE, session = session )
    })

    # Stock name printed top right of the header
    output$stockTicker <- renderText({
      req(op_stockInfo$name())
      sub("-.*", "", op_stockInfo$name())
    })


  })
}
