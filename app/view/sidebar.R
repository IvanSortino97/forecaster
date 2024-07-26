box::use(shiny[ actionButton, tags, HTML, observe, reactiveVal, moduleServer, req],
         bslib[sidebar, accordion, accordion_panel, sidebar_toggle],
         shiny.router[route_link, get_page],
         shinybrowser[get_device],
         bsicons[bs_icon])

accordion_section <- function(title, icon = NULL, page){
  tags$a(href = route_link(page), if(!is.null(icon)){ bs_icon(icon)}, tags$h6(title, style = "display: inline; margin-left: 5px;") )
}

#' @export
sidebar_component <- function(idSd){
  sidebar(

  id = idSd,

  # Home
  accordion_section(title = "Home",
                    page = "/",
                    icon = "house"),

  # Stock info
  accordion(
    accordion_panel(
      "Stock info",
      icon = bs_icon("graph-up"),
      accordion_section(title = "Stock selection",
                        page = "stockInfo",
                        icon = "list-task"),
      accordion_section(title = "Stock Analysis",
                        page = "stockAnalysis",
                        icon = "clipboard2-data")
    ),

    # Test buttons
    accordion_panel(
      "Section 2"
    )
  )

)}


#' @export
#' define style of a components and appear like pushed buttons
#' moved to app/view/head.R

