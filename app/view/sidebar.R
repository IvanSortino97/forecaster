box::use(shiny[tags,],
         bslib[sidebar, accordion, accordion_panel, sidebar_toggle],
         shiny.router[route_link],
         bsicons[bs_icon])

# Custom class for accordion sections
accordion_section <- function(title, icon = NULL, page, custom_class = "custom-accordion-section") {
  tags$a(
    id = if(page != "/") page else "home" ,
    href = route_link(page),
    class = custom_class,
    if (!is.null(icon)) { bs_icon(icon) },
    tags$p(title, style = "display: inline; margin-left: 5px; ")
  )
}

#' @export
sidebar_component <- function(idSd) {
  sidebar(
    id = idSd,
    class = "custom-sidebar",

    # Home
    accordion_section(
      title = "Home",
      page = "/",
      icon = "house"
    ),

    # Stock info
    accordion(
      open = c("Stock info"),
      accordion_panel(
        "Stock info",
        icon = bs_icon("graph-up"),
        accordion_section(
          title = "Stock Selection",
          page = "stockInfo",
          icon = "list-task"
        ),
        accordion_section(
          title = "Stock Analysis",
          page = "stockAnalysis",
          icon = "clipboard2-data"
        )
      ),

            accordion_panel(
        "PROPHET Model",
        icon = bs_icon("lightning-fill"),
       accordion_section(
          title = "Forecast",
          page = "prophetForecast",
          icon = "fast-forward"
        )
      ),

      accordion_panel(
        "GARCH Models",
        icon = bs_icon("hammer"),
       accordion_section(
          title = "Fit Models",
          page = "garchFit",
          icon = "sliders2-vertical"
        ),
       accordion_section(
          title = "Backtest Models",
          page = "garchBacktest",
          icon = "repeat"
        ),
       accordion_section(
          title = "Forecast",
          page = "garchForecast",
          icon = "fast-forward"
        )
      ),

      accordion_panel(
        "ARIMA Models",
        icon = bs_icon("cone")

      )
    )
  )
}

#' @export
#' define style of a components and appear like pushed buttons
#' moved to app/view/head.R

