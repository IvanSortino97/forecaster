# app/view/

box::use(
  shiny[...,observe, req, reactiveValues, div, moduleServer, NS,],
  bslib[..., page_fillable, card, card_header, card_body],
  data.table[data.table],
  echarts4r[..., renderEcharts4r, echarts4rOutput, ],
  shiny.router[is_page],
  shinybrowser[get_device],
)
box::use(
  app / logic / general_utils[title, subtitle ],
  app / logic / stockAnalysis_utils[get_returns, ],
)

#mobile = get_device()

#' @export
ui <- function(id) {
  ns <- NS(id)

  page_fillable(

    title("Stock Analysis"),
    subtitle("Perform analysis on returns, ACF, PACF e other things ... "),
    navset_card_pill(
      height = 400,
      full_screen = TRUE,
      id = ns("navsetPlots"),
      nav_panel(
        "Returns",
        echarts4rOutput(ns("returns_plot"))
      ),
      nav_panel(
        "Sq. returns",
        echarts4rOutput(ns("sqReturns_plot"))
      ),
      nav_panel(
        "Distribution",
        echarts4rOutput(ns("pricesDistr_plot"))
      )
    )

)
}

#' @export
server <- function(id, stockInfo) {

  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    r <- reactiveValues()


    # observe({
    #   nav_insert(
    #     "navsetPlots", target = "Returns",
    #     nav_panel(
    #       if(get_device() == "Mobile") "Sq. returns" else "Squared returns",
    #       echarts4rOutput(ns("sqReturns_plot"))
    #     )
    #   )
    # })

    observe({
      req(stockInfo()$data_xts(), is_page("stockAnalysis") )

      ret <- get_returns(stockInfo()$data_xts())
      r$returns <- ret$returns
      r$returns_dt <- ret$returns_dt
      r$squared_returns <- ret$squared_returns
      r$squared_returns_dt <- ret$squared_returns_dt
      r$price <- data.frame(price = stockInfo()$data()$Close)
    })

    output$returns_plot <- renderEcharts4r({
      req(r$returns_dt)

      r$returns_dt |>
        e_charts(date) |>
        e_line(daily.returns,
               smooth = FALSE,
               symbol = "none",
               lineStyle = list(color = "#5756ff", width = 1),
               legend = FALSE) |>
        e_title("Daily Returns") |>
        e_tooltip(trigger = "axis") |>
        e_legend(FALSE) |>
        e_grid(right = 5, left = 5) |>
        e_datazoom(type = "slider",x_axis_index = 0, toolbox = T)
    })

    output$sqReturns_plot <- renderEcharts4r({
      req(r$squared_returns_dt)

      r$squared_returns_dt |>
        e_charts(date) |>
        e_line(squared.returns,
               smooth = FALSE,
               symbol = "none",
               lineStyle = list(color = "#5756ff", width = 1),
               legend = FALSE) |>
        e_title("Squared Daily Returns") |>
        e_tooltip(trigger = "axis") |>
        e_legend(FALSE) |>
        e_grid(right = 5, left = 5) |>
        e_datazoom(type = "slider",x_axis_index = 0, toolbox = T)
    })

    output$pricesDistr_plot <- renderEcharts4r({
      req(r$returns_dt)

      r$returns_dt |>
        e_charts() |>
        e_density(daily.returns) |>
        e_title("Returns Distribution") |>
        e_tooltip(trigger = "axis") |>
        e_legend(FALSE) |>
        e_grid(right = 5, left = 5, bottom = 5)
    })



  })
}
