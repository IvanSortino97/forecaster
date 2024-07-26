# app/view/

box::use(
  shiny[...,observe, req, reactiveValues, div, moduleServer, NS,],
  bslib[page_fillable, card, card_header, card_body],
  data.table[data.table],
  echarts4r[..., renderEcharts4r, echarts4rOutput, ],
  shiny.router[is_page]
)
box::use(
  app / logic / general_utils[title, subtitle ],
  app / logic / stockAnalysis_utils[get_returns, ],
)

#' @export
ui <- function(id) {
  ns <- NS(id)

  page_fillable(

    title("Stock Analysis"),
    subtitle("Perform analysis on returns, ACF, PACF e other things ... "),
    card(
      card_header("Returns Plot"),
      echarts4rOutput(ns("returns_plot")),
      hr(),
      echarts4rOutput(ns("sqReturns_plot")),
      hr(),
      echarts4rOutput(ns("pricesDistr_plot"))
    )


)
}

#' @export
server <- function(id, stockInfo) {

  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    r <- reactiveValues()

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
        e_tooltip(trigger = "axis")
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
        e_tooltip(trigger = "axis")
    })

    output$pricesDistr_plot <- renderEcharts4r({
      req(r$returns_dt)

      r$returns_dt |>
        e_charts() |>
        e_density(daily.returns) |>
        e_title("Returns Distribution") |>
        e_tooltip(trigger = "axis")
    })



  })
}
