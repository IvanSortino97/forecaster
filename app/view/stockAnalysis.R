# app/view/

box::use(
  shiny[...,observe, req, reactiveValues, div, moduleServer, NS,],
  bslib[..., page_fillable, card, card_header, card_body],
  data.table[data.table],
  echarts4r[renderEcharts4r, echarts4rOutput, ],
  reactable[renderReactable, reactableOutput],
  shiny.router[is_page],
  shinybrowser[get_device],

)
box::use(
  app / logic / general_utils[title, subtitle ],
  app / logic / stockAnalysis_utils[get_returns,
                                    make_analysis_plots, make_price_table],
)

#' @export
ui <- function(id) {
  ns <- NS(id)

  page_fillable(

    title("Stock Analysis"),
    subtitle("Perform analysis on returns, ACF, PACF e other things ... "),
    navset_card_underline(
      height = 400,
      full_screen = TRUE,
      nav_panel( "Returns", echarts4rOutput(ns("returns_plot"))
      ),
      nav_panel( "Sq. returns", echarts4rOutput(ns("sqReturns_plot"))
      ),
      nav_panel( "Distribution", echarts4rOutput(ns("pricesDistr_plot"))
      )
    ),
    card(
      card_header("Summary statistics"),
      layout_column_wrap(
        width = 1/2,
        div(card_header("Prices", class = "p-0"), reactableOutput(ns("summaryPrice"), width = "95%")),
        div(card_header("Returns", class = "p-0"), reactableOutput(ns("summaryReturns"), width = "95%"))
      )

      )

)
}

#' @export
server <- function(id, stockInfo) {

  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    r <- reactiveValues()
    plots = reactiveVal()
    stats = reactiveVal()

    observe({
      req(stockInfo()$data_xts(), is_page("stockAnalysis") )

      ret <- get_returns(stockInfo()$data_xts())
      r$returns <- ret$returns
      r$returns_dt <- ret$returns_dt
      r$squared_returns <- ret$squared_returns
      r$squared_returns_dt <- ret$squared_returns_dt
      r$price <- data.frame(price = stockInfo()$data()$Close)
      plots(make_analysis_plots(r, stockInfo()$ticker()))
      stats(make_price_table(stockInfo()$data_xts(), r$returns))

    })

    output$returns_plot <- renderEcharts4r({
      req(plots())
      plots()$daily_returns_plot
    })
    output$sqReturns_plot <- renderEcharts4r({
      req(plots())
      plots()$squared_returns_plot
    })
    output$pricesDistr_plot <- renderEcharts4r({
      req(plots())
      plots()$returns_distribution_plot
    })

    output$summaryPrice <- renderReactable({
      req(stats())
      stats()$table_prices
    })
    output$summaryReturns <- renderReactable({
      req(stats())
      stats()$table_returns
    })


  })
}
