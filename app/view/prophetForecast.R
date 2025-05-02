# app/view/

box::use(
  shiny[div, moduleServer, NS, tags, observeEvent, observe, req, sliderInput],
  bslib[page_fillable, card, card_header, card_body, card_title, card_footer],
  shiny.router[is_page],
  shinyjs[hide],
  echarts4r[echarts4rOutput, renderEcharts4r],
)
box::use(
  app / logic / general_utils[conditional_page_fillable, make_spinner, show_condition, page_footer],
  app / logic / prophetForecast_utils[...],
)

#' @export
ui <- function(id) {
  ns <- NS(id)

  conditional_page_fillable(ns = ns,

      title = "Prophet Model",
      subtitle = "Prophet is a procedure for forecasting time series data based on an additive model where non-linear trends are fit with yearly, weekly, and daily seasonality, plus holiday effects. It works best with time series that have strong seasonal effects and several seasons of historical data. Prophet is robust to missing data and shifts in the trend, and typically handles outliers well.",
      condition_page = "stockInfo",
      body = div(
        card(
          card_body(echarts4rOutput(ns("plot"))),
            card_footer(
            sliderInput(ns("periods"), "Number of Days to Forecast",
                        min = 1, max = 100, value = 30, width = "100%", ticks = F)
  )

        ),
        
        page_footer(
          hrefPageNext = "garchFit",
          textPageNext = "Fitting Model",
          hrefPagePrecedent = "stockAnalysis",
          textPagePrecedent = "Stock Analysis"
        )
      )

  )
}

#' @export
server <- function(id, stockInfo) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    #spinner <- make_spinner("titleLoader")

    hide("conditionalPanel")
    observeEvent(stockInfo()$data_xts() , {
      show_condition(stockInfo()$data_xts())})
    
    observe({
    
      req(stockInfo()$returns(), is_page("prophetForecast"))

      show_condition(stockInfo()$returns())

      model <- make_prophet_model(stockInfo()$ticker(),
                                  stockInfo()$data()$date,
                                  stockInfo()$data()$Close,
                                  input$periods)
      
      output$plot <- renderEcharts4r({model$plot})

      
    })

  })
}
