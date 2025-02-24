# app/view/

box::use(
  shiny[div, moduleServer, NS, tags, observeEvent, observe, req, sliderInput],
  bslib[page_fillable, card, card_header, card_body, card_title],
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
      subtitle = "Subtitle",
      condition_page = "stockInfo",
      body = div(
        "div content",
sliderInput(ns("periods"), "Periods", min = 1, max = 100, value = 30),
        echarts4rOutput(ns("plot")),



            page_footer(hrefPageNext = "garchFit",
                textPageNext = "Fitting Model",
                hrefPagePrecedent = "stockAnalysis",
                textPagePrecedent = "Stock Analysis")
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

      model <- make_prophet_model(stockInfo()$data()$date,
                                  stockInfo()$data()$Close,
                                  input$periods)
      
      output$plot <- renderEcharts4r({model$plot})

      
    })

  })
}
