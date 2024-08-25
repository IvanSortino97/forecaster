# app/view/

box::use(
  shiny[... , div, moduleServer, NS, tags, observeEvent, observe],
  bslib[page_fillable, card, card_header, card_body, card_title],
  shinyjs[hide],
  shinybrowser[is_device_mobile],
  shiny.router[is_page],
)
box::use(
  app / logic / general_utils[in_card_subtitle_style, conditional_page_fillable, make_spinner, show_condition],
  app / logic / garchFit_utils[models],
  app / logic / garchBacktest_utils[...]
)

#' @export
ui <- function(id) {
  ns <- NS(id)

  conditional_page_fillable(ns = ns,

                            title = "GARCH Model backtesting",
                            subtitle = "Configure window size and backtesting methods to evaluate model performance.",
                            condition_page = "garchFit",
                            body = div(
                                tagList(lapply(models, function(x) conditionalBacktestCard(ns = ns, x)))
                            )
  )
}

#' @export
server <- function(id, stockInfo, garchFit) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    spinner <- make_spinner("titleLoader")
    #hide("conditionalPanel")
    observeEvent(garchFit()$fitResults(),{
      show_condition(garchFit()$fitResults())

      })

    #general observe to fetch stockInfo
    observeEvent(stockInfo()$returns(),{
      req(stockInfo()$returns())
      totObs <- length(stockInfo()$returns())
      outSample <- floor(totObs/3)
      inSample <- totObs - outSample

      lapply(models, function(x){
        updateSliderInput(inputId = paste0(x,"forecastLength"), max = totObs-1, value = outSample)
        obs <- if(is_device_mobile()) "obs." else "observations"

        output[[paste0(x,"sliderInfo")]] <- renderUI({
          slider_info(totObs, inSample, obs)
        })
      })
    })

  })
}
