# app/view/

box::use(
  shiny[... , div, moduleServer, NS, tags, observeEvent, observe],
  bslib[page_fillable, card, card_header, card_body, card_title],
  shinyjs[hide],
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
                            subtitle = "TODO: backtest models. Define window size and backtesting method ",
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

    hide("conditionalPanel")
    observeEvent(garchFit()$fitResults(),{
      show_condition(garchFit()$fitResults())

      })

  })
}
