box::use(
  shiny[conditionalPanel, tags, numericInput],
  bslib[card, card_header, popover],
  bsicons[bs_icon]
)

#' @export
conditionalBacktestCard <-  function(ns, model){

  conditionalPanel(condition = sprintf("input['app-garchFit-modelCheckbox'].includes('%s')", model),
                   card(
                     card_header(
                       model, model_settings(ns, model),
                       class = "d-flex justify-content-between"
                     ),
                     tags$p("body")
                   ))
}

model_settings <- function(ns, model){
  popover(
  bs_icon("gear", title = "settings", marginTop = "5px"),
  title = "Settings",
  numericInput( ns(paste0(model,"test")), "Window size", 500 )

  )
  }
