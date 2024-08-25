box::use(
  shiny[conditionalPanel, tags, numericInput, selectizeInput],
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

                     numericInput(ns(paste0(model,"forecastLength")), "Out.of-sample data", 1000)
                   ))
}

model_settings <- function(ns, model){
  popover(
  bs_icon("gear", title = "settings", marginTop = "5px"),
  title = "Settings",
  numericInput(ns(paste0(model,"refitEvery")), "Refit every", 100 ),
  selectizeInput(ns(paste0(model,"refitWindow")), "Refit window", choices =  c("moving","recursive"), selected = "moving"),
  selectizeInput(ns(paste0(model,"solver")), "solver", choices = c("nlminb", "solnp", "lbfgs", "gosolnp", "nloptr", "hybrid"), selected = "hybrid" )

  )
  }
