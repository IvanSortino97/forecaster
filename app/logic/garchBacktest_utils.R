box::use(
  shiny[uiOutput ,sliderInput,conditionalPanel, tags, numericInput, selectizeInput],
  bslib[card, card_header,card_body, card_footer, popover],
  bsicons[bs_icon]
)

box::use(
  app / logic / general_utils[subtitle]
)

#' @export
conditionalBacktestCard <-  function(ns, model){

  conditionalPanel(condition = sprintf("input['app-garchFit-modelCheckbox'].includes('%s')", model),
                   card(
                     model_header(ns, model),
                     model_body(ns, model),
                     model_footer(ns, model)
                   ))
}

model_header <- function(ns, model){
  card_header(
    model, model_settings(ns, model),
    class = "d-flex justify-content-between"
  )
}

model_settings <- function(ns, model){

  popover(
    bs_icon("gear", title = "settings", marginTop = "5px"),
    title = "Settings",
    numericInput(ns(paste0(model, "refitEvery")), "Refit every", 100),
    selectizeInput(
      ns(paste0(model, "refitWindow")),
      "Refit window",
      choices =  c("moving", "recursive"),
      selected = "moving"
    ),
    selectizeInput(
      ns(paste0(model, "solver")),
      "solver",
      choices = c("nlminb", "solnp", "lbfgs", "gosolnp", "nloptr", "hybrid"),
      selected = "hybrid"
    )
  )
}

model_body <- function(ns, model){
  card_body(
    tags$p("body")
  )
}

model_footer <- function(ns, model){
  card_footer(
    uiOutput(ns(paste0(model,"sliderInfo"))),
    sliderInput(ns(paste0(model,"forecastLength")), NULL, width = "100%",
                min = 1, max = 100, value = 50, ticks = F)
  )
}

#' @export
slider_info <- function(tot, inSample, obs){
  tags$div(style = "margin-bottom: 20px;",
  tags$div(class = "d-flex justify-content-between",
    tags$span(style = "display: inline-flex; align-items: center;",
      tags$p(sprintf("Total %s:",obs), style = "margin-right: 5px;"),
      tags$p(tot)
    ),
    tags$span(style = "display: inline-flex; align-items: center;",
      tags$p(sprintf("In-sample %s:",obs), style = "margin-right: 5px;"),
      tags$p(inSample)
    ),
  ),
  tags$span(style = "display: inline-flex; align-items: baseline; white-space: nowrap;",
            tags$span("Out-of-sample size:", style = "margin-right: 5px;"),
            tags$span(paste0("Default 1/3 of total ", obs) ,style ="font-size: 0.8rem; color: #7f8189;")
  )
  )
}
