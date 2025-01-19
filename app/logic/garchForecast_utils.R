box::use(
  shiny[uiOutput ,sliderInput,conditionalPanel, tags, numericInput, selectizeInput],
  bslib[navset_underline, nav_panel,card, card_header,card_body, card_footer],
  data.table[data.table],
 # rugarch[VaRTest],
  reactable[reactable, colDef, reactableOutput, reactableTheme],
  echarts4r[e_dims,e_show_loading, e_title, echarts4rOutput,e_scatter,e_grid,e_legend,e_tooltip,e_line,e_charts,]
)

box::use(
  app / logic / general_utils[subtitle],
  app / logic / garchFit_utils[models, body_subtitle],
  app / styles / colors[custom_blue]

)

#' @export
conditionalForecastCard <-  function(ns, model){

  conditionalPanel(condition = sprintf("input['app-garchFit-modelCheckbox'].includes('%s')", model),
                   card(full_sreen = T,
                        model_header(ns, model),
                        model_body(ns, model),
                        model_footer(ns, model)
                   ))
}

model_header <- function(ns, model){
  card_header(
    tags$div(
      class = "d-flex align-items-center",
      tags$div( paste0(names(models[which(models == model)]), " Model"),
                class = "d-flex align-items-center", style = "margin-bottom:5px;"),
      tags$div(style = "margin-left: 10px; width: 30px; height: 30px; margin-bottom: 5px",
               id = ns(paste0(model,"loader")))
    ),
    class = "d-flex justify-content-between pb-0"
  )
}

model_body <- function(ns, model){
  card_body(fill = F,
            navset_underline(header = tags$hr(style = "margin: 0; margin-top: 10px; margin-bottom: 15px;"),
                             nav_panel("Plot",
                                       echarts4rOutput(ns(paste0(model,"forecastPlot")), height = 350 )
                             ),
                             nav_panel("Report",
                                       body_subtitle("VaR Backtest Report", "margin-bottom: 0;"),
                                       reactableOutput(ns(paste0(model,"tableInfo"))),
                                       tags$br(),
                                       body_subtitle("VaR Exceedance", "margin-bottom: 0;"),
                                       reactableOutput(ns(paste0(model,"tableExceed"))),
                                       tags$br(),
                                       body_subtitle("Unconditional Coverage (Kupiec)", "margin-bottom: 0;"),
                                       reactableOutput(ns(paste0(model,"tableUc"))),
                                       tags$br(),
                                       body_subtitle("Conditional Coverage (Christoffersen)", "margin-bottom: 0;"),
                                       reactableOutput(ns(paste0(model,"tableCc"))),
                             )
            )
  )
}

model_footer <- function(ns, model){
  card_footer(
    sliderInput(ns(paste0(model,"nahead")), NULL, width = "100%",
                min = 1, max = 30, value = 10, ticks = F)
  )
}
