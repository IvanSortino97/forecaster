box::use(
  shiny[uiOutput ,sliderInput,conditionalPanel, tags, numericInput, selectizeInput],
  bslib[navset_underline, nav_panel,card, card_header,card_body, card_footer, popover],
  bsicons[bs_icon],
  data.table[data.table],
  rugarch[VaRTest],
  reactable[reactable, colDef, reactableOutput, reactableTheme],
  echarts4r[e_dims,e_show_loading, e_title, echarts4rOutput,e_scatter,e_grid,e_legend,e_tooltip,e_line,e_charts,]
)

box::use(
  app / logic / general_utils[subtitle],
  app / logic / garchFit_utils[models, body_subtitle],
  app / styles / colors[custom_blue]

)

#' @export
conditionalBacktestCard <-  function(ns, model){

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
    model_settings(ns, model),
    class = "d-flex justify-content-between pb-0"
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
  card_body(fill = F,
    navset_underline(header = tags$hr(style = "margin: 0; margin-top: 10px; margin-bottom: 15px;"),
                     nav_panel("Plot",
                               echarts4rOutput(ns(paste0(model,"backtestPlot")), height = 350 )
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

#' @export
make_BacktestPlot <- function(backtest, ticker, model){
  backtest_df <- backtest@forecast[["VaR"]]

  backtest_df$date <- as.Date(rownames(backtest_df))
  rownames(backtest_df) <- NULL
  backtest_df$violation01 <- ifelse(backtest_df$realized < backtest_df$`alpha(1%)`, backtest_df$realized, NA)
  backtest_df$violation05 <- ifelse(backtest_df$realized < backtest_df$`alpha(5%)` & is.na(backtest_df$violation01), backtest_df$realized, NA)

  #browser()
  backtest_df |>
    e_charts(date, height = "200px") |>
    #e_dims(height = "200px") |>
    e_line(realized, name = "Realized Returns",
           smooth = FALSE,
           symbol = "none",
           lineStyle = list(color = custom_blue, width = 1)) |>
    e_line(`alpha(5%)`,
           symbol = "none",
           lineStyle = list(color = "#FFD700", width = 1),
           itemStyle = list(color = "#FFD700"),
           name = "VaR 5%") |>
    e_line(`alpha(1%)`,
           symbol = "none",
           lineStyle = list(color = "#FF4500", width = 1),
           itemStyle = list(color = "#FF4500"),
           name = "VaR 1%") |>

    e_scatter(violation05, name = "VaR Violations 5%",
              symbol_size = 5, itemStyle = list(color = "#FF8C00")) |>
    e_scatter(violation01, name = "VaR Violations 1%",
              symbol_size = 5, itemStyle = list(color = "#8B0000")) |>
    e_tooltip(trigger = "axis") |>
    e_legend(type = "plain",
             orient = "horizontal",
             left = "center",
             bottom = 0,
             itemWidth = 20,
             itemHeight = 10) |>
    e_grid(right = 5, left = 43, top = 75, bottom = 80) |>
    e_title(text = sprintf("%s - VaR Violations", ticker),
            subtext = sprintf("Model: %s", names(models[which(models == model)]))) |>
    e_show_loading()

}



#' @export
make_report <- function(backtest, info = F , type = NULL){

  makeDt <- function(n,v,a = NULL) data.table(alpha = a, name = n, value = v)
  table <- data.table()

  if(info){

    model = sprintf("%s-%s",
                    backtest@model[["spec"]]@model[["modeldesc"]][["vmodel"]],
                    backtest@model[["spec"]]@model[["modeldesc"]][["distribution"]])
    backtestLength = backtest@model[["forecast.length"]]
    refitWindow = backtest@model[["refit.window"]]

    table <- makeDt(
      c("Model:", "Backtest Length:", "Refit Window:"),
      c(model, backtestLength,  refitWindow)
    )

    } else {
      actual = backtest@forecast$VaR$realized
      test01 <- VaRTest(alpha = 0.01, VaR = backtest@forecast$VaR$`alpha(1%)` ,actual = actual )
      test05 <- VaRTest(alpha = 0.05, VaR = backtest@forecast$VaR$`alpha(5%)` ,actual = actual )

      if(type == "exceed"){

        table <- rbind(makeDt(
          c("alpha:", "Expected Exceed:", "Actual VaR Exceed:"),
          c("1%", test01$expected.exceed, test01$actual.exceed),
          "alpha(1%)"
        ),
        makeDt(
          c("alpha:", "Expected Exceed:", "Actual VaR Exceed:"),
          c("5%", test01$expected.exceed, test01$actual.exceed),
          "alpha(5%)"
        ))
      } else if (type == "uc"){

        table <- rbind(
          makeDt(
            c("Null-Hypothesis:", "LR.uc Statistic:", "LR.uc Critical:", "LR.uc p-value:", "Decision:"),
            c(test01$uc.H0, test01$uc.LRstat, test01$uc.critical,test01$uc.LRp,test01$uc.Decision ),
          "alpha(1%)"
          ),
          makeDt(
            c("Null-Hypothesis:", "LR.uc Statistic:", "LR.uc Critical:", "LR.uc p-value:", "Decision:"),
            c(test05$uc.H0, test05$uc.LRstat, test05$uc.critical,test05$uc.LRp,test05$uc.Decision ),
          "alpha(5%)"
          )

        )

      } else {
        table <- rbind(
          makeDt(
            c("Null-Hypothesis:", "LR.cc Statistic:", "LR.cc Critical:", "LR.cc p-value:", "Decision:"),
            c(test01$cc.H0, test01$cc.LRstat, test01$cc.critical,test01$cc.LRp,test01$cc.Decision ),
            "alpha(1%)"
          ),
          makeDt(
            c("Null-Hypothesis:", "LR.cc Statistic:", "LR.cc Critical:", "LR.cc p-value:", "Decision:"),
            c(test05$cc.H0, test05$cc.LRstat, test05$cc.critical,test05$cc.LRp,test05$cc.Decision ),
            "alpha(5%)"
          )

        )

      }
    } # end table making

  table_group <- table
  columns <- NULL
  details <- NULL

  if(info){
    columns <- list( name = nameColDef, value = valueColDef)
  } else {
    columns <- list( alpha = nameColDef)
    table_group <- unique(table[, "alpha", drop = FALSE])
    details <- function(index){
      selected_alpha <- table_group$alpha[index]
      details_data <- table[table$alpha == selected_alpha, ]

      tags$div(style = "padding: 1rem",
               reactable(details_data,
                         outlined = T,
                         compact = T,
                         columns = list(
                           alpha = colDef(show = F),
                           name = nameColDef,
                           value = valueColDef
                         ),
                         theme = noHeader
                         )
               )
    }
  }

  #if(type == "exceed") browser()

  table_r <- reactable(table_group,
                       compact = T,
                       highlight = TRUE,
                       columns = columns,
                       theme = noHeader,
                       details = details)

return(table_r)

  }

# table styles
nameColDef <- colDef(
  style = list(
    textAlign = "left",
    fontWeight = "600",
    fontSize = "0.7rem",
    lineHeight = "1.375rem"
  )
)

valueColDef <- colDef(style = list(
  textAlign = "right",
  fontSize = "0.7rem"
))

noHeader <- reactableTheme(headerStyle = list(display = "none"),
                           cellPadding = "4px 8px")
