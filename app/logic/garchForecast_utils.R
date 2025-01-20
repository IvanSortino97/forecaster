box::use(
  shiny[uiOutput ,sliderInput,conditionalPanel, tags, numericInput, selectizeInput],
  bslib[navset_underline, nav_panel,card, card_header,card_body, card_footer],
  data.table[data.table, rbindlist],
  dplyr[mutate],
  rugarch[sigma],
  utils[tail],
  zoo[index],
  reactable[reactable, colDef,colFormat, reactableOutput, reactableTheme],
  echarts4r[echarts4rOutput, e_title,e_grid,e_legend,e_tooltip,e_line,e_charts,e_mark_line]
)

box::use(
  app / logic / general_utils[subtitle],
  app / logic / garchFit_utils[models, body_subtitle],
  app / styles / colors[custom_blue, alpha_1, alpha_5, violation_1, violation_5],

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
                             nav_panel("Results",
                                       body_subtitle("Forecast results", "margin-bottom: 0;"),
                                       reactableOutput(ns(paste0(model,"forecastRaw"))),

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

#' @export
create_forecast_df <- function(fit, returns, forecasted_sd, n_ahead) {
  
  cond_sd <- sigma(fit)  # Conditional standard deviations
  time_index <- index(returns)
  last_day = tail(time_index,1)
          
  fit_df <- data.frame(
    Time = time_index,
    ConditionalSD = cond_sd,
    C_Upper_95 = cond_sd * 1.96,
    C_Upper_99 = cond_sd * 2.576
  )

  fit_df <- tail(fit_df, min(n_ahead + 5, floor(n_ahead * 1.2)))

  forecast_df <- data.frame(
    Time = seq(last_day + 1, by = "day", length.out = n_ahead),
    ForecastedSD = forecasted_sd,
    F_Upper_95 = forecasted_sd * 1.96,
    F_Upper_99 = forecasted_sd * 2.576
  )

  merged_df <- rbindlist(list(fit_df, forecast_df), fill = T )

  return(merged_df)
} 

#' @export
make_forecast_plot <- function(forecast_df, model, n_ahead){

  last_day = tail(forecast_df[!is.na(ConditionalSD), Time],1)
  
  forecast_df |> 
    e_charts(Time) |>
    e_line(ConditionalSD, name = "Historical Volatility",
           smooth = FALSE,
           symbol = "none",
           itemStyle = list(color = custom_blue, width = 1)) |>
    e_line(C_Upper_95, name = "Historical 95% CI", 
           smooth = FALSE,
           symbol = "none",
           lineStyle = list(type = "dashed"), 
           itemStyle = list(color = alpha_5)) |>
    e_line(C_Upper_99, name = "Historical 99% CI", 
           smooth = FALSE,
           symbol = "none",
           lineStyle = list(type = "dashed"), 
           itemStyle = list(color = alpha_1)) |>
    e_line(ForecastedSD, name = "Forecasted Volatility") |>
    e_line(F_Upper_95, name = "Forecast 95% CI", 
           lineStyle = list(type = "dashed"),
           itemStyle = list(color = violation_5)) |>
    e_line(F_Upper_99, name = "Forecast 99% CI", 
           lineStyle = list(type = "dashed"),
           itemStyle = list(color = violation_1)) |>
    e_tooltip(trigger = "axis") |>
    e_mark_line(data = list(xAxis = last_day), title = paste0("Begin forecast: ", last_day + 1)) |>
    e_legend(type = "plain",
             orient = "horizontal",
             left = "center",
             bottom = 0,
             itemWidth = 20,
             itemHeight = 10) |>
    e_grid(right = 5, left = 43, top = 75, bottom = 80) |>
    e_title(sprintf("%s Volatility Forecast with %d-Day Ahead", model, n_ahead))

}

#' @export
make_raw_table <- function(forecast_df){

  reactable(
  forecast_df |> dplyr::mutate(across(-Time, ~round(., 4))),
  striped = FALSE,
  compact = TRUE,
  highlight = TRUE,
  bordered = TRUE,
  pagination = FALSE,
  height = 400,
  defaultColDef = colDef(
    style = list(fontSize = "0.7rem"),
    headerStyle = list(fontSize = "0.8rem")
  ),
  columns = list(
    Time = colDef(
      name = "Date",
      sticky = "left",
      format = colFormat(date = TRUE),
      style = function(value, index) {
        if (!is.na(forecast_df$ForecastedSD[index])) {
          list(
            background = "#ffeb3b50",
            textAlign = "left",
            fontWeight = "600",
            fontSize = "0.7rem",
            lineHeight = "1.375rem"
          )
        } else {
          list(
            textAlign = "left",
            fontWeight = "600",
            fontSize = "0.7rem",
            lineHeight = "1.375rem"
          )
        }
      }
    ),
    ForecastedSD = colDef(
      name = "Forecasted Volatility",
      style = function(value) {
        if (!is.na(value)) {
          list(
            background = "#ffeb3b50",
            textAlign = "right",
            fontSize = "0.7rem"
          )
        } else {
          list(
            textAlign = "right",
            fontSize = "0.7rem"
          )
        }
      }
    ),
    F_Upper_95 = colDef(
      name = "Forecast 95% CI",
      style = function(value) {
        if (!is.na(value)) {
          list(
            background = "#ffeb3b50",
            textAlign = "right",
            fontSize = "0.7rem"
          )
        } else {
          list(
            textAlign = "right",
            fontSize = "0.7rem"
          )
        }
      }
    ),
    F_Upper_99 = colDef(
      name = "Forecast 99% CI",
      style = function(value) {
        if (!is.na(value)) {
          list(
            background = "#ffeb3b50",
            textAlign = "right",
            fontSize = "0.7rem"
          )
        } else {
          list(
            textAlign = "right",
            fontSize = "0.7rem"
          )
        }
      }
    ),
    ConditionalSD = colDef(
      name = "Historical Volatility",
      style = function(value, index) {
        if (!is.na(forecast_df$ForecastedSD[index])) {
          list(
            background = "#ffeb3b50",
            textAlign = "right",
            fontSize = "0.7rem"
          )
        } else {
          list(
            textAlign = "right",
            fontSize = "0.7rem"
          )
        }
      }
    ),
    C_Upper_95 = colDef(
      name = "Historical 95% CI",
      style = function(value, index) {
        if (!is.na(forecast_df$ForecastedSD[index])) {
          list(
            background = "#ffeb3b50",
            textAlign = "right",
            fontSize = "0.7rem"
          )
        } else {
          list(
            textAlign = "right",
            fontSize = "0.7rem"
          )
        }
      }
    ),
    C_Upper_99 = colDef(
      name = "Historical 99% CI",
      style = function(value, index) {
        if (!is.na(forecast_df$ForecastedSD[index])) {
          list(
            background = "#ffeb3b50",
            textAlign = "right",
            fontSize = "0.7rem"
          )
        } else {
          list(
            textAlign = "right",
            fontSize = "0.7rem"
          )
        }
      }
    )
  )
)}
