box::use(
  data.table[data.table, as.data.table,],
  prophet[prophet, make_future_dataframe, ],
  stats[predict],
  echarts4r[e_charts,e_scatter, e_line, e_band2, e_title, e_tooltip, e_legend, e_grid, e_datazoom, e_show_loading] 

)

#' @export
make_prophet_model <- function(ticker, date, close, periods) {

  prophet_dt <- data.table( ds = as.Date(date), y = close)
  model <- prophet(prophet_dt, daily.seasonality = FALSE)
  future <- make_future_dataframe(model, periods = periods)
  forecast <- as.data.table(predict(model, future))
  forecast[, ds := as.Date(ds)]
  plot_dt <- merge(prophet_dt, forecast[, .(ds, yhat, yhat_lower, yhat_upper)], by = "ds", all = TRUE)

  plot <- plot_dt |>
  e_charts(ds) |>
  e_scatter(y, name = "Actual", color = "black", symbolSize = 3) |>
  e_line(yhat, name = "Forecast", symbol = "none", color = "#007bff") |>
  e_band2(yhat_lower, yhat_upper, name ="Confidence Interval", itemStyle = list(borderWidth = 0, opacity = 0.3)) |>
  
  #e_title(sprintf("%s forecast",ticker), subtext = "Model: Prophet") |>
  e_tooltip(trigger = "axis") |>
  e_datazoom(type = "slider", start = 50, end = 100) |>
  e_show_loading() |>
  e_grid(left = 35, right = 5, top = "10%") |>
  e_legend(top = 0, type = "scroll")

  return(list(dt = prophet_dt, model = model, forecast = forecast, plot = plot))

}