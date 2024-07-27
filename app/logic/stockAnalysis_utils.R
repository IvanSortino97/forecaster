box::use(
  quantmod[dailyReturn, Cl],
  zoo[coredata, index],
  stats[quantile],
  data.table[data.table],
  reactable[reactable, reactableTheme,colDef],
  echarts4r[e_x_axis,e_datazoom,e_grid,e_legend,e_tooltip,e_title,e_line,e_charts,e_density]
)


#' @export
get_returns <- function(data_xts){

  returns <- dailyReturn(Cl(data_xts))
  returns_dt <- data.table(date = index(returns), coredata(returns))
  squared_returns <- returns^2
  names(squared_returns) <- "squared.returns"
  squared_returns_dt <- data.table(date = index(squared_returns), coredata(squared_returns))
  return(list(
    returns = returns,
    returns_dt = returns_dt,
    squared_returns = squared_returns,
    squared_returns_dt = squared_returns_dt
  ))
}

#' @export
make_analysis_plots <- function(dt, ticker) {
  # Daily Returns Plot
  daily_returns_plot <- dt$returns_dt |>
    e_charts(date) |>
    e_line(daily.returns,
           smooth = FALSE,
           symbol = "none",
           lineStyle = list(color = "#5756ff", width = 1),
           legend = FALSE) |>
    e_title(paste0(ticker, " - Daily Returns")) |>
    e_tooltip(trigger = "axis") |>
    e_legend(FALSE) |>
    e_grid(right = 5, left = 5) |>
    e_datazoom(type = "slider", x_axis_index = 0, toolbox = TRUE)

  # Squared Daily Returns Plot
  squared_returns_plot <- dt$squared_returns_dt |>
    e_charts(date) |>
    e_line(squared.returns,
           smooth = FALSE,
           symbol = "none",
           lineStyle = list(color = "#5756ff", width = 1),
           legend = FALSE) |>
    e_title(paste0(ticker, " - Squared Daily Returns")) |>
    e_tooltip(trigger = "axis") |>
    e_legend(FALSE) |>
    e_grid(right = 5, left = 5) |>
    e_datazoom(type = "slider", x_axis_index = 0, toolbox = TRUE)

  # Returns Distribution Plot
  returns_distribution_plot <- dt$returns_dt |>
    e_charts() |>
    e_density(daily.returns) |>
    e_title(paste0(ticker, " - Returns Distribution")) |>
    e_tooltip(trigger = "axis") |>
    e_legend(FALSE) |>
    e_grid(right = 5, left = 5, bottom = 5) |>
    e_x_axis(name = "Returns")

  list(daily_returns_plot = daily_returns_plot,
       squared_returns_plot = squared_returns_plot,
       returns_distribution_plot = returns_distribution_plot)
}

compute_stats <- function(ts, type){

  from = NULL
  to = NULL
  cd <- coredata(ts)
  if(type == "prices"){
    from <- index(ts)[1] |> format("%d-%m-%Y")
    to <- index(ts)[length(index(ts))] |> format("%d-%m-%Y")
    cd <- coredata(Cl(ts))
  }

  quartiles <- quantile(cd, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)
  f <- function(x) as.character(x) |> substr(1,7)

  min_val <- min(cd) |> f()
  Q1 = quartiles[1] |> f()
  Median = quartiles[2] |> f()
  mean_val <- mean(cd) |> f()
  Q3 = quartiles[3] |> f()
  max_val <- max(cd) |> f()

  dt <- data.table(
    name = c("Min.", "Q1", "Median", "Mean", "Q3", "Max."),
    value = c(min_val, Q1, Median, mean_val, Q3, max_val)
  )

  dt <- reactable(dt,
    columns = list(value = colDef(
      style = list(
        textAlign = "right",
        fontWeight = "600",
        fontSize = "0.9rem",
        lineHeight = "1.375rem"
      )

    )),
    compact = TRUE,
    theme = reactableTheme(
      headerStyle = list(display = "none"),
      cellPadding = "4px 8px"
    )
  )
  return(dt)
}

#' @export
make_price_table <- function(xts, ret){

 stat_prices <- compute_stats(xts, "prices")
 stat_returns <- compute_stats(ret, "returns")

 return(list(
   table_prices = stat_prices,
   table_returns = stat_returns
 ))

}
