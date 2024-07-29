box::use(
  shiny[tags, HTML],
  quantmod[dailyReturn, Cl],
  bslib[card_header],
  zoo[coredata, index],
  stats[quantile, acf, pacf, Box.test, t.test],
  data.table[data.table],
  tseries[jarque.bera.test, adf.test],
  FinTS[ArchTest],
  shinyWidgets[switchInput],
  reactable[reactable, reactableTheme,colDef],
  echarts4r[e_bar, e_x_axis,e_datazoom,e_grid,e_legend,e_tooltip,e_title,e_line,e_charts,e_density]
)

box::use(app / styles / colors[custom_blue])

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
           lineStyle = list(color = custom_blue, width = 1),
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
           lineStyle = list(color = custom_blue, width = 1),
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


#' @export
#' compute default lag
get_lag <- function(returns){
  N <- length(returns)
  m <- 1
  lag <- floor(10 * log10(N / m))
  lag <- min(lag, N - 1)
  return(lag)
}

#' @export
#' ACF/PACF plot function
make_cf_plot <- function(returns, sq_returns, lag = NULL, ticker) {

  # Compute ACF and PACF of daily returns
  acf_values <- acf(returns, lag.max = lag, plot = FALSE)
  pacf_values <- pacf(returns, lag.max = lag, plot = FALSE)

  # Compute ACF and PACF of squared returns
  acf_sq_values <- acf(sq_returns, lag.max = lag, plot = FALSE)
  pacf_sq_values <- pacf(sq_returns, lag.max = lag, plot = FALSE)

  # Calculate the confidence intervals
  n <- length(returns)  # Number of observations
  conf_interval <- 1.96 / sqrt(n)

  # Create data table for ACF values with confidence intervals for returns
  acf_dt <- data.table(
    lag = c(0, acf_values$lag[-1, 1, 1]),  # Include the first lag (autocorrelation at lag 0)
    acf = c(NA, acf_values$acf[-1, 1, 1]),
    lower = c(-conf_interval, rep(-conf_interval, length(acf_values$lag[-1, 1, 1]))),
    upper = c(conf_interval, rep(conf_interval, length(acf_values$lag[-1, 1, 1])))
  )

  # Create data table for PACF values with confidence intervals for returns
  pacf_dt <- data.table(
    lag = c(0, pacf_values$lag[-1, 1, 1]),  # Include the first lag (partial autocorrelation at lag 0)
    pacf = c(NA, pacf_values$acf[-1, 1, 1]),
    lower = c(-conf_interval, rep(-conf_interval, length(pacf_values$lag[-1, 1, 1]))),
    upper = c(conf_interval, rep(conf_interval, length(pacf_values$lag[-1, 1, 1])))
  )

  # Create data table for ACF values with confidence intervals for squared returns
  acf_sq_dt <- data.table(
    lag = c(0, acf_sq_values$lag[-1, 1, 1]),  # Include the first lag (autocorrelation at lag 0)
    acf = c(NA, acf_sq_values$acf[-1, 1, 1]),
    lower = c(-conf_interval, rep(-conf_interval, length(acf_sq_values$lag[-1, 1, 1]))),
    upper = c(conf_interval, rep(conf_interval, length(acf_sq_values$lag[-1, 1, 1])))
  )

  # Create data table for PACF values with confidence intervals for squared returns
  pacf_sq_dt <- data.table(
    lag = c(0, pacf_sq_values$lag[-1, 1, 1]),  # Include the first lag (partial autocorrelation at lag 0)
    pacf = c(NA, pacf_sq_values$acf[-1, 1, 1]),
    lower = c(-conf_interval, rep(-conf_interval, length(pacf_sq_values$lag[-1, 1, 1]))),
    upper = c(conf_interval, rep(conf_interval, length(pacf_sq_values$lag[-1, 1, 1])))
  )

  bar_color <- 'gray'
  bar_width <- 1
  conf_color <- custom_blue

  # ACF Plot for Daily Returns
  acf_plot <- acf_dt |>
    e_charts(lag) |>
    e_bar(acf, name = "ACF", itemStyle = list(color = bar_color, barWidth = bar_width)) |>
    e_line(lower, name = "Confidence Interval", lineStyle = list(type = "dotted", color = conf_color), smooth = FALSE, symbol = "none") |>
    e_line(upper, name = "Confidence Interval", lineStyle = list(type = "dotted", color = conf_color), smooth = FALSE, symbol = "none") |>
    e_title("Autocorrelation Function", subtext = sprintf("%s - Daily Returns", ticker)) |>
    e_tooltip(trigger = "axis") |>
    e_x_axis(name = "Lag", nameLocation = "middle", nameGap = 28, min = 0, max = lag) |>
    e_grid(bottom = "15%") |>
    e_legend(FALSE)

  # PACF Plot for Daily Returns
  pacf_plot <- pacf_dt |>
    e_charts(lag) |>
    e_bar(pacf, name = "PACF", itemStyle = list(color = bar_color, barWidth = bar_width)) |>
    e_line(lower, name = "Confidence Interval", lineStyle = list(type = "dotted", color = conf_color), smooth = FALSE, symbol = "none") |>
    e_line(upper, name = "Confidence Interval", lineStyle = list(type = "dotted", color = conf_color), smooth = FALSE, symbol = "none") |>
    e_title("Partial Autocorrelation Function", subtext = sprintf("%s - Daily Returns", ticker)) |>
    e_tooltip(trigger = "axis") |>
    e_x_axis(name = "Lag", nameLocation = "middle", nameGap = 28, min = 0, max = lag) |>
    e_grid(bottom = "15%") |>
    e_legend(FALSE)

  # ACF Plot for Squared Returns
  acf_sq_plot <- acf_sq_dt |>
    e_charts(lag) |>
    e_bar(acf, name = "ACF", itemStyle = list(color = bar_color, barWidth = bar_width)) |>
    e_line(lower, name = "Confidence Interval", lineStyle = list(type = "dotted", color = conf_color), smooth = FALSE, symbol = "none") |>
    e_line(upper, name = "Confidence Interval", lineStyle = list(type = "dotted", color = conf_color), smooth = FALSE, symbol = "none") |>
    e_title("Autocorrelation Function", subtext = sprintf("%s - Squared Returns", ticker)) |>
    e_tooltip(trigger = "axis") |>
    e_x_axis(name = "Lag", nameLocation = "middle", nameGap = 28, min = 0, max = lag) |>
    e_grid(bottom = "15%") |>
    e_legend(FALSE)

  # PACF Plot for Squared Returns
  pacf_sq_plot <- pacf_sq_dt |>
    e_charts(lag) |>
    e_bar(pacf, name = "PACF", itemStyle = list(color = bar_color, barWidth = bar_width)) |>
    e_line(lower, name = "Confidence Interval", lineStyle = list(type = "dotted", color = conf_color), smooth = FALSE, symbol = "none") |>
    e_line(upper, name = "Confidence Interval", lineStyle = list(type = "dotted", color = conf_color), smooth = FALSE, symbol = "none") |>
    e_title("Partial Autocorrelation Function", subtext = sprintf("%s - Squared Returns", ticker)) |>
    e_tooltip(trigger = "axis") |>
    e_x_axis(name = "Lag", nameLocation = "middle", nameGap = 28, min = 0, max = lag) |>
    e_grid(bottom = "15%") |>
    e_legend(FALSE)



  return(list(
    acf = acf_plot,
    pacf = pacf_plot,
    acf_sq = acf_sq_plot,
    pacf_sq = pacf_sq_plot
  ))

}

#' @export
test_header <- function(title, switchId) {
  card_header(
    tags$div(class = "d-flex justify-content-between align-items-center",
             tags$div(style = "padding-top: 5px;", title),
             switchInput(
               inputId = switchId,
               labelWidth = "100%",
               size = "mini",
               inline = TRUE,
               value = TRUE,
               onLabel = "R",
               offLabel = HTML("R<sup>2</sup>"),
               onStatus = "default",
               width = "auto"
             )
    )
  )
}



make_test_dt <- function(nh, dt, pv, sl, ps, rs){
  data.table(
    name = c("Null Hypotesis", "Data", "p-value", "Significance level","Pass", "Result"),
    value = c(nh,dt, pv, sl, ps, rs)
  )
}

format_test_table <- function(dt) {
  reactable(dt,
            columns = list(
              name = colDef(
                #maxWidth = 80,
                style = list(
                  textAlign = "left",
                  fontWeight = "600",
                  fontSize = "0.7rem",
                  lineHeight = "1.375rem"
                )
              ),
              value = colDef(
                html = TRUE,
                style = function(value) {
                  color <- if (value == "Success") "green" else if (value == "Fail") "red"
                  list(
                    textAlign = "right",
                    fontSize = "0.7rem",
                    color = color
                  )
                }
              )
            ),
            compact = TRUE,
            theme = reactableTheme(
              headerStyle = list(display = "none"),
              cellPadding = "4px 8px"
            )
  )
}



#' @export
make_box_table <- function(returns = NULL, sq_returns = NULL, lag, type){

  series <- if(!is.null(returns)) "Returns" else HTML("Returns<sup>2</sup>")
  data <- if(!is.null(returns)) returns else sq_returns

  test <- Box.test(data, lag, type)
  data <- series
  nullH <- "No autocorrelation"
  pvalue = test$p.value
  significance_level <- 0.05
  pass <- if(pvalue < significance_level) "Fail" else "Success"
  result <- if(pvalue < significance_level) "Daily returns are not independently distributed" else "Daily returns are independently distributed"

  dt <- make_test_dt(nullH, data,
                     substr(pvalue, 1, 10),
                     significance_level, pass, result)

  format_test_table(dt)

}

#' @export
make_jb_table <- function(returns = NULL, sq_returns = NULL){

  series <- if(!is.null(returns)) "Returns" else HTML("Returns<sup>2</sup>")
  data <- if(!is.null(returns)) returns else sq_returns

  test <- jarque.bera.test(data)
  nullH <- "Normal distribution"
  data <- series
  pvalue = test$p.value
  significance_level <- 0.05
  pass <- if(pvalue < significance_level) "Fail" else "Success"
  result <- if(pvalue < significance_level) "Data does not follow a normal distribution" else "Data follows a normal distribution"

  dt <- make_test_dt(nullH, data,
                     substr(pvalue, 1, 10),
                     significance_level, pass, result)

  format_test_table(dt)

}

#' @export
make_adf_table <- function(returns = NULL, sq_returns = NULL, lag){

  series <- if(!is.null(returns)) "Returns" else HTML("Returns<sup>2</sup>")
  data <- if(!is.null(returns)) returns else sq_returns

  test <- suppressWarnings(adf.test(data, k=lag))
  nullH <- "Non-stationary"
  data <- series
  pvalue = test$p.value
  significance_level <- 0.05
  pass <- if(pvalue < significance_level) "Fail" else "Success"
  result <- if(pvalue < significance_level) "The time series is stationary" else "The time series is non-stationary"

  dt <- make_test_dt(nullH, data,
                     substr(pvalue, 1, 10),
                     significance_level, pass, result)

  format_test_table(dt)

}

#' @export
make_t_table <- function(returns = NULL, sq_returns = NULL, lag){

  series <- if(!is.null(returns)) "Returns" else HTML("Returns<sup>2</sup>")
  data <- if(!is.null(returns)) returns else sq_returns

  test <- suppressWarnings(t.test(data, k=lag))
  nullH <- "Mean = 0"
  data <- series
  pvalue = test$p.value
  significance_level <- 0.05
  pass <- if(pvalue < significance_level) "Fail" else "Success"
  result <- if(pvalue < significance_level) "The mean of the data is not zero" else "The mean of the data is zero"

  dt <- make_test_dt(nullH, data,
                     substr(pvalue, 1, 10),
                     significance_level, pass, result)

  format_test_table(dt)

}

#' @export
make_arch_table <- function(returns = NULL, sq_returns = NULL){

  series <- if(!is.null(returns)) "Returns" else HTML("Returns<sup>2</sup>")
  data <- if(!is.null(returns)) returns else sq_returns

  test <- suppressWarnings(ArchTest(data))
  nullH <- "No ARCH effects"
  data <- series
  pvalue = test$p.value
  significance_level <- 0.05
  pass <- if(pvalue < significance_level) "Fail" else "Success"
  result <- if(pvalue < significance_level) "There are ARCH effects in the data" else "There are no ARCH effects in the data"

  dt <- make_test_dt(nullH, data,
                     substr(pvalue, 1, 10),
                     significance_level, pass, result)

  format_test_table(dt)

}

