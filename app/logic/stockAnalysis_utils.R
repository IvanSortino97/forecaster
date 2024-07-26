box::use(
  quantmod[dailyReturn, Cl],
  zoo[coredata, index],
  data.table[data.table]
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
