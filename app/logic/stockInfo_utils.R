box::use(
  TTR[stockSymbols],
  #spsComps[addLoader],
  shiny[getShinyOption, shinyOptions],
  lubridate[ years, `%m-%`]
)

#' @export
get_symbols <- function(last_update){

  SETT <- getShinyOption("SETTINGS")

  last <- as.Date(SETT$symbols$last_update)
  time_passed <-  Sys.Date() - last

  if(time_passed > 1){

    symbols <- TTR::stockSymbols(exchange = "NASDAQ")[, c(1, 2, 8, 11, 13)]
    saveRDS(symbols, file = "app/files/symbols.rds")

    SETT$symbols$last_update <- as.character(Sys.Date())
    shinyOptions(SETTINGS = SETT)

  } else {
    symbols <- readRDS(file = "app/files/symbols.rds")
  }

  return(symbols)
}

#' @export
make_list <- function(dt){
  s <- dt$Symbol
  n <- dt$Name
  n <- sub("-.*", "", n)
  n <- paste(s,n, sep = " - ")
  names(s) <- n
  s
}

#' @export
#' x years ago
years_ago <- function(years) {
  date <- Sys.Date()
  date_x_years_ago <- date %m-% years(years)
  return(date_x_years_ago)
}


#' @export
# spinner <- addLoader$new(
#   target_selector =  x,
#   type = "dual-ring",
#   height = "20px",
#   color = "#757575"
# )
