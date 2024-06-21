box::use(
  TTR[stockSymbols],
  spsComps[addLoader],
  shiny[...],
)

#' @export
get_symbols <- function(last_update){

  SETT <- getShinyOption("SETTINGS")

  last <- as.Date(SETT$symbols$last_update)
  time_passed <-  Sys.Date() - last

  if(time_passed > 1){

    symbols <- stockSymbols(exchange = "NASDAQ")[, c(1, 2, 7, 8, 11, 13)]
    saveRDS(symbols, file = "app/files/symbols.rds")

    SETT$symbols$last_update <- as.character(Sys.Date())
    shinyOptions(SETTINGS = SETT)

  } else {
    symbols <- readRDS(file = "app/files/symbols.rds")
  }

  return(symbols)
}

#' @export
# spinner <- addLoader$new(
#   target_selector =  x,
#   type = "dual-ring",
#   height = "20px",
#   color = "#757575"
# )
