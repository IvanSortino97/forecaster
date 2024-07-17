box::use(
  TTR[stockSymbols],
  #spsComps[addLoader],
  shiny[getShinyOption, shinyOptions],
  lubridate[ years, `%m-%`],
  rvest[html_node,html_table, read_html]
)

#' @export
get_symbols <- function(last_update){

  SETT <- getShinyOption("SETTINGS")

  last <- as.Date(SETT$symbols$last_update)
  time_passed <-  Sys.Date() - last

  if(time_passed > 1){

    symbols <- TTR::stockSymbols(exchange = "NASDAQ")[, c(1, 2, 13, 8, 11)]
    saveRDS(symbols, file = "app/files/symbols.rds")

    SETT$symbols$last_update <- as.character(Sys.Date())
    shinyOptions(SETTINGS = SETT)

  } else {
    symbols <- readRDS(file = "app/files/symbols.rds")
  }

  return(symbols)
}

#' @export
get_sp500 <- function(){


  SETT <- getShinyOption("SETTINGS")

  last <- as.Date(SETT$symbols$last_update_sp500)
  time_passed <-  Sys.Date() - last

  if (time_passed > 1) {
    url <- "https://en.wikipedia.org/wiki/List_of_S%26P_500_companies"

    webpage <- read_html(url)
    sp500_table <- webpage |>
      html_node(xpath = '//*[@id="constituents"]') |>
      html_table(fill = TRUE)

    # colnames(sp500_table) <- c(
    #   "Symbol",
    #   "Name",
    #   "SEC filings",
    #   "Sector",
    #   "Sub_Industry",
    #   "Headquarters",
    #   "Date_Added",
    #   "CIK"
    # )

    # Drop the "SEC filings" column
    sp500_table <- sp500_table[,-3]

    symbols <- sp500_table[,1]
    saveRDS(symbols, file = "app/files/symbolsSP500.rds")

    SETT$symbols$last_update_sp500 <- as.character(Sys.Date())
    shinyOptions(SETTINGS = SETT)

  } else {
    symbols <- readRDS(file = "app/files/symbolsSP500.rds")
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
