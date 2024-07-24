box::use(
  TTR[stockSymbols],
  quantmod[getSymbols],
  data.table[data.table],
  #spsComps[addLoader],
  shiny[getShinyOption, shinyOptions],
  lubridate[ years, `%m-%`],
  rvest[html_nodes, html_text, read_html, html_table, ],
  reactable[reactable, reactableTheme,colDef]
)

#' @export
get_symbols <- function(){

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
get_data <- function(ticker, from){

  SETT <- getShinyOption("SETTINGS")

  last <- as.Date(SETT$symbols$last_update_data$date)
  time_passed <-  Sys.Date() - last
  last_ticker <- SETT$symbols$last_update_data$ticker

  if (time_passed > 1 || ticker != last_ticker) {
    data <- getSymbols(
      Symbols = ticker,
      src = "yahoo",
      auto.assign = FALSE,
      from = from
    )
    saveRDS(data, file = "app/files/cached_data.rds")

    SETT$symbols$last_update_data$date <- as.character(Sys.Date())
    SETT$symbols$last_update_data$ticker <- as.character(ticker)
    shinyOptions(SETTINGS = SETT)

  } else {
    message(sprintf("Reading cached '%s' data",ticker))
    data <- readRDS(file = "app/files/cached_data.rds")
  }

  return(data)
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
      html_nodes(xpath = '//*[@id="constituents"]') |>
      html_table(fill = TRUE)

    symbols <- sp500_table[[1]][,1]
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
#' Scraper for yahoo finance infos
scrape_yahoo_finance <- function(ticker) {
  # Define the URL for Yahoo Finance
  url <- paste0("https://finance.yahoo.com/quote/", ticker)
  webpage <- read_html(url)

  # Define the XPath expressions for scraping
  scrapelist <- list(
    description = '//p[@class="yf-1xu2f9r" and not(@title)]',
    stockPrice = '//*[@data-field="regularMarketOpen"]',
    fiftyTwoWeekRange = '//*[@data-field="fiftyTwoWeekRange"]',
    volume = '//*[@data-field="regularMarketVolume"]',
    averageVolume  = '//*[@data-field="averageVolume"]',
    peRatioTTM = '//li[span[contains(text(), "PE Ratio (TTM)")]]//fin-streamer[@data-field="trailingPE"]/@data-value',
    epsTTM = '//li[span[contains(text(), "EPS (TTM)")]]//fin-streamer[@data-field="trailingPE"]/@data-value',
    earningsDate = '//li[.//span[contains(text(),"Earnings Date")]]//span[contains(@class, "value")]',
    marketCap = '//li[p[contains(text(), "Market Cap")]]/p[@class="value yf-1n4vnw8"]',
    enterpriseValue = '//li[p[contains(text(), "Enterprise Value ")]]/p[@class="value yf-1n4vnw8"]',
    trailingPE = '//li[p[contains(text(), "Trailing P/E ")]]/p[@class="value yf-1n4vnw8"]',
    forwardPE = '//li[p[contains(text(), "Forward P/E ")]]/p[@class="value yf-1n4vnw8"]',
    pegRatio = '//li[p[contains(text(), "PEG Ratio (5yr expected) ")]]/p[@class="value yf-1n4vnw8"]',
    priceSale = '//li[p[contains(text(), "Price/Sales  (ttm)")]]/p[@class="value yf-1n4vnw8"]',
    priceBook = '//li[p[contains(text(), "Price/Book  (mrq)")]]/p[@class="value yf-1n4vnw8"]',
    enterpriseValueRevenue = '//li[p[contains(text(), "Enterprise Value/Revenue ")]]/p[@class="value yf-1n4vnw8"]',
    enterpriseValueEBITDA = '//li[p[contains(text(), "Enterprise Value/EBITDA ")]]/p[@class="value yf-1n4vnw8"]',
    profitMargin = '//li[p[contains(text(), "Profit Margin")]]/p[@class="value yf-lc8fp0"]',
    returnOnAssets = '//li[p[contains(text(), "Return on Assets  (ttm)")]]/p[@class="value yf-lc8fp0"]',
    returnOnEquity = '//li[p[contains(text(), "Return on Equity  (ttm)")]]/p[@class="value yf-lc8fp0"]',
    revenue = '//li[p[contains(text(), "Revenue  (ttm)")]]/p[@class="value yf-lc8fp0"]',
    netIncome = '//li[p[contains(text(), "Net Income Avi to Common  (ttm)")]]/p[@class="value yf-lc8fp0"]',
    dilutedEPS = '//li[p[contains(text(), "Diluted EPS  (ttm)")]]/p[@class="value yf-lc8fp0"]',
    totalCash = '//li[p[contains(text(), "Total Cash  (mrq)")]]/p[@class="value yf-lc8fp0"]',
    totalDebtEquity = '//li[p[contains(text(), "Total Debt/Equity  (mrq)")]]/p[@class="value yf-lc8fp0"]',
    leveredFreeCashFlow = '//li[p[contains(text(), "Levered Free Cash Flow  (ttm)")]]/p[@class="value yf-lc8fp0"]'
  )

  # Function to extract and clean the data
  extract_data <- function(xpath) {

    data <- webpage |>
      html_nodes(xpath = xpath) |>
      html_text()
    data <- gsub(",", "", data)
    if (length(data) == 0) {
      return(NA)
    }
    return(data[1])
  }

  return(lapply(scrapelist, extract_data))
}

#' @export
stat_table <- function(dt, type){
  dt <- switch (type,
    "valuation" = data.table(name = c(
      "Trailing P/E:",
      "Forward P/E:",
      "PEG Ratio:",
      "Price/Sales:",
      "Price/Book:",
      "Enterprise Value/Revenue:",
      "Enterprise Value/EBITDA:"
    ),
    value = c(
      dt$trailingPE,
      dt$forwardPE,
      dt$pegRatio,
      dt$priceSale,
      dt$priceBook,
      dt$enterpriseValueRevenue,
      dt$enterpriseValueEBITDA)
    ),
    "profitability" = data.table(name = c(
      "Profit Margin:",
      "Return on Assets:",
      "Return on Equity:",
      "Revenue:",
      "Net Income:",
      "Diluted EPS:"
    ),
    value = c(
      dt$profitMargin,
      dt$returnOnAssets,
      dt$returnOnEquity,
      dt$revenue,
      dt$netIncome,
      dt$dilutedEPS)
    ),
    "balancesheet" = data.table(name = c("Total Cash:",
                                         "Total Debt/Equity:",
                                         "Levered Free Cash Flow:"),
                                value = c(
                                  dt$totalCash,
                                  dt$totalDebtEquity,
                                  dt$leveredFreeCashFlow
                                ))
  )


dt |> reactable(
    columns = list(
      value = colDef(style = list(textAlign = "right", fontWeight = "bold"))),
    compact = TRUE,
    theme = reactableTheme(
      headerStyle = list(display = "none"),
      cellPadding = "4px 8px"
    )
  )
}


#' @export
# spinner <- addLoader$new(
#   target_selector =  x,
#   type = "dual-ring",
#   height = "20px",
#   color = "#757575"
# )
