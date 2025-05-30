box::use(
  bslib[card_body, card],
  bsicons[bs_icon],
  echarts4r[e_dims, e_y_axis, e_x_axis,e_show_loading,e_toolbox,e_grid,e_datazoom,e_area, e_candle, echarts4rOutput, e_legend, e_title, e_tooltip, e_line, e_charts],
  TTR[stockSymbols],
  dplyr[filter, select],
  quantmod[getSymbols, Cl, dailyReturn],
  data.table[data.table],
  #spsComps[addLoader],
  shiny[getShinyOption, shinyOptions, tags,div, HTML, textOutput, uiOutput],
  shinyWidgets[switchInput,],
  lubridate[ years, `%m-%`],
  rvest[html_nodes, html_text, read_html, html_table, ],
  reactable[reactable, reactableTheme,colDef],
  utils[tail],
)
box::use(app / logic / general_utils[tryCatch_toaster],
         app / styles / colors[custom_blue])

#' @export
get_symbols <- function(){

  SETT <- getShinyOption("SETTINGS")

  last <- as.Date(SETT$symbols$last_update)
  time_passed <-  Sys.Date() - last
  symbols <- NULL

  if(time_passed > 1){

    tryCatch_toaster({

    symbols <- TTR::stockSymbols(exchange = "NASDAQ")[, c(1, 2, 13, 8, 11)]
    saveRDS(symbols, file = "app/files/symbols.rds")

    SETT$symbols$last_update <- as.character(Sys.Date())
    shinyOptions(SETTINGS = SETT)
    })

  } else {
    symbols <- readRDS(file = "app/files/symbols.rds")
  }

  return(symbols)
}

#' @export
get_data <- function(ticker, from){

  message(sprintf("Getting '%s' data", ticker))
  SETT <- getShinyOption("SETTINGS")

  last <- as.Date(SETT$symbols$last_update_data$date)
  time_passed <-  Sys.Date() - last
  last_ticker <- SETT$symbols$last_update_data$ticker
  data <- NULL

  if (time_passed > 1 || ticker != last_ticker) {

    tryCatch_toaster({

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
    })

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
    symbols <- NULL

    tryCatch_toaster({

    message("Fetching SP500 list")
    webpage <- read_html(url)
    sp500_table <- webpage |>
      html_nodes(xpath = '//*[@id="constituents"]') |>
      html_table(fill = TRUE)

    symbols <- sp500_table[[1]][,1]
    saveRDS(symbols, file = "app/files/symbolsSP500.rds")

    SETT$symbols$last_update_sp500 <- as.character(Sys.Date())
    shinyOptions(SETTINGS = SETT)

    })
  } else {
    symbols <- readRDS(file = "app/files/symbolsSP500.rds")
  }

  return(symbols)
}

#' @export
get_dailyReturns <- function(data_xts){
  dailyReturn(Cl(data_xts))
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
months_ago <- function(months) {
  date <- Sys.Date()
  date_x_months_ago <- date %m-% months(months)
  return(date_x_months_ago)
}

#' @export
#' Scraper for yahoo finance infos
scrape_yahoo_finance <- function(ticker) {
  # Define the URL for Yahoo Finance
  url <- paste0("https://finance.yahoo.com/quote/", ticker)
  message(sprintf('Scraping "%s"',url))
  webpage <- NULL

  # Define the XPath expressions for scraping
  scrapelist <- list(
    description = '//p[@class="yf-1600hyr" and not(@title)]',
    description2 = '//p[@class="yf-1ja4ll8" and not(@title)]',
    stockPrice = '//*[@data-field="regularMarketOpen"]',
    fiftyTwoWeekRange = '//*[@data-field="fiftyTwoWeekRange"]',
    volume = '//*[@data-field="regularMarketVolume"]',
    averageVolume  = '//*[@data-field="averageVolume"]',
    peRatioTTM = '//li[span[contains(text(), "PE Ratio (TTM)")]]//fin-streamer[@data-field="trailingPE"]/@data-value',
    epsTTM = '//li[span[contains(text(), "EPS (TTM)")]]//fin-streamer[@data-field="trailingPE"]/@data-value',
    earningsDate = '//li[.//span[contains(text(),"Earnings Date")]]//span[contains(@class, "value")]',
    marketCap = '//li[p[contains(text(), "Market Cap")]]/p[@class="value yf-i6syij"]',
    enterpriseValue = '//li[p[contains(text(), "Enterprise Value ")]]/p[@class="value yf-i6syij"]',
    trailingPE = '//li[p[contains(text(), "Trailing P/E ")]]/p[@class="value yf-i6syij"]',
    forwardPE = '//li[p[contains(text(), "Forward P/E ")]]/p[@class="value yf-i6syij"]',
    pegRatio = '//li[p[contains(text(), "PEG Ratio (5yr expected) ")]]/p[@class="value yf-i6syij"]',
    priceSale = '//li[p[contains(text(), "Price/Sales  (ttm)")]]/p[@class="value yf-i6syij"]',
    priceBook = '//li[p[contains(text(), "Price/Book  (mrq)")]]/p[@class="value yf-i6syij"]',
    enterpriseValueRevenue = '//li[p[contains(text(), "Enterprise Value/Revenue ")]]/p[@class="value yf-i6syij"]',
    enterpriseValueEBITDA = '//li[p[contains(text(), "Enterprise Value/EBITDA ")]]/p[@class="value yf-i6syij"]',
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

  ret <- NULL
  tryCatch_toaster({
    webpage <- read_html(url)
    ret <- lapply(scrapelist, extract_data)
  })

  return(ret)
}

#' @export
make_stat_table <- function(dt, type){

  dt <- switch (
    type,
    "valuation" = data.table(
      name = c(
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
        dt$enterpriseValueEBITDA
      )
    ),
    "profitability" = data.table(
      name = c(
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
        dt$dilutedEPS
      )
    ),
    "balancesheet" = data.table(
      name = c("Total Cash:",
               "Total Debt/Equity:",
               "Levered Free Cash Flow:"),
      value = c(dt$totalCash,
                dt$totalDebtEquity,
                dt$leveredFreeCashFlow)
    )
  )


  dt |> reactable(
    columns = list(
      name = colDef(
        style = list(
          textAlign = "left",
          fontSize = "0.8rem"
        )
      ),
      value = colDef(
      style = list(
        textAlign = "right",
        fontWeight = "600",
        fontSize = "0.8rem",
        lineHeight = "1.375rem"
      )
    )),
    compact = TRUE,
    theme = reactableTheme(
      headerStyle = list(display = "none"),
      cellPadding = "4px 8px"
    )
  )
}


#' @export
make_stock_table <- function(dt){

  dt |> reactable(
    compact = TRUE,
    showPageInfo = FALSE,
    paginationType = "simple",
    #pagination = FALSE,
    #height = "300px",
    searchable = TRUE,
    highlight = TRUE,
    wrap = FALSE,
    selection = "single",
    onClick = "select",
    columns = list(
      Symbol = colDef(sticky = "left",
                      width = 80),
      Name = colDef(width = 350),
      Financial.Status = colDef(width = 150),
      ETF = colDef(width = 80)
    ),
    theme = reactableTheme(searchInputStyle = list(width = "100%"))
  )
}

#' @export
ui_switch_inputs <- function(id1, id2, idSpinner){
  div(style = "display: flex; align-items: flex-start; justify-content: flex-start; gap: 15px;",
      switchInput(
        inputId = id1,
        label = "Show list",
        labelWidth = "100%",
        size = "mini",
        inline = T,
        value = TRUE,
        onLabel = "✓",
        offLabel = "✕"
      ),
      switchInput(
        inputId = id2,
        label = "SP500",
        labelWidth = "100%",
        size = "mini",
        inline = T,
        value = TRUE,
        #onStatus = "success",
        onLabel = "✓",
        offLabel = "✕"
      ),
      div(
        id = idSpinner,
        style = "width: 21.33px; height: 21.33px; border-radius: 50%; align-self: flex-start;margin-top: 3.6px;"
      ),
  )
}

#' @export
#' custom radiobutton
customRadioGroupButtons <- function(inputId, choices, selected = NULL, status = "default",
                                    size = "normal", justified = FALSE, individual = FALSE,
                                    checkIcon = NULL, direction = "horizontal", width = NULL,
                                    class = NULL) {
  # Ensure selected is not NULL, default to the first choice if it is NULL
  if (is.null(selected) && length(choices) > 0) {
    selected <- choices[1]
  }

  # Generate button HTML
  btns <- lapply(seq_along(choices), function(i) {
    choiceName <- names(choices)[i]
    if (is.null(choiceName)) choiceName <- choices[i]
    choiceValue <- choices[i]
    btnClass <- paste0("btn btn-", status, " btn-", size)
    if (choiceValue == selected) btnClass <- paste(btnClass, "active")
    tags$button(
      type = "button",
      class = btnClass,
      `data-value` = choiceValue,
      choiceName
    )
  })

  # Create container div with optional custom class
  containerClass <- "btn-group-container-sw"
  if (!is.null(class)) {
    containerClass <- paste(containerClass, class)
  }

  # Create button group div
  btnGroup <- div(
    class = if (individual) "btn-group-toggle" else "btn-group",
    `data-toggle` = if (individual) "buttons" else NULL,
    role = "group",
    btns
  )

  # Add justification if specified
  if (justified) {
    btnGroup <- div(class = "btn-group btn-group-justified", role = "group", btns)
  }

  # Add direction if specified
  if (direction == "vertical") {
    btnGroup <- div(class = "btn-group-vertical", role = "group", btns)
  }

  # Return full UI component
  div(
    id = inputId,
    class = containerClass,
    style = if (!is.null(width)) paste("width:", width, ";") else NULL,
    btnGroup,
    tags$script(HTML(paste0(
      "$(function() {
        $('#", inputId, " .btn').click(function() {
          $('#", inputId, " .btn').removeClass('active');
          $(this).addClass('active');
          var val = $(this).data('value');
          Shiny.setInputValue('", inputId, "', val);
        });
      });"
    )))

  )
}

#' @export
ui_title_plot_card <- function(titleId,radiobuttonsId, plotId, tickerId){

  card(
    div(style = "padding-left: 5px;",
        tags$h5(textOutput(titleId), style = "font-weight: bold; color: #464646; font-size: 20px;"),
        uiOutput(tickerId)
        ),


      customRadioGroupButtons(
        inputId = radiobuttonsId,
        choices = c("3M" = 3,
                    "6M" = 6,
                    "1Y" = 12,
                    "5Y" = (12*5),
                    "ALL" = (12*10)),
        selected = 6,
        size = "xs",
        direction = "horizontal",
        individual = TRUE,
        class = "custom-radio-group"
      ),
    card_body(
      class = "resize_chart",
      #max_height = 200,
      fill = TRUE,
          echarts4rOutput(plotId)
    )
  )
}

#' @export

make_stock_plot <- function(data, slicer, defaultMonth = 6, name){
  d <- data

  if (!is.null(slicer)) {
    d  <- filter(data, date >= months_ago(slicer))
  } else {
    d  <- filter(data, date >= months_ago(defaultMonth))
  }

  y_min <- min(d$Low)
  y_max <- max(d$High)
  y_range <- y_max - y_min
  y_min_adjusted <- y_min - y_range * 0.1
  y_max_adjusted <- y_max + y_range * 0.1

  d |>
    e_charts(date) |>
    e_candle(
      opening = Open, closing = Close,
      low = Low, high = High,
      name = name
    ) |>
    #e_dims(height = "100px") |>
    e_datazoom(type = "slider") |>
    e_grid(top = 10, right = 5, left = 35) |>
    e_tooltip(trigger = "axis") |>
    e_legend(FALSE) |>
    e_toolbox(show = FALSE) |>
    e_show_loading() |>
    e_y_axis(min = y_min_adjusted,
             max = y_max_adjusted,
             axisLabel = list(showMinLabel = FALSE, showMaxLabel = FALSE))
}

#' @export
make_volume_plot <- function(data, days){

  data |>
    tail(days) |>
    e_charts(date) |>
    e_line(
      Volume,
      smooth = TRUE,
      lineStyle = list(color = custom_blue, width = 0.3),
      legend = FALSE
    ) |>
    e_area(
      Volume,
      smooth = TRUE,
      itemStyle = list(opacity = 0.1),
      color = custom_blue
    ) |>
    e_tooltip(trigger = "axis") |>
    e_x_axis(show = FALSE) |>
    e_y_axis(show = FALSE) |>
    e_grid(
      top = 0,
      right = 0,
      bottom = 0,
      left = 0
    ) |>
    e_legend(FALSE)
}

#' @export
ui_source_link <- function(ticker){
  tags$p(
    "Source: ",
    tags$a("Yahoo finance",
           target = "_blank",
           href = paste0("https://finance.yahoo.com/quote/", ticker),
           class = "noHover",
    ),
    style = "font-size: 12px; color: #7f8189; display: inline;"
  )
}

#' @export
get_variation<-function(data){

  old <- tail(data,2)[1,]$Close
  new <- tail(data,2)[2,]$Close
  variation <- round((new-old)/old*100,4)

  r <- NULL

  if(variation > 0 ){
    text <- paste0("+ ", abs(variation),"%")
    r <- list(icon = bs_icon("graph-up-arrow", class = "text-success"),
              var = text,
              status = "+")
  } else if( variation < 0){
    text <- paste0("- ", abs(variation),"%")
    r <- list(icon = bs_icon("graph-down-arrow", class = "text-danger"),
              var = text,
              status = "-")
  } else {
    r <- list(icon = bs_icon("dash-lg"),
              var = "-",
              status = "neutral")
  }

  return(r)
}
