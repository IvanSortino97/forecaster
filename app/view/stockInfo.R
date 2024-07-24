box::use(shiny[div, moduleServer, NS, selectizeInput, reactiveVal,radioButtons, ...],
         bslib[page_fillable, card, card_header, card_body, value_box, layout_column_wrap ],
         bsicons[bs_icon],
         shinyWidgets[switchInput, updateSwitchInput, radioGroupButtons],
         echarts4r[..., echarts4rOutput, renderEcharts4r, e_legend, e_title, e_tooltip, e_line, e_charts],
         dplyr[filter],
         zoo[coredata, index],
         data.table[data.table],
         utils[tail],
         reactable[reactableOutput, renderReactable, reactable, colDef, reactableTheme, getReactableState],
)
box::use(app / logic / stockInfo_utils[get_symbols, get_sp500, get_data, make_list, years_ago, scrape_yahoo_finance, stat_table])

#' @export
ui <- function(id) {
  ns <- NS(id)

  page_fillable(
    selectizeInput(
      ns("selectStock"),
      "Select stock",
      choices = NULL,
      options = list(
        placeholder = 'Please select a stock to analyze.',
        onInitialize = I('function() { this.setValue(""); }')),
      width = "100%"
    ),
    div(style = "display: flex; align-items: center; justify-content: flex-start; gap: 15px;",
        switchInput(
          inputId = ns("tableSwitch"),
          label = "Show list",
          labelWidth = "100%",
          size = "mini",
          inline = T,
          value = TRUE,
          onLabel = "✓",
          offLabel = "✕"
        ),
        switchInput(
          inputId = ns("sp500Switch"),
          label = "SP500",
          labelWidth = "100%",
          size = "mini",
          inline = T,
          value = TRUE,
          onStatus = "success",
          onLabel = "✓",
          offLabel = "✕"
        )
    ),
    conditionalPanel(
      condition = "input.tableSwitch === true",
      ns = ns,
      card(
        card_header("NASDAQ Stocks list"),
        card_body(
          reactableOutput(ns("stockTable"))
        )
      )
    ),
    card(
      card_body(echarts4rOutput(ns("stockPlot")),
                radioGroupButtons(
                  inputId = ns("yearSlicer"),
                  choices = c("3M","6M","1Y","5Y","ALL"),
                  selected = "6M",
                  #inline = TRUE,
                  #width = "100px",
                  #size = "xs"
                )
                )
    ),
    layout_column_wrap(
      #height = "600px",
      width = "200px",
      value_box(
        title = "Stock Price",
        value = textOutput(ns("stockPrice")),
        showcase = bs_icon("bar-chart")
      ),
      value_box(
        title = "52 Week Range",
        value = textOutput(ns("fiftyTwoWeekRange")),
        showcase = bs_icon("bar-chart")
      ),
      value_box(
        title = "Volume",
        value = textOutput(ns("volume")),
        showcase = echarts4rOutput(ns("volumePlot")),
        full_screen = TRUE,
        theme = "success",
        p("Open to see more")
      ),
      value_box(
        title = "Average Volume",
        value = textOutput(ns("averageVolume")),
        showcase = bs_icon("graph-up-arrow")
      ),
      value_box(
        title = "P/E Ratio (TTM)",
        value = textOutput(ns("peRatioTTM")),
        showcase = bs_icon("percent")
      ),
      value_box(
        title = "EPS (TTM)",
        value = textOutput(ns("epsTTM")),
        showcase = bs_icon("currency-exchange")
      ),
      value_box(
        title = "Market Cap",
        value = textOutput(ns("marketCap")),
        showcase = bs_icon("cash-stack")
      ),
      value_box(
        title = "Enterprise Value",
        value = textOutput(ns("enterpriseValue")),
        showcase = bs_icon("building")
      )
    ),
    card(
      card_header("Valuation Measures"),
      card_body(reactableOutput(ns("valuationMeasures")))
    ),
    card(
      card_header("Financial Highlights"),
      card_body(gap = 0,
      p("Profitability and Income Statement", class = "card-header", style = "border-bottom: none; padding-left: 0;"),
      reactableOutput(ns("profitabilityTable")),
      p("Balance Sheet and Cash Flow", class = "card-header", style = "border-bottom: none; padding-left: 0;"),
      reactableOutput(ns("balancesheetTable"))
      )
    )
  )
}

#' @export
server <- function(id, ...) {

  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    sp500 <- get_sp500()$Symbol
    symbols_dt <- reactive({
      if (input$sp500Switch)
        filter(get_symbols(), Symbol %in% sp500)
      else
        get_symbols()
    })

    selectedTicker <- reactiveVal("")
    selectedStockInfo <- reactiveVal(NULL)
    data <- reactiveVal(NULL)
    data_xts <- reactiveVal(NULL)
    scraped_info <- reactiveVal(NULL)

    observe(
      updateSelectizeInput(
        inputId = "selectStock",
        choices = make_list(symbols_dt()),
        selected = "",
        server = TRUE
      )
    )

    observeEvent(input$selectStock, {
      req(input$selectStock)
      selectedTicker(input$selectStock)
      updateSwitchInput(inputId = "tableSwitch", value = F)
    })

    output$stockTable <- renderReactable({
      symbols_dt() |> reactable(
        compact = TRUE,
        showPageInfo = FALSE,
        paginationType = "simple",
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
    })

    # Update selected stock from table list
    observeEvent(getReactableState("stockTable", "selected"), {
      selected_stock <- symbols_dt()[getReactableState("stockTable", "selected"), 1]
      updateSelectizeInput(inputId = "selectStock",
                           selected = selected_stock)
      selectedTicker(selected_stock)
    })

    # Pull data when stock is selected
    observeEvent(selectedTicker(), {
      req(selectedTicker())

      stock_data <- get_data(ticker = selectedTicker(),
                             from = years_ago(10))

      data_xts(stock_data)
      scraped_info(scrape_yahoo_finance(selectedTicker()))

      stock_data <- data.table(date = as.character(index(stock_data)),
                               coredata(stock_data))

      names(stock_data) <-
        sub(paste0(selectedTicker(), "."), "", names(stock_data))

      data(stock_data)
      selectedStockInfo(symbols_dt() |> filter(Symbol == selectedTicker()))
    })

    output$stockPlot <- renderEcharts4r({
      req(data())

      data() |>
        e_charts(date) |>
        e_candle(opening = Open, closing = Close, low = Low, high = High, name = selectedStockInfo()$Symbol) |>
        e_datazoom(type = "slider") |>
        e_title(selectedStockInfo()$Name, "Quantmod data") |>
        e_grid(right = 0, left = 0) |>
        e_tooltip(trigger = "axis") |>
        e_legend(FALSE)
    })

    output$volumePlot <- renderEcharts4r({
      req(data())

      data() |>
        tail(365) |>
        e_charts(date) |>
        e_line(Volume, smooth = TRUE, lineStyle = list(color = "white", width = 0.3), legend = FALSE) |>
        e_area(Volume, smooth = TRUE, itemStyle = list(opacity = 0.2), color = "white") |>
        e_tooltip(trigger = "axis") |>
        e_x_axis(show = FALSE) |>
        e_y_axis(show = FALSE) |>
        e_grid(top = 0, right = 0, bottom = 0, left = 0) |>
        e_legend(FALSE)
    })

    output$stockPrice <- renderText({
      req(scraped_info())
      scraped_info()$stockPrice
    })
    output$fiftyTwoWeekRange <- renderText({
      req(scraped_info())
      scraped_info()$fiftyTwoWeekRange
    })
    output$volume <- renderText({
      req(scraped_info())
      scraped_info()$volume
    })
    output$averageVolume <- renderText({
      req(scraped_info())
      scraped_info()$averageVolume
    })
    output$peRatioTTM <- renderText({
      req(scraped_info())
      scraped_info()$peRatioTTM
    })
    output$epsTTM <- renderText({
      req(scraped_info())
      scraped_info()$epsTTM
    })
    output$marketCap <- renderText({
      req(scraped_info())
      scraped_info()$marketCap
    })
    output$enterpriseValue <- renderText({
      req(scraped_info())
      scraped_info()$enterpriseValue
    })

    output$valuationMeasures <- renderReactable({
      req(scraped_info())
      stat_table(scraped_info(), "valuation")
    })
    output$profitabilityTable <- renderReactable({
      req(scraped_info())
      stat_table(scraped_info(), "profitability")
    })
    output$balancesheetTable <- renderReactable({
      req(scraped_info())
      stat_table(scraped_info(), "balancesheet")
    })


  })
}
