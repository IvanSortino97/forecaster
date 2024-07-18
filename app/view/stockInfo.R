box::use(shiny[div, moduleServer, NS, selectizeInput,reactiveVal, ...],
         bslib[page_fillable, card, card_header, card_body],
         shinyWidgets[switchInput, updateSwitchInput, prettyCheckbox],
         echarts4r[..., echarts4rOutput, renderEcharts4r, e_legend,e_title,e_tooltip,e_line,e_charts],
         dplyr[filter],
         zoo[coredata, index],
         data.table[data.table],
         quantmod[getSymbols],
         reactable[reactableOutput, renderReactable, reactable,colDef,reactableTheme, getReactableState],)
box::use(app / logic / stockInfo_utils[get_symbols,get_sp500, make_list, years_ago, scrape_yahoo_finance])


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
    div(
      h2(textOutput(ns("stockTitle"))),
      h3(textOutput(ns("stockTicker")))
    ),
    card(
      card_header("Data"),
      card_body(echarts4rOutput(ns("stockPlot")))
    ),
    card(
      card_header("Scraped Info"),
      card_body(verbatimTextOutput(ns("scrapedInfo")))
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
      #updateSwitchInput(inputId = "tableSwitch", value = F)
    })




    # Pull data when stock is selected
    observeEvent(selectedTicker(), {
      req(selectedTicker())

      stock_data <- getSymbols(
        Symbols = selectedTicker(),
        src = "yahoo",
        auto.assign = FALSE,
        from = years_ago(10)
      )
      data_xts(stock_data)
      scraped_info(scrape_yahoo_finance(selectedTicker()))

      stock_data <- data.table(date = as.character(index(stock_data)),
                                coredata(stock_data))

      names(stock_data) <-
        sub(paste0(selectedTicker(), "."), "", names(stock_data))

      data(stock_data)
      selectedStockInfo(symbols_dt() |> filter(Symbol == selectedTicker()))

    })


    output$stockTitle <- renderText({
      req(selectedStockInfo())
      selectedStockInfo()$Name
    })

    output$stockTicker <- renderText({
      req(selectedStockInfo())
      selectedStockInfo()$Symbol
    })

    output$stockPlot <- renderEcharts4r({
      req(data())

      data() |>
        e_charts(date) |>
        e_candle(opening = Open, closing = Close, low = Low, high = High, name = selectedStockInfo()$Symbol) |>
        e_datazoom(type = "slider") |>
        e_title(selectedStockInfo()$Name, "Quantmod data") |>
        e_tooltip(trigger = "axis") |>
        e_legend()
    })


    output$scrapedInfo <- renderText({
      scraped_info()$description

      })


  })
}
