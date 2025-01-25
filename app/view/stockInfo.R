box::use(shiny[ div, moduleServer, NS, selectizeInput, reactiveVal,radioButtons, renderUI,uiOutput, reactive, observe, req, conditionalPanel, textOutput, p, updateSelectizeInput, observeEvent, renderText, tags],
         bslib[page_fillable, card, card_header,card_title, card_body, value_box, layout_column_wrap ],
         bsicons[bs_icon],
         shinyWidgets[updateSwitchInput],
         dplyr[filter,],
         echarts4r[echarts4rOutput, renderEcharts4r],
         zoo[coredata, index],
         data.table[data.table, setorder],
         spsComps[addLoader],
         shiny.router[route_link],
         reactable[reactableOutput, renderReactable, getReactableState],
)
box::use(app / logic / general_utils[title, subtitle, tryCatch_toaster, page_footer, make_spinner])
box::use(app / logic / stockInfo_utils[get_symbols, get_sp500, get_data,get_variation, get_dailyReturns,
                                       make_list, make_stock_table, make_stock_plot, make_volume_plot,
                                       years_ago, months_ago, scrape_yahoo_finance, make_stat_table,
                                       ui_switch_inputs, ui_title_plot_card, ui_source_link])

#' @export
ui <- function(id) {
  ns <- NS(id)

  page_fillable(

    title("Stock Selection",
          ns("loadingDiv")),
    subtitle("Selection and analysis dashboard for NASDAQ Stocks with interactive data visualization and financial metrics"),

    selectizeInput(
      ns("selectStock"),
      label = NULL,
      choices = NULL,
      options = list(
        placeholder = 'Please select a stock to analyze.',
        onInitialize = I('function() { this.setValue(""); }')),
      width = "100%"
    ),
    ui_switch_inputs(ns("tableSwitch"),
                     ns("sp500Switch"),
                     ns("") #loading div
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

    conditionalPanel(
      condition = "input.tableSwitch === false",
      ns = ns,

    ui_title_plot_card(titleId = ns("stockTitle"),
                        radiobuttonsId = ns("yearSlicer"),
                        plotId = ns("stockPlot"),
                       tickerId = ns("stockUrl")),
    layout_column_wrap(
      width = "200px",

      value_box(
        title = "Stock Price",
        value = textOutput(ns("stockPrice")),
        showcase = uiOutput(ns("stockPriceIcon")),
        uiOutput(ns("stockPriceVariation"))
      ),
      value_box(
        title = "52 Week Range",
        value = textOutput(ns("fiftyTwoWeekRange")),
        showcase = bs_icon("calendar3-range")
      ),
      value_box(
        title = "Volume",
        value = textOutput(ns("volume")),
        showcase = echarts4rOutput(ns("volumePlot")),
        full_screen = TRUE,
        p("",id = ns("infoVolume"), class= "p_sub_small")
      ),
      value_box(
        title = "Average Volume",
        value = textOutput(ns("averageVolume")),
        showcase = bs_icon("bar-chart-line-fill")
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
      max_height = 200,
      full_screen = TRUE,
      textOutput(ns("description"))
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
    ),
    page_footer(hrefPageNext = "stockAnalysis",
                textPageNext = "Stock Analysis")

    )
  )
}

#' @export
server <- function(id, ...) {

  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    spinner <- make_spinner("loadingDiv")

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
    returns <- reactiveVal(NULL)

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
      make_stock_table(symbols_dt())
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
      spinner$show()

      stock_data <- get_data(ticker = selectedTicker(),
                             from = years_ago(10))

      data_xts(stock_data)
      returns(get_dailyReturns(stock_data))
      scraped_info(scrape_yahoo_finance(selectedTicker()))

      stock_data <- data.table(date = as.character(index(stock_data)),
                               #Date = index(stock_data),
                               coredata(stock_data))

      setorder(stock_data, date) # Set order to ensure consistency while using tail and head function
      names(stock_data) <- sub(paste0(selectedTicker(), "."), "", names(stock_data))

      data(stock_data)
      selectedStockInfo(symbols_dt() |> filter(Symbol == selectedTicker()))
      spinner$hide()
    })

    output$stockTitle <- renderText(selectedStockInfo()$Name)
    output$stockUrl <- renderUI(ui_source_link(selectedTicker()))

    output$stockPlot <- renderEcharts4r({

      req(data())
      make_stock_plot(data(),
                      input$yearSlicer,
                      defaultMonth = 6,
                      name = selectedStockInfo()$Symbol)
    })


    output$volumePlot <- renderEcharts4r({
      req(data())
      make_volume_plot(data(), days = 61)
    })

    variation <- reactive({
      req(data())
      get_variation(data())
      })

    output$stockPriceIcon <- renderUI({
      req(data())
      variation()$icon
    })

    output$stockPrice <- renderText({
      req(scraped_info())
      scraped_info()$stockPrice
    })
    output$stockPriceVariation <- renderUI({
      req(variation())

      switch (variation()$status,
        "+" = tags$p(variation()$var, style = "color: #269b3c;"),
        "-" = tags$p(variation()$var, style = "color: #ca2626"),
        "neutral" = tags$p(variation()$var, style = "color: #ca2626")
      )
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

    output$description <- renderText({
      req(scraped_info())
      if (!is.na(scraped_info()$description)) scraped_info()$description else scraped_info()$description2
    })

    output$valuationMeasures <- renderReactable({
      req(scraped_info())
      make_stat_table(scraped_info(), "valuation")
    })
    output$profitabilityTable <- renderReactable({
      req(scraped_info())
      make_stat_table(scraped_info(), "profitability")
    })
    output$balancesheetTable <- renderReactable({
      req(scraped_info())
      make_stat_table(scraped_info(), "balancesheet")
    })

    return(
      list(
        ticker = reactive(selectedTicker()),
        name = reactive(selectedStockInfo()$Name),
        data = reactive(data()),
        data_xts = reactive(data_xts()),
        returns = reactive(returns())
      )
    )

  })
}
