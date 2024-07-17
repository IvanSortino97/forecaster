box::use(shiny[div, moduleServer, NS, selectizeInput,reactiveVal, ...],
         bslib[page_fillable, card, card_header, card_body],
         shinyWidgets[switchInput, updateSwitchInput, prettyCheckbox],
         echarts4r[echarts4rOutput, renderEcharts4r, e_legend,e_title,e_tooltip,e_line,e_charts],
         dplyr[filter],
         zoo[coredata, index],
         data.table[data.table],
         quantmod[getSymbols],
         reactable[reactableOutput, renderReactable, reactable,colDef, getReactableState],)
box::use(app / logic / stockInfo_utils[get_symbols,get_sp500, make_list, years_ago])


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
    )

  )
}


#' @export
server <- function(id, ...) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    symbols_dt <- get_symbols()
    sp500 <- get_sp500()$Symbol
    symbol_list <- make_list(symbols_dt)
    selectedStock <- reactiveVal(NULL)
    data <- reactiveVal(NULL)

    filtered_symbols <- reactive({
      if (input$sp500Switch)  filter(symbols_dt, Symbol %in% sp500) else symbols_dt
    })

    updateSelectizeInput(inputId = "selectStock",
                         choices = symbol_list,
                         selected = "",
                         server = TRUE)

    output$stockTable <- renderReactable({

      filtered_symbols() |> reactable(compact = TRUE,
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
                                      ))
    })



    # Pull data when stock is selected
    observeEvent(input$selectStock, {

      req(input$selectStock)

      updateSwitchInput(inputId = "tableSwitch", value = F)

      ticker <- input$selectStock
      stock_data <- getSymbols(Symbols = ticker,
                               src = "yahoo",
                               auto.assign = FALSE,
                               from = years_ago(10) )

      stock_data <- data.table(date = index(stock_data),
                               coredata(stock_data))

      names(stock_data) <- sub(paste0(input$selectStock, "."),"", names(stock_data))

      data(stock_data)
      selectedStock(filtered_symbols() |> filter(Symbol == ticker))

    })

    # Update selected stock from table list
    observeEvent(getReactableState("stockTable", "selected"), {
      selected <- getReactableState("stockTable", "selected")
      if (length(selected) > 0) {
        selected_stock <- filtered_symbols()[selected, 1]
        updateSelectizeInput(inputId = "selectStock",
                             selected = selected_stock)
      }
    })

    output$stockTitle <- renderText({
      req(selectedStock())
      selectedStock()$Name
    })

    output$stockTicker <- renderText({
      req(selectedStock())
      selectedStock()$Symbol
    })

    output$stockPlot <- renderEcharts4r({

      req(data())

      data() |>
        e_charts(date) |>
        e_line(Close,
               name = selectedStock()$Symbol,
               smooth = TRUE,
               symbol = "none",
               animation = TRUE) |>
        e_title("10-Year Historical Stock Data") |>
        e_tooltip(trigger = "axis") |>
        e_legend()
    })



  })
}
