box::use(shiny[div, moduleServer, NS, selectizeInput,reactiveVal, ...],
         bslib[page_fillable, card, card_header, card_body],
         shinyWidgets[switchInput, updateSwitchInput, prettyCheckbox],
         echarts4r[echarts4rOutput],
         zoo[coredata, index],
         data.table[data.table],
         quantmod[getSymbols],
         reactable[reactableOutput, renderReactable, reactable],)
box::use(app / logic / stockInfo_utils[get_symbols, make_list, years_ago])


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
      card_header("Data"),
      card_body(reactableOutput(ns("dataTable")))
    ),

    echarts4rOutput(ns("stockPlot"))
  )
}


#' @export
server <- function(id, ...) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    symbols_dt <- get_symbols()
    symbol_list <- make_list(symbols_dt)

    updateSelectizeInput(inputId = "selectStock",
                         choices = symbol_list,
                         selected = "",
                         server = TRUE)

    output$stockTable <- renderReactable({
      symbols_dt |> reactable(compact = TRUE,
                              showPageInfo = FALSE,
                              searchable = TRUE)
    })


    data <- reactiveVal(NULL)

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

    })

    output$dataTable <-  renderReactable({
      req(data())
      data() |> reactable(compact = TRUE,
                              showPageInfo = FALSE,
                              searchable = FALSE)
    })



  })
}
