# app/view/

box::use(
  shiny[...,observe, req, reactiveValues, div, moduleServer, NS,],
  bslib[..., page_fillable, card, card_header, card_body],
  data.table[data.table],
  echarts4r[renderEcharts4r, echarts4rOutput, ],
  reactable[renderReactable, reactableOutput],
  shiny.router[is_page],
  htmltools[css],
  stats[acf, pacf],
  shinybrowser[get_device],

)
box::use(
  app / logic / general_utils[title, subtitle ],
  app / logic / stockAnalysis_utils[get_returns,get_lag,
                                    make_analysis_plots, make_price_table, make_cf_plot,
                                    ],
)

#' @export
ui <- function(id) {
  ns <- NS(id)

  page_fillable(


    title("Stock Analysis"),
    subtitle("Perform analysis on returns, ACF, PACF e other things ... "),
    navset_card_underline(
      height = 400,
      full_screen = TRUE,
      nav_panel("Returns", echarts4rOutput(ns("returns_plot"))),
      nav_panel("Sq. returns", echarts4rOutput(ns("sqReturns_plot"))),
      nav_panel("Distribution", echarts4rOutput(ns("pricesDistr_plot")))
    ),

    card_title("ACF and PACF"),
    card(
      card_body(
        navset_underline(
          nav_panel("Returns", echarts4rOutput(ns("cf_returns_plot"))),
          nav_panel("Sq. returns", echarts4rOutput(ns("cf_sqReturns_plot")))
        )
      ),
      card_footer(class = "pb-0",

        layout_column_wrap(
          width = NULL,
          style = css(grid_template_columns = "1fr 2fr"),
                  div(class = "flex-container",
                    radioButtons(ns("cfRadio"), label = NULL, inline = T,
                               choices = c("Returns" = "ret",
                                           "Sq. returns" = "sqRet"))),
                  sliderInput(ns("cfSlider"), label = NULL,
                              min = 0, max = 150, value = 20, ticks = F)

          )


      )
    ),
    card(
      card_header(class = "pb-0",
                  div("Summary statistics",
                    p("", id = ns("infoMobile"), class = "p_sub_small d-inline ps-2"))),
      layout_column_wrap(
        width = 1 / 2,
        height_mobile = "220px",
        fillable = F,
        div(
          card_header("Prices", class = "p-0"),
          reactableOutput(ns("summaryPrice"), width = "95%")
        ),
        div(
          card_header("Returns", class = "p-0"),
          reactableOutput(ns("summaryReturns"), width = "95%")
        )
      )
    ),
    card_title("Statistical tests"),
    card(navset_pill_list(
      well = F,

      nav_panel(title = "One", p("First tab content.")),
      nav_panel(title = "Two", p("Second tab content.")),
      nav_panel(title = "Three", p("Third tab content"))

    ))
  )
}

#' @export
server <- function(id, stockInfo) {

  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    r <- reactiveValues()
    stats = reactiveVal()
    plots = reactiveVal()
    plots_cf = reactiveVal()
    once <- reactiveVal(T)

    observe({
      req(stockInfo()$data_xts(), is_page("stockAnalysis") )

      ret <- get_returns(stockInfo()$data_xts())
      r$returns <- ret$returns
      r$returns_dt <- ret$returns_dt
      r$squared_returns <- ret$squared_returns
      r$squared_returns_dt <- ret$squared_returns_dt
      r$price <- data.frame(price = stockInfo()$data()$Close)

      if (once()) {
        updateSliderInput(session, "cfSlider", value = get_lag(r$returns))
        once(FALSE)
      }

      plots(make_analysis_plots(r, stockInfo()$ticker()))
      stats(make_price_table(stockInfo()$data_xts(), r$returns))
      plots_cf(make_cf_plot(r$returns,r$squared_returns,
                            input$cfSlider, stockInfo()$ticker()))
    })

    # first card
    output$returns_plot <- renderEcharts4r({
      req(plots())
      plots()$daily_returns_plot
    })
    output$sqReturns_plot <- renderEcharts4r({
      req(plots())
      plots()$squared_returns_plot
    })
    output$pricesDistr_plot <- renderEcharts4r({
      req(plots())
      plots()$returns_distribution_plot
    })

    # ACF/PACF
    output$cf_returns_plot <- renderEcharts4r({
      req(plots_cf())

      if(input$cfRadio == "ret") plots_cf()$acf else plots_cf()$acf_sq
    })
    output$cf_sqReturns_plot <- renderEcharts4r({
      req(plots_cf())
      if(input$cfRadio == "ret") plots_cf()$pacf else plots_cf()$pacf_sq
    })

    output$summaryPrice <- renderReactable({
      req(stats())
      stats()$table_prices
    })
    output$summaryReturns <- renderReactable({
      req(stats())
      stats()$table_returns
    })


  })
}
