# app/view/

box::use(
  shiny[...,observe, req, reactiveValues, div, moduleServer, NS,],
  bslib[..., page_fillable, card, card_header, card_body],
  data.table[data.table],
  echarts4r[renderEcharts4r, echarts4rOutput, ],
  reactable[renderReactable, reactableOutput],
  shiny.router[is_page],
  htmltools[css],
  shinybrowser[get_device],
  spsComps[addLoader],
)
box::use(
  app / logic / general_utils[title, subtitle ],
  app / logic / stockAnalysis_utils[get_returns,get_lag,
                                    make_analysis_plots, make_price_table, make_cf_plot,
                                    make_box_table, make_jb_table, make_adf_table, make_t_table, make_arch_table,
                                    test_header],
)

#' @export
ui <- function(id) {
  ns <- NS(id)

  page_fillable(

    div( style = "display: flex; align-items: top;",
        title("Stock Analysis"),
        div(id = ns("stockAnalysisLoader"), style = "height: 23px; width: 23px; margin-left: 10px;")
    ),


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
      card_body(class = "pb-0",
        navset_underline(
          nav_panel("ACF", div(style="padding-top: 18px",
                               div(class = "resize_chart",
                               echarts4rOutput(ns("acf_plot"))))),
          nav_panel("PACF", div(style="padding-top: 18px",
                                div(class = "resize_chart",
                                echarts4rOutput(ns("pacf_plot")))))
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
                              min = 1, max = 150, value = 20, ticks = F))
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
    layout_column_wrap(width = 1 / 2,
                       card(
                         test_header("Box Test",
                                     ns("boxSwitch")),
                         card_body(reactableOutput(ns("boxTable"))),
                         card_footer(class = "pb-0",
                                     layout_column_wrap(
                                       width = NULL,
                                       style = css(grid_template_columns = "1fr 2fr"),
                                       div(class = "flex-container",
                                           radioButtons(ns("boxRadio"), label = NULL, inline = T,
                                                        choices = c("Ljung-Box" = "Ljung-Box",
                                                                    "Box-Pierce" = "Box-Pierce"))),
                                       sliderInput(ns("boxSlider"), label = NULL,
                                                   min = 1, max = 150, value = 20, ticks = F))
                         )
                       ),
                       card(
                         test_header("Augmented Dickey-Fuller Test",
                                     ns("adfSwitch")),
                         card_body( reactableOutput(ns("adfTable"))),
                         card_footer(class = "pb-0",
                                     sliderInput(ns("adfSlider"),
                                                 label = NULL, width = "100%",
                                                 min = 1, max = 50, value = 10, ticks = F))
                       ),
                       card(
                         test_header("Jarque-Bera Test",ns("jbSwitch")),
                         card_body(reactableOutput(ns("jbTable")))
                       ),

                       card(
                         test_header("T-Test", ns("tSwitch")),
                         card_body(reactableOutput(ns("tTable")))
                       ),
                       card(
                         test_header("ARCH Test", ns("archSwitch")),
                         card_body(reactableOutput(ns("archTable")))
                       )
    )
  )
}

#' @export
server <- function(id, stockInfo) {

  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    spinner <- addLoader$new(
      target_selector =  "stockAnalysisLoader",
      type = "facebook", #ripple or dual-ring
      height = "21.33px",
      color = "#757575"
    )

    once <- reactiveVal(T)
    r <- reactiveValues()
    stats = reactiveVal()
    plots = reactiveVal()
    plots_cf = reactiveVal()
    test = reactiveValues()

    observe({
      req(stockInfo()$data_xts(), is_page("stockAnalysis") )

      spinner$show()

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

      test$box_r <- make_box_table(returns = r$returns,
                                   lag = input$boxSlider,
                                   type = input$boxRadio)
      test$box_r2 <- make_box_table(sq_returns = r$squared_returns,
                                    lag = input$boxSlider,
                                    type = input$boxRadio)
      test$jb_r <- make_jb_table(returns =  r$returns)
      test$jb_r2 <- make_jb_table(sq_returns =  r$squared_returns)
      test$adf_r <- make_adf_table(returns =  r$returns, lag = input$adfSlider)
      test$adf_r2 <- make_adf_table(sq_returns =  r$squared_returns, lag = input$adfSlider)
      test$t_r <- make_t_table(returns =  r$returns)
      test$t_r2 <- make_t_table(sq_returns =  r$squared_returns)
      test$arch_r <- make_arch_table(returns =  r$returns)
      test$arch_r2 <- make_arch_table(sq_returns =  r$squared_returns)

      spinner$hide()
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
    output$acf_plot <- renderEcharts4r({
      req(plots_cf())

      if(input$cfRadio == "ret") plots_cf()$acf else plots_cf()$acf_sq
    })
    output$pacf_plot <- renderEcharts4r({
      req(plots_cf())
      if(input$cfRadio == "ret") plots_cf()$pacf else plots_cf()$pacf_sq
    })

    # Summary
    output$summaryPrice <- renderReactable({
      req(stats())
      stats()$table_prices
    })
    output$summaryReturns <- renderReactable({
      req(stats())
      stats()$table_returns
    })

    # Test
    output$boxTable <- renderReactable({
      req(test$box_r)
      if (input$boxSwitch) test$box_r else test$box_r2
    })

    output$jbTable <- renderReactable({
      req(test$jb_r)
      if (input$jbSwitch) test$jb_r else test$jb_r2
    })

    output$adfTable <- renderReactable({
      req(test$adf_r)
      if (input$adfSwitch) test$adf_r else test$adf_r2
    })

    output$tTable <- renderReactable({
      req(test$t_r)
      if (input$tSwitch) test$t_r else test$t_r2
    })

    output$archTable <- renderReactable({
      req(test$arch_r)
      if (input$archSwitch) test$arch_r else test$arch_r2
    })


  })
}
