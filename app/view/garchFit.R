# app/view/

box::use(
  shiny[..., conditionalPanel, observeEvent, reactiveVal, checkboxGroupInput, div, moduleServer, NS],
  bslib[page_fillable, card, card_header, card_body, card_title],
  shinyjs[show, hide],
  data.table[data.table],
  reactable[reactable, renderReactable],
)
box::use(
  app / logic / general_utils[in_card_subtitle_style, conditional_page_fillable, make_spinner, select_stock_condition],
  app / logic / garchFit_utils[...]
)


#' @export
ui <- function(id) {
  ns <- NS(id)

  conditional_page_fillable(ns = ns,
                            title = "Fit Models",
                            subtitle = "Specify parameters to fit models. Auto mode provide the best parameter minimizin AIC statistic",
                            body = div(
                              card(
                                tags$h5("Select model to fit", style = in_card_subtitle_style),
                                model_checkbox(ns("modelCheckbox"))
                              ),
                              card_title("Specify parameters"),
                              conditional_model(ns = ns,
                                                model = "GARCH"
                              ),
                              conditional_model(ns = ns,
                                                model = "eGARCH"
                              ),
                              conditional_model(ns = ns,
                                                model = "GJR - GARCH"
                              ),
                              conditional_model(ns = ns,
                                                model = "TGARCH"
                              ),
                              conditional_model(ns = ns,
                                                model = "APARCH"
                              ),
                              conditional_model(ns = ns,
                                                model = "IGARCH"
                              ),
                              conditional_model(ns = ns,
                                                model = "FIGARCH"
                              ),
                              conditional_model(ns = ns,
                                                model = "QGARCH"
                              ),
                              conditional_model(ns = ns,
                                                model = "NGARCH"
                              ),
                              conditional_model(ns = ns,
                                                model = "VGARCH"
                              ),
                            )
  )
}

#' @export
server <- function(id, stockInfo) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    spinner <- make_spinner("titleLoader")

    # Example of a function call that might be used in the server logic
    # hide("conditionalPanel")
    # observeEvent(stockInfo()$data_xts() , {
    #   select_stock_condition(stockInfo()$data_xts())
    # })

    observeEvent(input$GARCHswitch,{
      req(stockInfo()$returns(), input$GARCHswitch)

      model <- as.character("sGARCH")
      distributions <- c("norm", "std", "snorm", "sstd")

      results <- list()
      distributions_combinations <- expand.grid(p = 1:3,
                                                q = 1:3,
                                                ar = 0,
                                                ma = 0,
                                                dist = distributions)
      len <- nrow(distributions_combinations)

      withProgress(
        message = model,
        value = 0,{

          for (i in 1:len) {
            p <- as.integer(distributions_combinations$p[i])
            q <- as.integer(distributions_combinations$q[i])
            ar <- as.integer(distributions_combinations$ar[i])
            ma <- as.integer(distributions_combinations$ma[i])
            dist <- as.character(distributions_combinations$dist[i])

            incProgress(amount = 1/len,
                        detail = sprintf("[%s] GARCH(%s,%s) ARMA(%s,%s)",dist, p, q, ar, ma))

            ic <-fit_garch(model, p, q, ar, ma, dist, stockInfo()$returns())
            results[[paste(p, q, ar, ma, dist, sep = "_")]] <- ic
          }
        }
      )

      results_df <- do.call(rbind, lapply(names(results), function(x) data.table(
        Model = x,
        AIC = results[[x]][1],
        BIC = results[[x]][2]
      )))

      # Find the best model based on AIC
      best_aic <- results_df[which.min(results_df$AIC), ]
      cat("Best model based on AIC:\n")
      print(best_aic)

      # Find the best model based on BIC
      best_bic <- results_df[which.min(results_df$BIC), ]
      cat("Best model based on BIC:\n")
      print(best_bic)

      #updateNumericInput(inputId = "GARCHp", value = 3333)
      #updateNumericInput(inputId = "GARCHq", value = 3333)

      output$GARCHautoTable <- renderReactable({

        reactable(results_df, compact = TRUE)
      })
    })
  })
}
