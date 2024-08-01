# app/view/

box::use(
  shiny[..., conditionalPanel, observeEvent, reactiveVal, checkboxGroupInput, div, moduleServer, NS],
  bslib[page_fillable, card, card_header, card_body, card_title],
  shinyjs[show, hide, disable],
  reactable[reactable, renderReactable, colDef],
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

    hide("conditionalPanel")
    observeEvent(stockInfo()$data_xts() , {
      select_stock_condition(stockInfo()$data_xts())
    })

    observeEvent(input$GARCHswitch,{
      req(stockInfo()$returns(), input$GARCHswitch)

      model <- as.character("sGARCH")

      best_fit <- get_best_fit(model, stockInfo()$returns())
      criteria = "AIC"
      best_fit_index <- which.min(best_fit$results[[criteria]])
      dist <- best_fit$results[best_fit_index, ]$dist
      p <- best_fit$results[best_fit_index, ]$p
      q <- best_fit$results[best_fit_index, ]$q

      #TODO: enable inputs when switch to OFF
      updateSelectizeInput(inputId = "GARCHdist", selected = dist)
      disable("GARCHdist")
      updateNumericInput(inputId = "GARCHp", value = p)
      disable("GARCHp")
      updateNumericInput(inputId = "GARCHq", value = q)
      disable("GARCHq")

      output$GARCHautoTable <- renderReactable({


        reactable(
          best_fit$results[, 1:4], # Display the first four columns
          groupBy = "Distribution",
          compact = TRUE,
          defaultColDef = colDef(
            style = function(value) {
              color <- if (value == "error") "red" else "black"
              list(fontSize = "0.8rem", color = color)
            }
          ),
          rowStyle = function(index) {
            if (index == best_fit_index) {
              list(backgroundColor = "#FFFFCC", fontWeight = "bold")
            } else {
              list()
            }
          },
          columns = list(
            Distribution = colDef(
              width = 200,
              style = list(whiteSpace = "nowrap")
            ),
            AIC = colDef(width = 120),
            BIC = colDef(width = 120)
          )
        )

      })
    })
  })
}
