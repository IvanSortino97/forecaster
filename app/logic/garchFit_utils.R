box::use(
  shiny[tags, checkboxGroupInput, conditionalPanel],
  shinyWidgets[switchInput],
  bslib[card, card_header, card_body]
)

#' @export
model_checkbox <- function(id){
  tags$div(
    class = "multicol",
    checkboxGroupInput(id,
                       label = NULL,
                       selected = c("GARCH", "eGARCH"),
                       choices = c(
                         "GARCH" = "GARCH",
                         "eGARCH" = "eGARCH",
                         "GJR - GARCH" = "GJRGARCH",
                         "TGARCH" = "TGARCH",
                         "APARCH" = "APARCH",
                         "IGARCH" = "IGARCH",
                         "FIGARCH" = "FIGARCH",
                         "QGARCH" = "QGARCH",
                         "NGARCH" = "NGARCH",
                         "VGARCH" = "VGARCH"
                       ))
  )
}


settings_header <- function(title, switchId ) {
  card_header(
    tags$div(class = "d-flex justify-content-between align-items-center",
             tags$div(style = "padding-top: 5px;", title),
             switchInput(
               inputId = switchId,
               labelWidth = "100%",
               size = "mini",
               inline = TRUE,
               value = TRUE,
               label = "auto",
               onLabel = "✓",
               offLabel = "✕",
               width = "auto"
             )
    ),
  )
}

#' @export
parameter_card <- function(title, switchId = "", body) {
  card(
    settings_header(title, switchId ),
    card_body(body)
  )
}

#' @export
conditional_model <- function(ns, model, body){

  conditionalPanel(ns = ns,
                   condition = sprintf("input.modelCheckbox.includes('%s')",
                                       sub(" - ","",model) ),
                   parameter_card(
                     title = paste0(model, " Model"),
                     switchId = ns(paste0(model, "-switch")),
                     body = body
                   )
  )
}
