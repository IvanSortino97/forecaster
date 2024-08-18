box::use(
  shiny[verbatimTextOutput, incProgress,withProgress, selectizeInput, tags, checkboxGroupInput,checkboxInput, conditionalPanel, numericInput],
  bslib[card, card_header, card_body, layout_column_wrap, navset_underline, nav_panel],
  bsicons[bs_icon],
  data.table[data.table],
  reactable[reactableOutput, reactable, colDef],
  htmlwidgets[JS],
  shinyWidgets[switchInput],
  stringr[str_pad],
  stats[setNames],
  rugarch[infocriteria,ugarchfit,ugarchspec],
)
box::use(app / logic / general_utils[in_card_subtitle_style])

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
                         "APARCH" = "APARCH",
                         "IGARCH" = "IGARCH",
                         "FIGARCH" = "FIGARCH"
                       ))
  )
}

#' @export
model_switch <- function(model) {
  switch(model,
         "GARCH" = "sGARCH",
         "eGARCH" = "eGARCH",
         "GJRGARCH" = "gjrGARCH",
         "APARCH" = "apARCH",
         "IGARCH" = "iGARCH",
         "FIGARCH" = "fGARCH"
  )
}

#' @export
fit_garch <- function(model, p, q, ar = 0, ma = 0, dist, data, info = T) {
  model_name <- model_switch(model)

  spec <- ugarchspec(
    variance.model = list(model = model_name, garchOrder = c(p, q)),
    mean.model = list(armaOrder = c(ar, ma), include.mean = TRUE),
    distribution.model = dist  # Distribution model
  )

  fit <- ugarchfit(spec = spec, data = data)

  # Extract AIC and BIC
  ic <- infocriteria(fit)
  if (info) return(ic) else return(fit)
}



distributions = c("Normal" = "norm",
                  "Student-t" = "std",
                  "Skewed Normal" = "snorm",
                  "Skewed Student-t" = "sstd")

#' @export
get_best_fit <- function(model, returns){

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
                    detail = sprintf("GARCH (%s,%s) ARMA (%s,%s) [%s]",
                                      p, q, ar, ma, dist))

        ic <- tryCatch( fit_garch(model, p, q, ar, ma, dist, returns),
                        error = function(e){
                          list("error","error")
                        })
        results[[paste(dist, p, q, ar, ma, sep = "_")]] <- list(ic,
                                                                param = list(p,q,ar,ma),
                                                                dist = dist)
      }
    }
  )

  results_df <- do.call(rbind, lapply(names(results), function(x) data.table(
      Distribution = results[[x]]$dist,
      Parameters = paste(results[[x]]$param, collapse = "_"),
      AIC = results[[x]][[1]][1],
      BIC = results[[x]][[1]][2],
      p = results[[x]]$param[[1]],
      q = results[[x]]$param[[2]],
      ar = results[[x]]$param[[3]],
      ma = results[[x]]$param[[4]],
      dist = results[[x]]$dist
  )))

  reverse_distributions <- setNames(names(distributions), distributions)
  results_df[, Distribution := sapply(Distribution, function(d) reverse_distributions[[d]])]

  n_char = 10
  results_df[, AIC := sapply(as.character(AIC), function(a) substr(a, 1, n_char))]
  results_df[, BIC := sapply(as.character(BIC), function(a) substr(a, 1, n_char))]

  return(list(
    results = results_df
  ))

}

#' @export
make_id <- function(model, suffix = ""){paste0(sub(" - ","",model), suffix)}

settings_header <- function(title, model, ns ) {
  card_header(
    tags$div(class = "d-flex justify-content-between align-items-center",
             tags$div(style = "padding-top: 5px;", title),

             tags$div(class = "d-flex justify-content-end align-items-center",
                      # Show info icon -> make it a button to hide the parameter table
                      # conditionalPanel(ns = ns,
                      #                  condition = sprintf("input.%s === true", make_id(model, "switch")),
                      #                  bsicons::bs_icon("info")),
                      switchInput(
                        inputId = ns(make_id(model,"switch")),
                        labelWidth = "100%",
                        size = "mini",
                        inline = TRUE,
                        value = FALSE,
                        label = "auto",
                        onLabel = "✓",
                        offLabel = "✕",
                        width = "auto"
                      )
             )
    ),
  )
}

#' @export
parameter_card <- function(title, model, body, ns) {
  card(
    settings_header(title, model, ns),
    card_body(gap = 0,
              # conditionalPanel(ns = ns,
              #                  condition = sprintf("input.%s === true", make_id(model, "switch")),
              #                  tags$div(style = "padding-top: 10px; padding-bottom: 15px;",
              #                  reactableOutput(ns(make_id(model, "autoTable")))
              #                  )
              # ),
              body)
  )
}

#' @export
conditional_model <- function(ns , model){

  conditionalPanel(ns = ns,
                   condition = sprintf("input.modelCheckbox.includes('%s')",
                                       make_id(model) ),
                   parameter_card(
                     ns = ns,
                     model = model,
                     #switchId = ns(paste0(model, "-switch")),
                     title = paste0(model, " Model"),
                     body = model_body(ns = ns,
                                       model = model)
                   )
  )
}

#' @export
model_body <- function(ns, model){

  navset_underline(

   nav_panel("Parameters",

             conditionalPanel(ns = ns,
                              condition = sprintf("input.%s === true", make_id(model, "switch")),
                              tags$div(style = "padding-top: 10px; padding-bottom: 10px;",
                                       reactableOutput(ns(make_id(model, "autoTable")))
                              )
             ),

    tags$div(tags$p("Distribution", style = paste(in_card_subtitle_style,"font-size: 0.9rem;padding-top: 15px")),
             tags$div(style = "width: 50%;", class = "model-param",
             selectizeInput(inputId = ns(make_id(model,"dist")),
                            label = NULL, width = "100%",
                            choices = distributions)),
             tags$p("GARCH Order", style = paste(in_card_subtitle_style,"font-size: 0.9rem;")),
             tags$div(style = "display: flex; gap: 10px; width: 50%;", class = "model-param",
             numericInput(inputId = ns(make_id(model,"p")),
                          label = NULL, value = 1),
             numericInput(inputId = ns(make_id(model,"q")),
                          label = NULL, value = 1)
             ),
             tags$p("ARMA Order", style = paste(in_card_subtitle_style,"font-size: 0.9rem")),
             tags$div(style = "display: flex; gap: 10px; width: 50%;", class = "model-param",
             numericInput(inputId = ns(make_id(model,"ar")),
                          label = NULL, value = 0),
             numericInput(inputId = ns(make_id(model,"ma")),
                          label = NULL, value = 0)),
             checkboxInput(inputId = ns(make_id(model, "includeMean")), value = FALSE, label = "Include mean")
             )
            ),
   nav_panel("Results",
             tags$div(
               tags$p("results body subtitle", style = paste(in_card_subtitle_style,"font-size: 0.9rem;padding-top: 15px")),
               verbatimTextOutput(outputId = ns(make_id(model, "results")))
             )
             )
  )
}

#' @export
get_param <- function(best_fit, criteria){
  best_fit_index <- which.min(best_fit$results[[criteria]])
  dist <- best_fit$results[best_fit_index, ]$dist
  p <- best_fit$results[best_fit_index, ]$p
  q <- best_fit$results[best_fit_index, ]$q
  ar <- best_fit$results[best_fit_index, ]$ar
  ma <- best_fit$results[best_fit_index, ]$ma
  return(list(
    index = best_fit_index,
    dist = dist,
    p = p,
    q = q,
    ar = ar,
    ma = ma
  ))
}

#' @export
make_autoTable <- function(dt, best_fit_index, criteria) {

  best_fit_group <- dt[best_fit_index, "Distribution"]

  reactable(
    dt[, 1:4], # Display the first four columns
    groupBy = "Distribution",
    compact = TRUE,
    defaultColDef = colDef(
      style = function(value) {
        color <- if (value == "error") "red" else "black"
        list(fontSize = "0.8rem", color = color)
      },
      headerStyle = list(
        fontSize = "0.8rem"
      )
    ),
    rowStyle = function(index) {
      if (index == best_fit_index) {
        list(backgroundColor = "#FFFFCC", fontWeight = "bold")
      }
    },
    columns = list(
      Distribution = colDef(
        width = 200,
        style = list(
          whiteSpace = "nowrap",
          fontSize = "0.8rem"
        )
      ),
      Parameters = colDef(align = "right"),
      AIC = colDef(width = 120, align = "right"),
      BIC = colDef(width = 120, align = "right")
    )
    )
}
