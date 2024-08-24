box::use(
  shiny[plotOutput, verbatimTextOutput, incProgress,withProgress, selectizeInput, tags, checkboxGroupInput,checkboxInput, conditionalPanel, numericInput],
  bslib[popover, card, card_header, card_body, layout_column_wrap, navset_underline, nav_panel],
  bsicons[bs_icon],
  data.table[data.table, as.data.table],
  reactable[reactableOutput, reactable, colDef, reactableTheme],
  htmlwidgets[JS],
  shinyWidgets[switchInput],
  shinybrowser[is_device_mobile],
  graphics[par],
  stringr[str_pad],
  stats[setNames],
  rugarch[infocriteria,ugarchfit,ugarchspec, plot],
)
box::use(app / logic / general_utils[in_card_subtitle_style])

#' @export
models = c(
  "GARCH" = "GARCH",
  "eGARCH" = "eGARCH",
  "GJR - GARCH" = "GJRGARCH",
  "APARCH" = "APARCH",
  "IGARCH" = "IGARCH",
  "FIGARCH" = "FIGARCH"
)

submodels = c("GARCH", "TGARCH", "AVGARCH", "NGARCH", "NAGARCH", "APARCH","GJRGARCH", "ALLGARCH")

#' @export
model_checkbox <- function(id){
  tags$div(
    class = "multicol",
    checkboxGroupInput(id,
                       label = NULL,
                       selected = c("GARCH", "eGARCH"),
                       choices = models)
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

distributions = c("Normal" = "norm",
                  "Student-t" = "std",
                  "Skewed Normal" = "snorm",
                  "Skewed Student-t" = "sstd")

#' @export
conditional_model <- function(ns , model){
  conditionalPanel(ns = ns,
                   condition = sprintf("input.modelCheckbox.includes('%s')",
                                       make_id(model) ),
                   parameter_card(
                     ns = ns,
                     model = model,
                     title = paste0(names(models[which(models == model)]), " Model"),
                     body = model_body(ns = ns,
                                       model = model)
                   )
  )
}

parameter_card <- function(title, model, body, ns) {
  card(
    settings_header(title, model, ns),
    card_body(gap = 0, body)
  )
}

settings_header <- function(title, model, ns ) {
  card_header(
    tags$div(class = "d-flex justify-content-between align-items-center",
             tags$div(class = "d-flex align-items-center",
                      tags$div(style = "padding-top: 5px;", title),
                      tags$div(style = "margin-left: 10px; padding-top: 5px; width: 30px; height: 30px;",
                               id = ns(make_id(model,"loader")))
             ),
             tags$div(class = "d-flex justify-content-end align-items-center",
                      autofitSettings(ns, model),
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

autofitSettings <- function(ns, model){
  popover(
    bs_icon("gear", title = "Settings", marginTop = "3px", marginRight = "10px"),
    title = "Autofit Settings",
    numericInput( ns(make_id(model,"garchRange")), "max GARCH", 3 ),
    numericInput( ns(make_id(model,"armaRange")), "max ARMA", 0 )

  )
}



body_subtitle <- function(text, additional_style = "") tags$p(text, style = paste(in_card_subtitle_style,paste0("font-size: 0.9rem;",additional_style)))
pad_reactable <- function(outputId) tags$div(style = "padding: 0 10px;", reactableOutput(outputId = outputId) )

#' @export
model_body <- function(ns, model){

  navset_underline(header = tags$hr(style = "margin: 0; margin-top: 10px"),

                   nav_panel("Parameters",

                             conditionalPanel(ns = ns,
                                              condition = sprintf("input.%s === true", make_id(model, "switch")),
                                              tags$div(style = "padding-top: 10px; padding-bottom: 10px;",
                                                       reactableOutput(ns(make_id(model, "autoTable")))
                                              )
                             ),

                             tags$div(
                               body_subtitle("Distribution","padding-top: 15px"),
                               tags$div(style = "width: 50%;", class = "model-param",
                                        selectizeInput(inputId = ns(make_id(model,"dist")),
                                                       label = NULL, width = "100%",
                                                       choices = distributions)),

                               if(model == "FIGARCH"){tags$div(
                                 body_subtitle("fGARCH submodel"),
                                 tags$div(style = "width: 50%;", class = "model-param",
                                          selectizeInput(inputId = ns(make_id(model,"submodel")),
                                                         label = NULL, width = "100%",
                                                         choices = submodels)))},

                               body_subtitle("GARCH Order"),
                               tags$div(style = "display: flex; gap: 10px; width: 50%;", class = "model-param",
                                        numericInput(inputId = ns(make_id(model,"p")),
                                                     label = NULL, value = 1, min = 0),
                                        numericInput(inputId = ns(make_id(model,"q")),
                                                     label = NULL, value = 1, min = 0)
                               ),

                               body_subtitle("ARMA Order"),
                               tags$div(style = "display: flex; gap: 10px; width: 50%;", class = "model-param",
                                        numericInput(inputId = ns(make_id(model,"ar")),
                                                     label = NULL, value = 0, min = 0),
                                        numericInput(inputId = ns(make_id(model,"ma")),
                                                     label = NULL, value = 0, min = 0)),
                               checkboxInput(inputId = ns(make_id(model, "includeMean")), value = FALSE, label = "Include mean")
                             )
                   ),
                   nav_panel("Results",
                             tags$div(

                               body_subtitle("Conditional Variance Dynamics:", "padding-top: 15px"),
                               pad_reactable(outputId = ns(make_id(model, "cvdTable"))),

                               body_subtitle("Optimal Parameters:", "padding-top: 15px"),
                               pad_reactable(outputId = ns(make_id(model, "opTable"))),

                               body_subtitle("Robust Standard Errors:", "padding-top: 15px"),
                               pad_reactable(outputId = ns(make_id(model, "rseTable"))),
                               tags$div(
                                 style = "padding: 30px 15px 15px; text-align: center;",
                                 switchInput(
                                   inputId = ns(make_id(model, "rawFitSwitch")),
                                   labelWidth = "100%",
                                   size = "mini",
                                   inline = TRUE,
                                   value = FALSE,
                                   label = "Show raw fit results",
                                   onLabel = "✓",
                                   offLabel = "✕",
                                   width = "auto"
                                 )
                               ),

                               conditionalPanel(
                                 ns = ns,
                                 condition = sprintf("input.%s === true", make_id(model, "rawFitSwitch")),
                                 tags$div(style = "padding-top: 10px; padding-bottom: 10px; height: 300px; overflow: auto;",
                                          verbatimTextOutput(outputId = ns(
                                            make_id(model, "results")
                                          )))
                               )
                             )
                   ),
                   nav_panel("Plots",
                             tags$div(style = "padding: 0 10px;",
                                      selectizeInput(
                                        inputId = ns(make_id(model, "selectPlot")),
                                        label = "Select plot",
                                        selected = 2,
                                        width = "100%",
                                        choices = list(
                                          "Series with 2 Conditional SD Superimposed" = 1,
                                          "Series with 1% VaR Limits" = 2,
                                          "Conditional SD (vs |returns|)" = 3,
                                          "ACF of Observations" = 4,
                                          "ACF of Squared Observations" = 5,
                                          "ACF of Absolute Observations" = 6,
                                          "Cross-Correlations of Squared vs Actual Observations" = 7,
                                          "Empirical Density of Standardized Residuals" = 8,
                                          "norm - QQ Plot" = 9,
                                          "ACF of Standardized Residuals" = 10,
                                          "ACF of Squared Standardized Residuals" = 11,
                                          "New Impact Curve" = 12
                                        )
                                      ),
                                      tags$div(
                                        class = "resize_chart",
                                        plotOutput(ns(make_id(model,"fitPlot")),
                                                   height = "auto")
                                      )
                             ))
  )
}


#' @export
fit_garch <- function(model, p, q, ar = 0, ma = 0, dist, data, info = T, mean = T, submodel = NULL) {

  model_name <- model_switch(model)

  spec <- ugarchspec(
    variance.model = list(model = model_name,
                          garchOrder = c(p, q),
                          submodel = submodel),
    mean.model = list(armaOrder = c(ar, ma), include.mean = mean),
    distribution.model = dist  # Distribution model
  )

  fit <- ugarchfit(spec = spec, data = data)

  # Extract AIC and BIC
  ic <- infocriteria(fit)
  if (info) return(ic) else return(list( fit = fit,
                                         spec = spec))
}




#' @export
get_best_fit <- function(model, returns, input , garchRange = 3, armaRange = 0){

  results <- list()
  distributions_combinations <- expand.grid(p = 1:garchRange,
                                            q = 1:garchRange,
                                            ar = 0:armaRange,
                                            ma = 0:armaRange,
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
        ic <- tryCatch( fit_garch(model, p, q, ar, ma, dist, returns, submodel = if(model == "FIGARCH") input[[make_id(model,"submodel")]]),
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


  # Extract the best fit group
  best_fit_group <- dt[best_fit_index, "Distribution"]
  best_fit <- dt[best_fit_index,]

  dt <- dt[,1:4]
  unique_dt <- unique(dt[, "Distribution", drop = FALSE])

  best_fit_group_index <- which(unique_dt$Distribution == best_fit_group$Distribution)

  reactable(
    unique_dt,
    compact = T,
    highlight = TRUE,
    theme = reactableTheme(
      headerStyle = list(display = "none")
    ),
    details = function(index){
      selected_distribution <- unique_dt$Distribution[index]
      dist_data <- dt[dt$Distribution == selected_distribution, ]
      best_fit_in_dist_data <- which(
        apply(dist_data, 1, function(row) all(row == best_fit[,1:4]))
      )

      tags$div(style = "padding: 1rem",
               reactable(dist_data,
                         outlined = T,
                         compact = T,
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
                             if (length(best_fit_in_dist_data) > 0 && index == best_fit_in_dist_data) {
                               list(backgroundColor = "#FFFFCC", fontWeight = "bold")
                             }
                           },
                           columns = list(
                             Distribution = colDef(
                               width = 150,
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
               )

    },
    rowStyle = function(index) {
      if (index == best_fit_group_index) {
        list(backgroundColor = "#FFFFCC",
             fontWeight = "bold",
             fontSize = "0.8rem")
      }
    }
  )

  }



#' @export
not_null <- function(param, default = 0){
  if(!is.na(param)) return(param) else return(default)
}


#' @export
makeCvdTable <- function(fit){

  garch_model = sprintf("%s(%s,%s)",
                        fit@model[["modeldesc"]][["vmodel"]],
                        fit@model[["modelinc"]][["alpha"]],
                        fit@model[["modelinc"]][["beta"]])

  mean_model = sprintf("ARFIMA(%s,%s,0)",
                       fit@model[["modelinc"]][["ar"]],
                       fit@model[["modelinc"]][["ma"]])

  distribution = fit@model[["modeldesc"]][["distribution"]]


  reactable(data.table(name = c("GARCH Model","Mean Model", "Distribution"),
                       value = c(garch_model,mean_model,distribution)),

            columns = list(
              name = colDef(
                style = list(
                  textAlign = "left",
                  fontSize = "0.8rem",
                  fontWeight = "600",
                  lineHeight = "1.375rem"
                )
              ),
              value = colDef(
                style = list(
                  textAlign = "right",
                  fontSize = "0.8rem"
                )
              )),
            compact = TRUE,
            theme = reactableTheme(
              headerStyle = list(display = "none"),
              cellPadding = "4px 8px"
            )
            )

}

#' @export
makeOpRseTable <- function(fit, type) {

  matrix <- if(type == "op") fit@fit$matcoef else fit@fit$robust.matcoef

  table <- cbind(data.table(name = rownames(matrix), as.data.table(matrix)))

  reactable(table,
            compact = TRUE,
            pagination = FALSE,
            defaultColDef = colDef(
              style = list(
                textAlign = "right",
                fontSize = "0.8rem"
              ),
              cell = function(value) if(is.numeric(value)) round(value,6) else value ),
            columns = list(
              name = colDef(
                header = function(value) gsub("name", "", value),
                style = list(
                  textAlign = "left",
                  fontSize = "0.8rem",
                  fontWeight = "600"
                  )
              )),
            theme = reactableTheme(
              headerStyle = list(
                fontSize = "0.8rem"
              )
            )
  )
}

#' @export
makeFitPlot <- function(fit, num){
  old_par <- par(no.readonly = TRUE)
  par(mar = c(2, 2, 1, 0.1))
  p <- plot(fit, which = as.numeric(num))
  par(old_par)
  return(p)
}
