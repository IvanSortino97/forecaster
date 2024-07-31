box::use(shiny[ img, tags, HTML, shinyOptions, getShinyOption, onSessionEnded, textOutput, withProgress],
         shiny.router[route_link],
         shinyjs[hide, show],
         shinytoastr[toastr_error, toastr_warning],
         bslib[card, page_fillable],
         bsicons[bs_icon],
         spsComps[addLoader],
         yaml[read_yaml, write_yaml])

#' @export
logo <- img(
  src = "static/logo.png",
  height = "40px",
  width = "40px",
  style = "margin-left: 10px"
)

#' @export
#' On start functions
onStart <- function(){
  message('Reading "setting.yaml"')
  yaml <- read_yaml("app/settings.yaml")
  shinyOptions(SETTINGS = yaml)
}

#' @export
#' On close functions
onEnd <- function(){
  onSessionEnded(function(){
  SETT <- getShinyOption("SETTINGS")
  write_yaml(SETT , "app/settings.yaml")
  message('"settings.yaml" updated')
})
}


#' @export
header <- function(title, idtextOutput) {
  tags$div(
    style = "padding: 10px; border-bottom: 1px solid #e9ecef; display: flex; justify-content: left; align-items: center;",
    logo,
    tags$a(
      href = route_link("/"),
      class = "custom-link",
      tags$h4(title, style = "display: inline; margin-left: 15px; color: #464646;")
    ),
    tags$div(style = "flex-grow: 1;"),
    tags$a(
      class = "custom-link",
      href = route_link("stockInfo"),
      style = "display: flex; align-items: center; overflow-x: hidden;",  # Added overflow-x: hidden
      tags$p(
        tags$div(textOutput(idtextOutput), style = "margin-right: 10px; white-space: nowrap;"),  # Prevent line break
        style = "margin: 0; color: #464646;"
      )
    )
  )
}



#' @export
title <- function(text, id = ""){
  tags$div( style = "display: flex; align-items: top;",
       tags$h4(text, style = "font-weight: bold; color: #464646; font-size: 20px;"),
       tags$div(id = id, style = "height: 23px; width: 23px; margin-left: 10px;")
  )
}

#' @export
subtitle <- function(text, size = "14px"){
  tags$p(text, style = sprintf("font-size: %s; color: #7f8189;",size))
}

#' @export
tryCatch_toaster <- function(expr, timeoutToaster = 3000) {

  expr_sub <- substitute(expr)

    tryCatch({
      eval(expr_sub, envir = parent.frame())
    }, warning = function(warning) {
      shinytoastr::toastr_warning(
        title = "Warning",
        message = warning$message,
        position = "top-center",
        timeOut = timeoutToaster,
        closeButton = TRUE
      )
      return(NULL)
    }, error = function(error) {
      shinytoastr::toastr_error(
        title = "Error",
        message = error$message,
        position = "top-center",
        timeOut = timeoutToaster,
        closeButton = TRUE
      )
      return(NULL)
  })
}

#' @export
page_footer <- function(hrefPagePrecedent = NULL, hrefPageNext = NULL, textPagePrecedent = "Previous Page", textPageNext = "Next Page") {
  shiny::div(
    shiny::tags$hr(class = "m-0 mb-2"), # Horizontal line
    if (!is.null(hrefPagePrecedent)) {
      shiny::tags$a(
        href = hrefPagePrecedent,
        class = "btn btn-light btn-sm",
        style = "float: left; border: 1px solid #ccc; padding: 5px 10px; margin: 5px;",
        textPagePrecedent
      )
    },
    if (!is.null(hrefPageNext)) {
      shiny::tags$a(
        href = hrefPageNext,
        class = "btn btn-light btn-sm",
        style = "float: right; border: 1px solid #ccc; padding: 5px 10px; margin: 5px;",
        textPageNext
      )
    },
    style = "overflow: hidden;"
  )
}

#' @export
no_stock_message <- function() {

  tags$div(style = "padding-top: 20px;",
           tags$div(
             class = "alert alert-warning mt-3 text-center",
             tags$h6(class = "alert-heading",
                     "Please select a stock before proceeding."),
             tags$a(
               'Go to "Stock Selection"',
               href = route_link("stockInfo"),
               class = "btn btn-light btn-sm mt-2",
               style = "border: 1px solid #ccc;"
             )
           ))
}

#' @export
select_stock_condition <- function(data, id_message = "conditionalMessage", id_conditional = "conditionalPanel"){

  if (!is.null(data)) {
    show(id_conditional)
    hide(id_message)
  } else {
    hide(id_conditional)
    show(id_message)
  }

}


#' @export
make_spinner <- function(id, type = "facebook"){ addLoader$new(
  target_selector = id,
  type = type,#ripple or dual-ring
  height = "21.33px",
  color = "#757575"
)
}

#' @export
conditional_page_fillable <- function(title,
                                      subtitle,
                                      idLoader = "titleLoader",
                                      idMessage = "conditionalMessage",
                                      idPanel = "conditionalPanel",
                                      ns,
                                      body){


  page_fillable(
    title(title, id = ns(idLoader)),
    subtitle(subtitle),
    tags$div(id = ns(idMessage), no_stock_message()),
    tags$div(id = ns(idPanel), body)
  )
}
