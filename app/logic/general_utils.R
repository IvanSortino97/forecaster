box::use(shiny[img, tags, HTML, shinyOptions, getShinyOption, onSessionEnded],
         shiny.router[route_link],
         bslib[card],
         bsicons[bs_icon],
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
header <- function(title) {
  tags$div(style = "padding: 10px 10px 10px 10px;border-bottom: 1px solid #e9ecef; display: flex; justify-content: left; align-items: center;",
           logo,
           tags$a(href = route_link("/"),
                  class = "custom-link",
                  tags$h4(title, style = "display: inline; margin-left: 15px;"))
  )
}

#' @export
title <- function(text){
  tags$h4(text, style = "font-weight: bold; color: #464646; font-size: 20px;")

}

#' @export
subtitle <- function(text, size = "14px"){
  tags$p(text, style = sprintf("font-size: %s; color: #7f8189;",size))
}
