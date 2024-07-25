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
           tags$style(HTML("
             .custom-link:hover {
               background-color: inherit;
               color: inherit;
             }
           ")),
           logo,
           tags$a(href = route_link("/"),
                  class = "custom-link",
                  style = "display: inline; padding: 0; text-decoration: none; font-size: inherit; color: inherit;",
                  tags$h4(title, style = "display: inline; margin-left: 15px;"))
  )
}

#' @export
title <- function(text){
  tags$h4(text, style = "font-weight: bold; color: #464646; font-size: 20px;")

}

#' @export
subtitle_box <- function(text){
  card(text)
}
