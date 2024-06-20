box::use(shiny[img, tags, HTML],
         shiny.router[route_link],
         bsicons[bs_icon])

#' @export
logo <- img(
  src = "static/logo.png",
  height = "40px",
  width = "40px",
  style = "margin-left: 10px"
)

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

