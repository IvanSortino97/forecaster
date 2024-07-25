box::use(shiny[ actionButton, tags, HTML],
         bslib[sidebar, accordion, accordion_panel],
         shiny.router[route_link],
         bsicons[bs_icon])

accordion_section <- function(title, icon = NULL, page){
  tags$a(href = route_link(page), if(!is.null(icon)){ bs_icon(icon)}, tags$h6(title, style = "display: inline; margin-left: 5px;") )
}

#' @export
sidebar_component <- sidebar(

  # Home
  accordion_section(title = "Home",
                    page = "/",
                    icon = "house"),

  # Stock info
  accordion(
    accordion_panel(
      "Stock info",
      icon = bs_icon("graph-up"),
      accordion_section(title = "Stock selection",
                        page = "stockInfo",
                        icon = "list-task"),
      accordion_section(title = "Page 2",
                        page = "page2",
                        icon = "apple")
    ),

    # Test buttons
    accordion_panel(
      "Section 2"
    )
  )

)


#' @export
#' define style of a components and appear like pushed buttons
head <- tags$head(
  tags$style(
    HTML(
      "
         a {
          padding: 10px 15px;
          text-decoration: none;
          font-size: 18px;
          color: black;
          display: block;
        }
         a:hover {
          background-color: grey;
          color: white;
        }
         a.active {
          background-color: blue;
          color: white;
        }
      "
    )
  ),
  tags$script(
    HTML(
      "
        $(document).ready(function() {
          function updateActiveClass() {
            if (window.location.hash === '#!/') {
              $('.sidebar a').removeClass('active');
            }
          }

          // Run on initial load
          updateActiveClass();

          // Add click event listener
          $('.sidebar a').click(function() {
            $('.sidebar a').removeClass('active');
            $(this).addClass('active');
          });

          // Monitor hash changes
          $(window).on('hashchange', function() {
            updateActiveClass();
          });
        });
      "
    )
  )
)

