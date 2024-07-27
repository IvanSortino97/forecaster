box::use(shiny[tags, HTML, ],)



#' @export
html <- tags$head(


  # sidebar style + script
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
  ),
  #scroll to top when page is changed
  tags$script(
    HTML(
      "
    $(document).ready(function() {
      Shiny.addCustomMessageHandler('scrollToTop', function(message) {
        // Select the specific container by its class
        var container = document.querySelector('div.main.bslib-gap-spacing.html-fill-container');

        if (container) {
          container.scrollTop = 0; // Scroll to the top
        }
      });
    });
    "
    )
  ),
  # Resize Squared returns title tab
  tags$script(
    HTML(
      'document.addEventListener("DOMContentLoaded", function() {
        function updateText() {
          var element = document.querySelector("#app-stockAnalysis-navsetPlots li:nth-child(2) a");
          if (window.innerWidth <= 767) {
            element.textContent = "Sq. returns";
          } else {
            element.textContent = "Squared returns";
          }
        }

        // Run on initial load
        updateText();

        // Run on window resize
        // window.addEventListener("resize", updateText);
      });'
    )
  )
)


