box::use(shiny[tags, HTML, ],)



#' @export
html <- tags$head(
  # tags$style(
  #   HTML(
  #     "
  #     "
  #   )
  # ),


  # sidebar style (moved to main.scss) + script
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
  ),
  # show scroll info
  tags$script(
    HTML(
      '
      document.addEventListener("DOMContentLoaded", function() {
        function updateText() {
          var element = document.querySelector("#app-stockAnalysis-infoMobile");
          if (window.innerWidth <= 767) {
            element.textContent = "Scroll to see more";
          } else {
            element.textContent = "";
          }
        }

        // Call updateText on load
        updateText();

        // Update text on window resize
        window.addEventListener("resize", updateText);
      });
      '
    )
  ),
  # show Volume info
  tags$script(
    HTML(
      '
      document.addEventListener("DOMContentLoaded", function() {
        function updateText() {
          var element = document.querySelector("#app-stockInfo-infoVolume");
          if (window.innerWidth <= 767) {
            element.textContent = "";
          } else {
            element.textContent = "Open to see more";
          }
        }

        // Call updateText on load
        updateText();

        // Update text on window resize
        window.addEventListener("resize", updateText);
      });
      '
    )
  ),
)


