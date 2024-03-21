library(shiny)
library(shinythemes)
library(bslib)
library(bsicons)
library(jsonlite)
library(shinyjs)


#' Fonction pour creer l'interface utilisateur Shiny
#'
#' Cette fonction cree une interface utilisateur Shiny avec un titre et une barre latérale.
#' @return Une interface utilisateur Shiny.
#' @export
ui <- page_sidebar(
  theme = bs_theme(bootswatch= "minty", primary = "#661951", secondary = "#ECBADE",
                   success = "#661951",`enable-shadows` = TRUE, spacer = "0.5rem"),
  title = "Picross",
  sidebar = sidebar(
    list(
      actionButton("toggle_instructions", "Instructions"),
      uiOutput("instructions"),
      sliderInput("size", "Taille du plateau", min = 5, max = 15, value = 5),
      card(
        radioButtons("difficulte", card_header("Difficulté"),
                     choices = c("Facile", "Moyen", "Difficile", "Très difficile"),
                     selected = "Facile")
      ),
      actionButton("go", "Jouer"),
      actionButton("verif", "Vérifier"),
      card(p("Temps:",id = "clock", "00:00:00"),class="text-success"),
      card(uiOutput("num_tentatives"))
    )
  ),
  detectclick <- tags$head(
    tags$script(
      HTML(
        '
        var cellStates = {};

        $(document).on("click", ".grid-cell", function() {
          var cell = $(this);
          var cellId = cell.attr("id");


          if (isValidCellId(cellId)) {
            if (!cell.hasClass("black")) {
              cell.addClass("black");
              cellStates[cellId] = 1;
              cell.css("background-color", "#661951")
            } else {
             cell.removeClass("black");
             cellStates[cellId] = 0;
              cell.css("background-color", "#ECBADE");
            }
            var cellStatesJSON = JSON.stringify(cellStates);
           Shiny.setInputValue("cell_states", cellStatesJSON);


          Shiny.setInputValue("clicked_cell_id", cellId);
        } else {
         console.error("ID de cellule non valide : " + cellId);
         }
        });

        function isValidCellId(cellId) {
          return (cellId !== undefined && cellId !== null && cellId !== "");
        }
        function clearCellStates() {
          cellStates = {};
          Shiny.setInputValue("cell_states", "{}");
        }
      $(document).on("click", "#go", function() {
        clearCellStates();
      });
      '
      )
    )
  ),
  card(uiOutput("grid",style = "margin: auto;")),
  horloge <- tags$script('
    var timerStarted = false; // Variable to track if timer has started
    var startTime = 0; // Variable to store start time

    Shiny.addCustomMessageHandler("startTimer", function(message) {
      if (!timerStarted) { // Start timer only if not started already
        startTime = new Date().getTime();
        timerStarted = true;

        function updateClock() {
          var currentTime = new Date().getTime();
          var timeDiff = currentTime - startTime;
          var seconds = Math.floor((timeDiff / 1000) % 60);
          var minutes = Math.floor((timeDiff / (1000 * 60)) % 60);
          var hours = Math.floor((timeDiff / (1000 * 60 * 60)) % 24);
          $("#clock").text(
            "Temps: " +
            ("0" + hours).slice(-2) + ":" +
            ("0" + minutes).slice(-2) + ":" +
            ("0" + seconds).slice(-2)
          );
        }
        setInterval(updateClock, 1000);
      }
    });

    $(document).on("click", "#go", function() {
      timerStarted = false;
      startTime = 0;
    });
  ')
)
