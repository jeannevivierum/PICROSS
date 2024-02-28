library(shiny)
library(shinythemes)
library(bslib)
library(bsicons)

taille_initiale=5

#grille(taille): fonction génère une grille aléatoire de taille taille x taille 
#avec des valeurs aléatoires qui sont des 0 ou des 1 

grille <- function(taille){
  return(matrix(sample(c(1, 0), taille*taille, replace=TRUE), nrow=taille))
}


#compte_grp_lin(grille, num): fonction qui compte les groupes de 1 consécutifs 
#dans la ligne num de la grille. Elle renvoie un vecteur avec le nombre de 1 
#consécutifs pour chaque groupe.

compte_grp_lin <- function(grille, num) {
  taille <- nrow(grille) 
  count_grp <- c() #vecteur renvoyé
  count <- 0
  for (i in 1:taille) { 
    if (grille[num, i] == 1) { #on compte le nombre de 1 qui se suivent
      count <- count + 1
    } else { #si 0, on ajoute le compte a la liste
      if (count > 0) {
        count_grp <- c(count_grp, count)
        count <- 0
      }
    }
  }
  if (count > 0) {
    count_grp <- c(count_grp, count)
  }
  
  return(count_grp)
}


#compte_grp_col(grille, num): fonction qui compte les groupes de 1 consécutifs 
#dans la colonne num de la grille. (idem que celle d'avant mais pour les col)

compte_grp_col <- function(grille, num) {
  taille <- ncol(grille) 
  count_grp <- c() #vecteur renvoyé
  count <- 0
  for (i in 1:taille) { 
    if (grille[i, num] == 1) { #on compte le nombre de 1 qui se suivent
      count <- count + 1
    } else { #si 0, on ajoute le compte a la liste
      if (count > 0) {
        count_grp <- c(count_grp, count)
        count <- 0
      }
    }
  }
  if (count > 0) {
    count_grp <- c(count_grp, count)
  }
  
  return(count_grp)
}

#vectcol(grille): fonction applique compte_grp_col à chaque colonne de la grille
#et renvoie une liste de vecteurs où chaque élément représente les groupes de 1 
#consécutifs dans une colonne.
vectcol <- function(grille) {
  nb_colonnes <- ncol(grille)
  res <- vector("list", length = nb_colonnes)
  for (i in 1:nb_colonnes) {
    res[[i]] <- compte_grp_col(grille, i)
  }
  return(res)
}
#vectlin(grille): même chose mais ligne et pas colonne
vectlin <- function(grille) {
  nb_lin <- nrow(grille)
  res <- vector("list", length = nb_lin)
  for (i in 1:nb_lin) {
    res[[i]] <- compte_grp_lin(grille, i)
  }
  return(res)
}


detectclick <- tags$head(
  tags$script(
    HTML(
      '
      var cellStates = {};
      
      $(document).on("click", ".grid-cell", function() {
        var cell = $(this);
        var cellId = cell.attr("id");
        if (!cell.hasClass("black")) {
          cell.addClass("black");
          cellStates[cellId] = 1; // Marquer la cellule comme noire
          cell.css("background-color", "#333")
        } else {
          cell.removeClass("black");
          cellStates[cellId] = 0; // Marquer la cellule comme blanche
          cell.css("background-color", "white");
        }
        var cellStatesJSON = JSON.stringify(cellStates);
        Shiny.setInputValue("cell_states", cellStatesJSON);
      });
      
      $(document).on("click", ".action-button", function() {
        var buttonId = $(this).attr("id");
        Shiny.onInputChange(buttonId, buttonId);
      });
      '
    )
  )
)
  
# Définissez la fonction verif dans R pour vérifier la grille
verif <- function(cellStates){
  # Convertir les données JavaScript en un objet R
  cellStates <- jsonlite::fromJSON(cellStates)
  # Vérifier chaque cellule
  for (cellId in names(cellStates)) {
    if (cellStates[cellId] == 1) {
      return(FALSE) # Puzzle incorrect si une cellule est noire
    }
  }
  return(TRUE) # Puzzle correct sinon
}

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
    // Reset timer variables when "Rejouer" button is clicked
    timerStarted = false;
    startTime = 0;
  });
')

# UI de l'application
ui <- page_sidebar(
  title = "Picross",
  sidebar = sidebar(
    list(
      actionButton("toggle_instructions", "Instructions"),
      uiOutput("instructions"),
      sliderInput("size", "Taille du plateau", min = 5, max = 15, value = taille_initiale),
      actionButton("go", "Rejouer"),
      actionButton("verif", "Vérifier"),
      card(p("Temps:",id = "clock", "00:00:00"))
    ) 
  ),
  detectclick,
  card(uiOutput("grid",style = "margin: auto;")),
  horloge
)

server <- function(input, output, session) {
  board <- reactiveVal(grille(taille_initiale))
  
  observeEvent(input$go, {
    board(grille(input$size))
    session$sendCustomMessage(type = "startTimer", message = list())
  })
  
  observeEvent(input$size, {
    board(grille(input$size))
  })
  
  
  
  instructions_state <- reactiveValues(show = FALSE)
  
  observeEvent(input$toggle_instructions, {
    instructions_state$show <- !instructions_state$show
  })
  
  output$instructions <- renderUI({
    if (instructions_state$show) {
      tagList(
        p("Cliquez sur les cases pour les remplir ou les vider en respectant les conditions."),
        p("Les nombres en haut de la grille indiquent le nombre de cases à noircir sur la colonne correspondante."),
        p("Les nombres à gauche de la grille indiquent le nombre de cases à noircir sur la ligne correspondante."),
        p("Essayez de résoudre le puzzle !")
      )
    }
  })
  
  observeEvent(input$verif, {
    # Appeler la fonction verif lorsque l'état des cellules est mis à jour
    verif_result <- verif(input$cell_states)
    if (verif_result) {
      showModal(modalDialog(
        title = "Résultat de la vérification",
        "Le puzzle est correct !"
      ))
    } else {
      showModal(modalDialog(
        title = "Résultat de la vérification",
        "Le puzzle contient des erreurs."
      ))
    }
  })
  
  observe({
    grid <- board()
    taille <- input$size
    
    num_col <- lapply(1:taille, function(j) {
      label <- paste(compte_grp_col(grid, j), collapse = "<br>")
      div(HTML(label), style = paste0("text-align: center; white-space: pre-wrap; grid-column: ", j, ";"))
    })
    
    num_lin <- lapply(1:taille, function(i) {
      label <- paste(paste0(compte_grp_lin(grid, i)," "), collapse = "")
      div(HTML(label), style = "text-align: center;")
    })
    
    output$grid <- renderUI({
      div(
        style = "display: grid; grid-template-columns: repeat(auto-fit, minmax(40px, auto)) 1fr; grid-gap: 1px;",
        div(
          style = "display: grid; grid-template-rows: repeat(auto-fit, minmax(40px, auto)); grid-column: 2; justify-content: space-between;",
          do.call(tagList, num_col)
        ),
        div(
          style = "display: grid; grid-template-columns: repeat(auto-fit, minmax(40px, auto)) 1fr; grid-gap: 1px; grid-column: 1; grid-row: 2; align-items: end;",
          do.call(tagList, num_lin)
        ),
        div(
          id = "grid-container",
          style = paste0(
            "grid-column: 2; grid-row: 2; display: grid;",
            "grid-template-columns: repeat(", taille, ", 1fr);",
            "grid-template-rows: repeat(", taille, ", 1fr);",
            "grid-gap: 1px;"
          ),
          lapply(1:taille, function(i) {
            lapply(1:taille, function(j) {
              id <- paste0("cell_", i, "_", j)
              cell_value <- grid[i, j]
              style <- if ((cell_value == 1) || (cell_value == 0)) "background-color: white; border: 1px solid #333;" else ""
              actionButton(
                id, 
                "", 
                style = paste0(
                  "width: 100%;",
                  "height: 0;",
                  "padding-top: 100%;",
                  style
                ),
                class = "grid-cell"
              )
            })
          })
        )
      )
    })
  })
  
  
}

# Lancement de l'application
shinyApp(ui = ui, server = server)
