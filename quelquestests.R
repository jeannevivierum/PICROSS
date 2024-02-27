library(shiny)

# Définition de la taille initiale de la grille
taille_initiale <- 5

# Fonction pour générer une grille de picross aléatoire
grille_aleatoire <- function(taille) {
  matrix(sample(c(0, 1), taille * taille, replace = TRUE, prob = c(0.6, 0.4)), nrow = taille)
}

# Fonction pour compter les éléments consécutifs dans un vecteur
compter_consecutifs <- function(vecteur) {
  r <- c()
  count <- 0
  for (i in vecteur) {
    if (i == 1) {
      count <- count + 1
    } else if (count > 0) {
      r <- c(r, count)
      count <- 0
    }
  }
  if (count > 0) {
    r <- c(r, count)
  }
  return(r)
}

# Fonction pour calculer les indices des blocs noirs dans une ligne ou colonne
indices_blocs_noirs <- function(vecteur) {
  r <- c()
  count <- 0
  for (i in vecteur) {
    if (i == 1) {
      count <- count + 1
    } else if (count > 0) {
      r <- c(r, count)
      count <- 0
    }
  }
  if (count > 0) {
    r <- c(r, count)
  }
  if (length(r) == 0) {
    return("0")
  } else {
    return(paste(r, collapse = " "))
  }
}

# Fonction pour calculer les indices des blocs noirs dans chaque ligne
vectlin <- function(grid) {
  apply(grid, 1, indices_blocs_noirs)
}

# Fonction pour calculer les indices des blocs noirs dans chaque colonne
vectcol <- function(grid) {
  apply(grid, 2, indices_blocs_noirs)
}

# UI de l'application
ui <- fluidPage(
  tags$head(
    tags$script(
      HTML(
        '
        $(document).on("click", ".grid-cell", function() {
          var cell = $(this);
          if (!cell.hasClass("black")) {
            cell.addClass("black");
            cell.css("background-color", "#333");
          } else {
            cell.removeClass("black");
            cell.css("background-color", "white");
          }
        });
        '
      )
    )
  ),
  titlePanel("Picross"),
  sidebarLayout(
    sidebarPanel(
      h3("Instructions"),
      p("Cliquez sur les cases pour les remplir ou les vider."),
      p("Essayez de résoudre le puzzle !"),
      sliderInput("size", "Taille du plateau", min = 5, max = 15, value = taille_initiale),
      actionButton("go", "Rejouer")
    ),
    mainPanel(
      uiOutput("grid"),
      style = "width: 35%; height: 35%;overflow: auto;"
    )
  )
)

server <- function(input, output, session) {
  board <- reactiveVal(grille_aleatoire(taille_initiale))
  
  observeEvent(input$size, {
    board(grille_aleatoire(input$size))
  })
  
  output$grid <- renderUI({
    grid <- board()
    taille <- input$size
    
    num_col <- c("°", if (length(vectcol(grid)) == taille) {
      lapply(1:taille, function(j) {
        label <- paste(vectcol(grid)[[j]], collapse = "<br>")
        div(HTML(label), style = "text-align: center;white-space: pre-wrap;")
      })
    } else {
      rep("", taille)
    })
    
    num_lin <- lapply(1:taille, function(i) {
      label <- paste(vectlin(grid)[[i]], collapse = " ")
      div(HTML(label), style = "text-align: center;")
    })
    
    div(
      div(
        style = "display: flex; justify-content: space-between;",
        do.call(tagList, num_col)
      ),
      div(
        style = "display: grid; grid-template-columns: auto 1fr; grid-gap: 1px; align-items: center;",
        div(
          style = "text-align: center;",
          do.call(tagList, num_lin)
        ),
        div(
          id = "grid-container",
          style = paste0(
            "display: grid;",
            "grid-template-columns: repeat(", taille, ", 1fr);",
            "grid-template-rows: repeat(", taille, ", 1fr);",
            "grid-gap: 1px;"
          ),
          lapply(1:taille, function(i) {
            lapply(1:taille, function(j) {
              id <- paste0("cell_", i, "_", j)
              cell_value <- grid[i, j]
              style <- if (cell_value == 1) "background-color: white; border: 1px solid #333;" else ""
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
    )
  })
}

# Lancement de l'application
shinyApp(ui = ui, server = server)
