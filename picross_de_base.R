library(shiny)
taille_initiale=5

grille <- function(taille){
  return(matrix(sample(c(1, 0), taille*taille, replace=TRUE), nrow=taille))
}


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


vectcol <- function(grille) {
  nb_colonnes <- ncol(grille)
  res <- vector("list", length = nb_colonnes)
  for (i in 1:nb_colonnes) {
    res[[i]] <- compte_grp_col(grille, i)
  }
  return(res)
}

vectlin <- function(grille) {
  nb_lin <- nrow(grille)
  res <- vector("list", length = nb_lin)
  for (i in 1:nb_lin) {
    res[[i]] <- compte_grp_lin(grille, i)
  }
  return(res)
}

# UI de l'application
ui <- fluidPage(
  titlePanel("Picross"),
  sidebarLayout(
    sidebarPanel(
      h3("Instructions"),
      p("Cliquez sur les cases pour les remplir ou les vider."),
      p("Essayez de résoudre le puzzle !"),
      sliderInput("size", "Taille du plateau", min = 5, max = 15, value = taille_initiale)
    ),
    mainPanel(
      uiOutput("grid"),
      style = "width: 35%; height: 35%;overflow: auto;"
    )
  )
)

# Server de l'application
server <- function(input, output, session) {
  board <- reactiveVal(grille(taille_initiale))
  
  observeEvent(input$size, {
    board(grille(input$size))
  })
  
  output$grid <- renderUI({
    grid <- board()
    taille <- input$size
    tags <- list()
    for (i in 1:taille) {
      for (j in 1:taille) {
        id <- paste0("cell_", i, "_", j)
        cell_value <- grid[i, j]
        style <- if (cell_value == 1) "background-color: #333; color: white;" else ""
        tags[[id]] <- actionButton(
          id, 
          "", 
          style = paste0(
            "width: 100%;",
            "height: 0;",
            "padding-top: 100%;",
            style
          )
        )
      }
    }
    div(
      id = "grid-container",
      style = paste0(
        "display: grid;",
        "grid-template-columns: repeat(", taille, ", 1fr);",
        "grid-template-rows: repeat(", taille, ", 1fr);",
        "grid-gap: 1px;"
      ),
      do.call(tagList, tags)
    )
  })
}

# Lancement de l'application
shinyApp(ui = ui, server = server)
