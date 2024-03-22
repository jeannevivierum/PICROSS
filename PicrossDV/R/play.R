library(shiny)

#' Fonction pour lancer le jeu
#'
#' Cette fonction lance l'application
#' @return Le jeu.
#' @export
play_pic<- function(){return(shinyApp(ui = ui, server = server))}
