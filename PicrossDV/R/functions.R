#' Fonction traduire une difficulte
#'
#' Sert a traduire une difficulte en une proportion
#' @param str La difficulte choisie: "Facile", "Moyen", "Difficile", "Très difficile"
#' @return Un pourcentage de cases noirs pour une future grille
#' @export
#' @examples
#' difficulty("Facile")
difficulty <- function(str){
  if(str=="Facile"){return(0.35)}
  else if (str=="Moyen"){return(0.4)}
  else if (str=="Difficile"){return(0.5)}
  else if (str=="Très difficile"){return(0.55)}
}

#' Generer une grille aleatoire
#'
#' Genere une grille aleatoire avec une proportion de cases noirs dependant de la difficulte choisie
#' @param taille La taille de la grille
#' @param difficulte La proportion de cases noires
#' @return Une grille aleatoire
#' @export
#' @examples
#' grille(5,0.3)
grille <- function(taille,difficulte) {
  return(matrix(sample(c(0, 1), taille * taille, replace = TRUE, prob = c(difficulte,1-difficulte)), nrow = taille))
}

#' Compter les groupes de 1 consecutifs lignes
#'
#' Fonction pour compter les groupes de 1 consecutifs dans une ligne de la grille
#' @param grille La grille de jeu
#' @param num Le numero de ligne
#' @return Un vecteur avec le nombre de groupes de 1 consecutifs
#' @export
#' @examples
#' grille_exemple <- matrix(c(1, 0, 1, 1, 0, 1, 0, 1, 1), nrow = 3, byrow = TRUE)
#' compte_grp_lin(grille_exemple, 2)
compte_grp_lin <- function(grille, num) {
  taille <- nrow(grille)
  count_grp <- c() #vecteur renvoye
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

#' Compter les groupes de 1 consecutifs colonnes
#'
#' Fonction pour compter les groupes de 1 consecutifs dans une colonne de la grille
#' @param grille La grille de jeu
#' @param num Le numero de colonne
#' @return Un vecteur avec le nombre de groupes de 1 consecutifs
#' @export
#' @examples
#' grille_exemple <- matrix(c(1, 0, 1, 1, 0, 1, 0, 1, 1), nrow = 3, byrow = TRUE)
#' compte_grp_col(grille_exemple, 2)
compte_grp_col <- function(grille, num) {
  taille <- ncol(grille)
  count_grp <- c() #vecteur renvoye
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

#' Fonction applique compte_grp_col a chaque colonne de la grille.
#'
#' @param grille La grille de jeu
#' @return Une liste de vecteurs où chaque element represente les groupes de 1 consecutifs dans une colonne.
#' @export
#' @examples
#' grille_exemple <- matrix(c(1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 1, 0, 1, 0, 0, 1, 0, 1, 1, 1, 0, 1, 0, 1), nrow = 5, byrow = TRUE)
#' vectcol(grille_exemple)
vectcol <- function(grille) {
  nb_colonnes <- ncol(grille)
  res <- vector("list", length = nb_colonnes)
  for (i in 1:nb_colonnes) {
    res[[i]] <- compte_grp_col(grille, i)
  }
  return(res)
}

#' Fonction applique compte_grp_lin a chaque ligne de la grille.
#'
#' @param grille La grille de jeu
#' @return Une liste de vecteurs où chaque element represente les groupes de 1 consecutifs dans une ligne.
#' @export
#' @examples
#' grille_exemple <- matrix(c(1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 1, 0, 1, 0, 0, 1, 0, 1, 1, 1, 0, 1, 0, 1), nrow = 5, byrow = TRUE)
#' vectlin(grille_exemple)
vectlin <- function(grille) {
  nb_lin <- nrow(grille)
  res <- vector("list", length = nb_lin)
  for (i in 1:nb_lin) {
    res[[i]] <- compte_grp_lin(grille, i)
  }
  return(res)
}


#' Fonction de verification
#'
#' Fonction pour verifier si la grille est resolue correctement, fonctionne si la grille n'est pas a solution unique.
#' @param cellStates L'etat des cellules
#' @param grille La grille de jeu
#' @return TRUE si la grille est correctement resolue, sinon FALSE
#' @export
#' @examples
#' # Creer une grille de jeu de 3x3 avec une solution correcte
#' grille_correcte <- matrix(c(1, 0, 1,
#'                             0, 1, 0,
#'                             1, 0, 1), nrow = 3, byrow = TRUE)
#'
#' # Definir l'etat des cellules
#' cell_states <- '{"cell_1_1":1, "cell_1_2":0, "cell_1_3":1,
#'                  "cell_2_1":0, "cell_2_2":1, "cell_2_3":0,
#'                  "cell_3_1":1, "cell_3_2":0, "cell_3_3":1}'
#'
#' # Verifier si la grille est correctement resolue
#' verif(cell_states, grille_correcte)
verif <- function(cellStates, grille) {
  cell_states <- jsonlite::fromJSON(cellStates)
  matrice <- matrix(0, nrow = nrow(grille), ncol = ncol(grille))
  for (cellId in names(cell_states)) {
    coord <- as.numeric(strsplit(cellId, "_")[[1]][2:3])
    matrice[[coord[1],coord[2]]] <- unlist(cell_states[cellId])[1]
  }
  return(identical(vectlin(matrice),vectlin(grille))&&identical(vectcol(matrice),vectcol(grille)))
}
