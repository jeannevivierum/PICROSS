
taille=10

grille <- matrix(sample(c(1, 0), 100, replace=TRUE), nrow=taille)



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

