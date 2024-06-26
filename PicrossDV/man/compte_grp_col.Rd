% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions.R
\name{compte_grp_col}
\alias{compte_grp_col}
\title{Compter les groupes de 1 consecutifs colonnes}
\usage{
compte_grp_col(grille, num)
}
\arguments{
\item{grille}{La grille de jeu}

\item{num}{Le numero de colonne}
}
\value{
Un vecteur avec le nombre de groupes de 1 consecutifs
}
\description{
Fonction pour compter les groupes de 1 consecutifs dans une colonne de la grille
}
\examples{
grille_exemple <- matrix(c(1, 0, 1, 1, 0, 1, 0, 1, 1), nrow = 3, byrow = TRUE)
compte_grp_col(grille_exemple, 2)
}
