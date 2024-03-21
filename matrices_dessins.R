# Matrices avec des vrais dessins

#Bateau 
bateau <- matrix(0, nrow = 10, ncol = 10)

bateau[10, 3:8] <- 1
bateau[9, 2] <- 1
bateau[9, 9] <- 1
bateau[8, 1] <- 1
bateau[8, 10] <- 1
bateau[8, 3:8] <- 1
bateau[7, 6] <- 1
bateau[6, 6] <- 1
bateau[5, 2:6] <- 1
bateau[4, 3:6] <- 1
bateau[3, 4:6] <- 1
bateau[2, 5:6] <- 1
bateau[1,6] <-1

print(bateau)

#Coccinelle
coccinelle <- matrix(0, nrow = 10, ncol = 10)

coccinelle[10, 4:7]<-1
coccinelle[9, 3]<-1
coccinelle[9, 5:8]<-1
coccinelle[8, 2:6]<-1
coccinelle[8, 8:9]<-1
coccinelle[7, 2:4]<-1
coccinelle[7, 6:9]<-1
coccinelle[6, 2:9]<-1
coccinelle[5, 3]<-1
coccinelle[5, 5:6]<-1
coccinelle[5, 8]<-1
coccinelle[4, 4:7]<-1
coccinelle[3, 5:6]<-1
coccinelle[2, 4]<-1
coccinelle[2, 7]<-1

print(coccinelle)
