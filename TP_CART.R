
#Charger le jeu de données dans R. Transformer la variable de sortie y en facteur.
setwd('C:/Users/pc/Documents/Cours/App-sup/Course/Course_17_afaire/Cours6_afaire_Dtree_/Cours_CART/data4')

rm(list=ls())
#data <- read.table(file="../../data/synth_train.txt", header=TRUE)
data <- read.table("synth_train.txt", header=TRUE)
dim(data)
## [1] 100 3
data$y <- as.factor(data$y)


## Charger le package rpart (rpart = recursive partitioning) et consulter l'aide de la fonction rpart.

library(rpart)
help(rpart)

## Construire un arbre de classification t à l'aide de la fonction rpart (attention la fonction demande
##une formule du style y. et des données data=data_synth). Faire afficher l'arbre en tapant t,
## tracer l'arbre à l'aide des fonctions plot(t) puis text(t).

t <- rpart(y~., data=data)
t

##
plot(t)
text(t)

##

## Calculer l'erreur d'apprentissage du prédicteur obtenu.
pred <- predict(t, newdata=data, type="class")
sum(pred!=data$y)/length(pred)
## [1] 0.08

## Charger le jeu de données test puis calculer le taux d'erreur test.

data.test <- read.table("synth_test.txt", header=TRUE)
dim(data.test)
## [1] 200 3
pred.test <- predict(t, newdata=data.test, type="class")
sum(pred.test!=data.test$y)/length(pred.test)
## [1] 0.19


## Faire de même avec l'arbre maximal (on pourra regarder la fonction rpart.control qui permet
##                                    de régler les règles de construction d'un arbre).

max.cont <- rpart.control(minsplit=2, cp=0)
t.max <- rpart(y~., data=data, control=max.cont)
t.max

##

plot(t.max)
text(t.max)

pred.max<- predict(t.max, newdata=data, type="class")
sum(pred.max!=data$y)/length(pred.max)
## [1] 0
pred.max.test <- predict(t.max, newdata=data.test, type="class")
sum(pred.max.test!=data.test$y)/length(pred.max.test)
## [1] 0.11

z <- as.numeric(data$y)
a <- seq(from=min(data$x1), to=max(data$x1), length.out=100)
b <- seq(from=min(data$x2), to=max(data$x2), length.out=100)

grille <- NULL
for (i in a){
  grille <- rbind(grille, cbind(x1=i,x2=b))
}
grille <- as.data.frame(grille)
# on predit avec t et on trace les predictions
pred.grille <- predict(t, newdata=grille, type="class")
plot(data[,-1], pch=z, col=z)
points(grille, pch=20, col=pred.grille, cex=0.5)
legend("topright", legend=c("classe 1", "classe 2"), pch=1:2, col=1:2)

##idem avec t.max
pred.grille.max <- predict(t.max, newdata=grille, type="class")
plot(data[,-1], pch=z, col=z)
points(grille, pch=20, col=pred.grille.max, cex=0.5)
legend("topright", legend=c("classe 1", "classe 2"), pch=1:2, col=1:2)

## Exercice2

##Charger les jeu de données zip_train.txt et zip_test.txt.
data.zip <- read.table("zip_train.txt", header=TRUE)
dim(data.zip)


##

head(data.zip)[,1:10]
##

data.zip$y <- as.factor(data.zip$y)
data.zip.test <- read.table(file="zip_test.txt", header=TRUE)
dim(data.zip.test)

## test
head(data.zip.test)[,1:10]
data.zip.test$y <- as.factor(data.zip.test$y)
##

#Calculer les taux d'erreur empirique et taux d'erreur test d'un arbre CART construit sur ce jeu de
#données.

t.zip <- rpart(y~., data=data.zip)
t.zip

##

plot(t.zip)
text(t.zip)

## 
# taux d'erreur empirique
zip.pred <- predict(t.zip, newdata=data.zip, type="class")
sum(zip.pred!=data.zip$y)/length(zip.pred)
## [1] 0.208
# taux d'erreur test
zip.pred.test <- predict(t.zip, newdata=data.zip.test, type="class")
sum(zip.pred.test!=data.zip.test$y)/length(zip.pred.test)
## [1] 0.332








