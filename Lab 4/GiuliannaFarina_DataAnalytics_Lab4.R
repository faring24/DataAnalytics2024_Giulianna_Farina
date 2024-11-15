####### Data Analytics Fall 2024 Lab 4 ######
### Giulianna Farina

### set working directory
setwd("~/Documents/Data Analytics/Labs/Lab 4")

library(ggplot2)
library(tidyverse)
library(ggfortify)
library(e1071)
library(class)
library(psych)
library(readr)
library(caret)

# Read in data
wine <- read_csv("wine.data")
names(wine) <- c("Type","Alcohol","Malic acid","Ash","Alcalinity of ash","Magnesium","Total phenols","Flavanoids","Nonflavanoid Phenols","Proanthocyanins","Color Intensity","Hue","Od280/od315 of diluted wines","Proline")
head(wine)

wine$Type <- as.factor(wine$Type)

wine <- wine[,-c(4,5,10)]

pairs.panels(wine[,-1],gap = 0,bg = c("red", "yellow", "blue")[wine$Type],pch=21)

wine.df <- wine
head(wine.df)


# creating dataframe from wine dataset, omitting first column
wine.X <- wine.df[,-1]
wine.X

principal_components <- princomp(wine.X, cor = TRUE, score = TRUE)

summary(principal_components)

principal_components$loading

## 1: Compute the PCs and plot the dataset using the 1st and 2nd PC.
autoplot(principal_components, data = wine, colour = 'Type',
         loadings = TRUE, loadings.colour = 'violet',
         loadings.label = TRUE, loadings.label.size = 3)



## 2: Identify the variables that contribute the most to the 1st PC
### The variables that contribute the most to the 1st PC is Nonflavanoid Phenols, Malic Acid, and Color Intensity. These numbers all were less than 0, contributing the most amount of change to the first CP value.

## 3: Train a classifier model to predict wine type using the 13 attributes
k=13
knn.pred <- knn(train = wine[,-1], test = wine[,-1], cl = wine$Type, k = k)

cm <- table(Predicted=knn.pred, Actual = wine$Type, dnn=list('predicted','actual'))
cm

n = sum(cm) # number of instances
nc = nrow(cm) # number of classes
diag = diag(cm) # number of correctly classified instances per class 
rowsums = apply(cm, 1, sum) # number of instances per class
colsums = apply(cm, 2, sum) # number of predictions per class
p = rowsums / n # distribution of instances over the actual classes
q = colsums / n # distribution of instances over the predicted 

precision = diag / colsums 
recall = diag / rowsums 
f1 = 2 * precision * recall / (precision + recall) 

data.frame(recall, precision, f1)

## 4: Train a classifier model to predict wine type using the data projected into the first 3 PCs

k=13
knn.pred2 <- knn(train = principal_components$scores[,1:3], test = principal_components$scores[,1:3], cl = wine$Type, k = k)
cm2 <- table(Predicted=knn.pred2, Actual = wine$Type, dnn=list('predicted','actual'))
cm2




## 5: Drop the variables least contributing to the 1st PC and rerun PCA
wine.X1 <- wine[,-3,6,8]
wine.X1


new_princ_components <- princomp(wine.X, cor = TRUE, scores = TRUE)
summary(new_princ_components)

new_princ_components$loadings

new_princ_components

## 6: Train a classifier model to predict wine type using the data projected into the first 3 PCs after rerunning PCA.
k = 13
knn.pred_3pc <- knn(train = new_princ_components$sco[,-1:3], test = new_princ_components$scores[,-1:3], cl = wine$Type, k = k)

cm3pc <- table(Predicted=knn.pred2, Actual = wine$Type, dnn=list('predicted','actual'))
cm3pc

## 7: Compare the 3 classification models using contingency tables and prevision/recall/f1 metrics
cm_model1 <- confusionMatrix(cm)
cm_model1$byClass[, c("Precision", "Recall", "F1")]

cm_model2 <- confusionMatrix(cm2)
cm_model2$byClass[, c("Precision", "Recall", "F1")]

cm_model3 <- confusionMatrix(cm3pc)
cm_model3$byClass[, c("Precision", "Recall", "F1")]
