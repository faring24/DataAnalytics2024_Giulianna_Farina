####### Data Analytics Fall 2024 Lab 02 Part 2 ######


### set working directory
setwd("~/Documents/Data Analytics/Labs/Lab 2 Part 2")

### read in data
iris <- read.csv("iris.csv", header=TRUE)
abalone <- read.csv("abalone.data", header=FALSE)
abalone.names <- read.csv("abalone.names", header=FALSE)

## Call the NaiveBayes Classifier Package e1071, which auto calls the Class package ##
library("e1071")
classifier<-naiveBayes(iris[,1:5], iris[,5])
table(predict(classifier, iris[,-5]), iris[,5], dnn=list('predicted','actual'))
classifier$apriori
classifier$tables$Petal.Length
plot(function(x) dnorm(x, 1.462, 0.1736640), 0, 8, col="red", main="Petal length distribution for the 3 different species")
curve(dnorm(x, 4.260, 0.4699110), add=TRUE, col="blue")
curve(dnorm(x, 5.552, 0.5518947 ), add=TRUE, col = "green")


## Exercise 1

abalone <- read.csv(url("https://archive.ics.uci.edu/ml/machine-learning-databases/abalone/abalone.data"),
                    header = FALSE, sep = ",")
colnames(abalone) <- c("sex", "length", 'diameter', 'height', 'whole_weight', 'shucked_wieght',
                       'viscera_wieght', 'shell_weight', 'rings' )
abalone$age.group <- cut(abalone$rings, br=c(0,8,11,35), labels = c("young", 'adult', 'old'))
classifier<-naiveBayes(abalone[,2:8], abalone[,10])

### Length Classifier
table(predict(classifier, abalone[,-10]), abalone[,10], dnn=list('predicted','actual'))
classifier$apriori
classifier$tables$length
plot(function(x) dnorm(x, 0.4209915, 0.11137474,), ylim=c(0, 5), 0, 1, col="magenta", main="Abalone length distribution for the 3 different age groups")
curve(dnorm(x, 0.5707182, 0.08740980), add=TRUE, col="turquoise1")
curve(dnorm(x, 0.5868542, 0.08100644), add=TRUE, col = "springgreen")

### Diameter Classifier
table(predict(classifier, abalone[,-10]), abalone[,10], dnn=list('predicted','actual'))
classifier$apriori
classifier$tables$diameter
plot(function(x) dnorm(x, 0.3212758, 0.09029187,), xlim=c(0, 0.8), ylim=c(0, 7), 0, 1, col="magenta", main="Abalone diameter distribution for the 3 different age groups")
curve(dnorm(x, 0.4458591, 0.07153798), add=TRUE, col="turquoise1")
curve(dnorm(x, 0.4632083, 0.06699741), add=TRUE, col = "springgreen")

### Height Classifier
table(predict(classifier, abalone[,-10]), abalone[,10], dnn=list('predicted','actual'))
classifier$apriori
classifier$tables$height
plot(function(x) dnorm(x, 0.1065956, 0.04183039,), xlim=c(0, 0.3), ylim=c(0, 14), 0, 1, col="magenta", main="Abalone height distribution for the 3 different age groups")
curve(dnorm(x, 0.1516906, 0.02984784), add=TRUE, col="turquoise1")
curve(dnorm(x, 0.1648125, 0.02935998), add=TRUE, col = "springgreen")


## Exercise 2
iris <- iris[,-1]
s_iris <- sample(150,100)

n <- nrow(iris.train)
train.indexes <- sample(n,n*.7)
print(train.indexes)

iris.train <-iris[s_iris,]
iris.test <-iris[-s_iris,]

k = 10

### Subset 1
KNNpred <- knn(train = iris.train[1:3], test = iris.test[1:3], cl = iris.train$Species, k = k)
contingency.table <- table(KNNpred,iris.test$Species)
contingency.table

contingency.matrix = as.matrix(contingency.table)
sum(diag(contingency.matrix))/length(iris.test$Species)

#### train knn model
KNNpred <- knn(train = iris.train[1:3], test = iris.test[1:3], cl = iris.train$Species, k = k)

#### evaluate
contingency.table <- table(Predicted = KNNpred, Actual = iris.test$Species, dnn=list('predicted', 'actual'))
print(contingency.table)
contingency.matrix = as.matrix(contingency.table)
sum(diag(contingency.matrix))/length(iris.test$Species)

### run text with multiple k values
accuracy <- c()
ks <- c(5,7,9,11,13)
for (k in ks) {
  KNNpred <- knn(train = iris.train[1:3], test = iris.test[1:3], cl = iris.train$Species, k = k)
  
  cm = as.matrix(table(Actual=KNNpred, Predicted = iris.test$Species, dnn=list('predicted','actual')))
  
  accuracy <- c(accuracy,sum(diag(cm))/length(iris.test$Species))
}
plot(ks,accuracy,type = "b")


### Subset 2
KNNpred <- knn(train = iris.train[2:4], test = iris.test[2:4], cl = iris.train$Species, k = k)
contingency.table <- table(KNNpred,iris.test$Species)
contingency.table

contingency.matrix = as.matrix(contingency.table)
sum(diag(contingency.matrix))/length(iris.test$Species)

#### train knn model
KNNpred <- knn(train = iris.train[2:4], test = iris.test[2:4], cl = iris.train$Species, k = k)

#### evaluate
contingency.table <- table(Predicted = KNNpred, Actual = iris.test$Species, dnn=list('predicted', 'actual'))
print(contingency.table)
contingency.matrix = as.matrix(contingency.table)
sum(diag(contingency.matrix))/length(iris.test$Species)

### run text with multiple k values
accuracy <- c()
ks <- c(5,7,9,11,13)
for (k in ks) {
  KNNpred <- knn(train = iris.train[2:4], test = iris.test[2:4], cl = iris.train$Species, k = k)
  
  cm = as.matrix(table(Actual=KNNpred, Predicted = iris.test$Species, dnn=list('predicted','actual')))
  
  accuracy <- c(accuracy,sum(diag(cm))/length(iris.test$Species))
}
plot(ks,accuracy,type = "b")


## Exercise 3
### Abalone Dataset
#### plot dataset colored by class
ggplot(abalone, aes(x = length, y = shell_weight, colour = age.group)) +
  geom_point()

#### train kmeans
abalone.km <- kmeans(abalone[,5], centers = 3)

## WCSS: total within cluster sum of squares
abalone.km$tot.withinss

## get and plot clustering output 
assigned.clusters <- as.factor(abalone.km$cluster)

ggplot(abalone, aes(x = length, y = shell_weight, colour = assigned.clusters)) +
  geom_point()

## run tests with multiple k values and plot WCSS
wcss <- c()
ks <- c(4,5,6,7,8,9)

for (k in ks) {
  
  abalone.km <- kmeans(abalone[,5], centers = 3)
  
  wcss <- c(wcss,abalone.km$tot.withinss)
  
}

plot(ks,wcss,type = "b")

### Iris Dataset
#### plot dataset colored by class
ggplot(iris, aes(x = Sepal.Length, y = Petal.Length, colour = Species)) +
  geom_point()

#### train kmeans
iris.km <- kmeans(iris[,4], centers = 3)

## WCSS: total within cluster sum of squares
iris.km$tot.withinss

## get and plot clustering output 
iris.assigned.clusters <- as.factor(iris.km$cluster)
iris.assigned.clusters

ggplot(iris, aes(x = Sepal.Length, y = Petal.Length, colour = iris.assigned.clusters)) +
  geom_point()

## run tests with multiple k values and plot WCSS
wcss <- c()
ks <- c(2,3,4,5,6,7)

for (k in ks) {
  
  iris.km <- kmeans(iris[,5], centers = 3)
  
  wcss <- c(wcss,iris.km$tot.withinss)
  
}

plot(ks,wcss,type = "b")

