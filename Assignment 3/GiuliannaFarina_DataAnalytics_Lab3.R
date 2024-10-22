####### Data Analytics Fall 2024 Lab 3 / Assignment 2 - Giulianna Farina ######


### set working directory
setwd("~/Documents/Data Analytics/Labs/Lab 3")

### read in data
lab3_epi <- read.csv("epi2024results_DA_F24_lab03.csv", header=TRUE)

## Variable Distributions
### 1. Derive Subsets
lab3_epi_global_west <- lab3_epi[lab3_epi$region=="Global West",]
lab3_epi_eastern_europe <- lab3_epi[lab3_epi$region=="Eastern Europe",]

### 1.1 Histograms
hist(lab3_epi_global_west$BDH, seq(10., 100., 2.0), col = "plum", main = "Histogram for Global West BDH", prob=TRUE)
lines(density(lab3_epi_global_west$BDH, na.rm=TRUE, bw="SJ"))

hist(lab3_epi_eastern_europe$ECO, seq(0., 100., 2.0), col = "seagreen1", main = "Histogram for Eastern Europe ECO", prob=TRUE)
lines(density(lab3_epi_eastern_europe$ECO, na.rm=TRUE, bw="SJ"))

### 1.2 QQ Plots
qqnorm(lab3_epi_global_west$BDH, main = "Q-Q Plot for Global West BDH")
qqline(lab3_epi_global_west$BDH, col="plum")

qqnorm(lab3_epi_eastern_europe$ECO, main = "Histogram for Eastern Europe ECO")
qqline(lab3_epi_eastern_europe$ECO, col="seagreen1")


## Linear Models
### 2.1
lin_model <- lm(EPI ~ ECO + BDH + MKP + MHP + MPE, data = lab3_epi)
summary(lin_model)

plot(lab3_epi$ECO, lab3_epi$BDH, main = "EPI: ECO vs BDH with Fitted Line",
     xlab = "ECO", ylab = "BDH", pch = 19, col = "steelblue1")
abline(lm(BDH ~ ECO, data = lab3_epi), col = "pink", lwd = 2)

### 2.2
lin_model_gw <- lm(EPI ~ ECO + PAR + SPI + TBN + TKP, data = lab3_epi_global_west)
summary(lin_model_gw)

plot(lab3_epi_global_west$ECO, lab3_epi_global_west$PAR, main = "EPI Global West: ECO vs PAR with Fitted Line",
     xlab = "ECO", ylab = "PAR", pch = 19, col = "pink")
abline(lm(PAR ~ ECO, data = lab3_epi_global_west), col = "steelblue1", lwd = 2)
### After comparing the two models, I believe the 2.1 model is a better fit. This is because there is a larger amount of data to plot and the fitted line aligns better with the results on the plot. 


## Classification (kNN)
### 3
knn_vars <- c('EPI', 'ECO', 'BDH', 'MKP', 'BER', 'region')
dfq3 <- lab3_epi[, knn_vars]
head(dfq3)

### 3.1
x <- dfq3[, knn_vars[1:5]]
y <- dfq3$region 

n <- nrow(dfq3)
train_indexes <- sample(n,n*.7)
print(train_indexes)

dfq3.train <- dfq3[train_indexes, ]
dfq3.test <- dfq3[-train_indexes, ] 

y_train <- dfq3.train$region 
y_test <- dfq3.test$region
x_train <- dfq3.train[, 1:5]
x_test <- dfq3.test[, 1:5]

k = 5
KNNpred <- knn(train = x_train[1:3], test = x_test[1:3], cl = y_train, k = k)
contingency.table <- table(KNNpred, x_test$EPI)
contingency.table
contingency.matrix = as.matrix(contingency.table)
sum(diag(contingency.matrix))/length(x_test$EPI)

### 3.2
contingency.table <- table(KNNpred, y_test$ECO)
contingency.table
contingency.matrix = as.matrix(contingency.table)
sum(diag(contingency.matrix))/length(y_test$ECO)

contingency.table <- table(KNNpred, x_test$BDH)
contingency.table
contingency.matrix = as.matrix(contingency.table)
sum(diag(contingency.matrix))/length(x_test$BDH)

contingency.table <- table(KNNpred, x_test$MKP)
contingency.table
contingency.matrix = as.matrix(contingency.table)
sum(diag(contingency.matrix))/length(x_test$MKP)

### The first knn model seems to provide more accurate results when comparing the regions and second model.


## Clustering
### 4
regions1 <- subset(lab3_epi, region %in% c("Southern Asia", "Eastern Europe", "Greater Middle East"))
regions2 <- subset(lab3_epi, region %in% c("Global West", "Asia-Pacific", "Latin America & Caribbean"))

### 4.1 
regions1_data <- as.matrix(regions1[knn_vars])
regions2_data <- as.matrix(regions2[knn_vars])

wcss_matrix <- matrix(NA, nrow = length(ks), ncol = 2)
colnames(wcss_matrix) <- c("Regions1_WCSS", "Regions2_WCSS")
rownames(wcss_matrix) <- as.character(ks)

### 4.2
ks <- c(10,20,30,40,50,60)
wcss_matrix <- matrix(NA, nrow = length(ks), ncol = 5)
wcss_df <- as.data.frame(wcss_matrix)
print(wcss_df)

for (k in ks) {
  
  regions1.km <- kmeans(lab3_epi[,1], centers = 1)
  wcss <- c(wcss,regions1.km$tot.withinss)
  
  regions2.km <- kmeans(lab3_epi[,1], centers = 1)
  wcss <- c(wcss,regions2.km$tot.withinss)
  
}

plot(ks,wcss_df,type = "b")

### I believe the second model is better since there is greater variance among regions.