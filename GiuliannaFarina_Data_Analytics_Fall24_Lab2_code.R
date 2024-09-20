####### Data Analytics Fall 2024 Lab 01 ######

library(ggplot2)

### set working directory
setwd("~/Documents/Data Analytics/Labs/Lab 2")

### read in data
epi.results <- read.csv("epi2024results06022024.csv", header=TRUE)
epi.weights <- read.csv("epi2024weights.csv")

View(epi.results)
View(epi.weights)

#### Exploratory Analysis ####

epi.results$EPI.new

epi.results[1,5]

attach(epi.results)

EPI.new

EPI.new[1]

## NA values
na.indices <- is.na(EPI.new) 

## drop NAs
Epi.new.compl <- EPI.new[!na.indices]

## convert to data frame and add country
Epi.new.compl <- data.frame(Country = country[!na.indices], EPI = EPI.new[!na.indices])

## summary stats
summary(EPI.new)

fivenum(EPI.new,na.rm=TRUE)

## histograms
hist(EPI.new)

hist(EPI.new, seq(20., 80., 2.0), prob=TRUE)

rug(EPI.new)

lines(density(EPI.new,na.rm=TRUE,bw=1))
lines(density(EPI.new,na.rm=TRUE,bw="SJ"))

x <- seq(20., 80., 1.0)
qn<- dnorm(x,mean=42, sd=5,log=FALSE)
lines(x,0.4*qn)

qn<- dnorm(x,mean=65, sd=5,log=FALSE)
lines(x,0.12*qn)

##################

### Comparing distributions of 2 variables

boxplot(EPI.old, EPI.new, names=c("EPI.old","EPI.new"))

### Quantile-quantile plots

qqnorm(EPI.new)
qqline(EPI.new)

x <- seq(20., 80., 1.0)
qqplot(qnorm(ppoints(200)), x)
qqline(x)

qqplot(qnorm(ppoints(200)),EPI.new)
qqline(EPI.new)

qqplot(rnorm(1000),EPI.new)
qqline(EPI.new)

d1 <- rnorm(10000)
d2 <- rnorm(10000)
qqplot(d1,d1)
qqline(d1)

### Empirical Cumulative Distribution Function
plot(ecdf(EPI.new), do.points=FALSE) 

plot(ecdf(rnorm(1000, 45, 10)), do.points=FALSE, main="Norm Dist vs. EPI.new ECDF")
lines(ecdf(EPI.new))

plot(ecdf(EPI.old), do.points=FALSE, main="EPI.old vs. EPI.new ECDF")
lines(ecdf(EPI.new))


#### Populations Dataset ####

## read data
populations_2023 <- read.csv("~/Documents/Data Analytics/Labs/Lab 2/countries_populations_2023.csv")

## drop country populations that don't exist in epi results
populations <- populations_2023[-which(!populations_2023$Country %in% epi.results$country),]

## sort populations by country name
populations <- populations[order(populations$Country),]

## drop country results that don't exist in populations
epi.results.sub <- epi.results[-which(!epi.results$country %in% populations$Country),]

## sort results by country name
epi.results.sub <- epi.results.sub[order(epi.results.sub$country),]

## only keep relevant columns
epi.results.sub <- epi.results.sub[,c("country","EPI.old","EPI.new","BDH.new", "MKP.new", "MHP.new")]

## convert to mnumeric
epi.results.sub$population <- as.numeric(populations$Population)

## compute population log
epi.results.sub$population_log <- log10(epi.results.sub$population)

boxplot(epi.results.sub$population_log)

## attach(epi.results.sub)

## created linear model of EPI.new = a(population_log) + b
lin.mod.epinew <- lm(EPI.new~population_log,epi.results.sub)

plot(EPI.new~population_log)
abline(lin.mod.epinew)

summary(lin.mod.epinew)

plot(lin.mod.epinew)


ggplot(epi.results.sub, aes(x = population_log, y = EPI.new)) +
  geom_point() +
  stat_smooth(method = "lm")

ggplot(lin.mod.epinew, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  labs(title='Residual vs. Fitted Values Plot', x='Fitted Values', y='Residuals')


## another lm
lin.mod.pop <- lm(population_log~EPI.new,epi.results.sub)
plot(population_log~EPI.old)
abline(lin.mod.pop)

summary(lin.mod.pop)

plot(lin.mod.pop)


ggplot(epi.results.sub, aes(x = EPI.old, y = population_log)) +
  geom_point() +
  stat_smooth(method = "lm")

ggplot(lin.mod.pop, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  labs(title='Residual vs. Fitted Values Plot', x='Fitted Values', y='Residuals')


## Giulianna Farina Lab 2 Work

### Comparing distributions with 3 other variables

boxplot(BDH.new, MKP.new, MHP.new, names=c("BDH.new", "MKP.new", "MHP.new"))

### Quantile-quantile plots for variable BDH.new

qqnorm(BDH.new)
qqline(BDH.new)

x <- seq(20., 80., 1.0)
qqplot(qnorm(ppoints(200)), x)
qqline(x)

qqplot(qnorm(ppoints(200)),BDH.new)
qqline(BDH.new)

qqplot(rnorm(1000),BDH.new)
qqline(BDH.new)

d1 <- rnorm(10000)
d2 <- rnorm(10000)
qqplot(d1,d1)
qqline(d1)

### Quantile-quantile plots for variable MKP.new

qqnorm(MKP.new)
qqline(MKP.new)

x <- seq(20., 80., 1.0)
qqplot(qnorm(ppoints(200)), x)
qqline(x)

qqplot(qnorm(ppoints(200)),MKP.new)
qqline(MKP.new)

qqplot(rnorm(1000),MKP.new)
qqline(MKP.new)

d1 <- rnorm(10000)
d2 <- rnorm(10000)
qqplot(d1,d1)
qqline(d1)

### Quantile-quantile plots for variable MHP.new

qqnorm(MHP.new)
qqline(MHP.new)

x <- seq(20., 80., 1.0)
qqplot(qnorm(ppoints(200)), x)
qqline(x)

qqplot(qnorm(ppoints(200)),MHP.new)
qqline(MHP.new)

qqplot(rnorm(1000),MHP.new)
qqline(MHP.new)

d1 <- rnorm(10000)
d2 <- rnorm(10000)
qqplot(d1,d1)
qqline(d1)


### Empirical Cumulative Distribution Function for variable BDH.new 
plot(ecdf(BDH.new), do.points=FALSE) 

plot(ecdf(rnorm(1000, 45, 10)), do.points=FALSE, main="Norm Dist vs. BDH.new ECDF")
lines(ecdf(BDH.new))

plot(ecdf(BDH.old), do.points=FALSE, main="BDH.old vs. BDH.new ECDF")
lines(ecdf(BDH.new))

### Empirical Cumulative Distribution Function for variable MKP.new 
plot(ecdf(MKP.new), do.points=FALSE) 

plot(ecdf(rnorm(1000, 45, 10)), do.points=FALSE, main="Norm Dist vs. MKP.new ECDF")
lines(ecdf(MKP.new))

plot(ecdf(BDH.old), do.points=FALSE, main="MKP.old vs. MKP.new ECDF")
lines(ecdf(MKP.new))

### Empirical Cumulative Distribution Function for variable MHP.new 
plot(ecdf(MHP.new), do.points=FALSE) 

plot(ecdf(rnorm(1000, 45, 10)), do.points=FALSE, main="Norm Dist vs. MHP.new ECDF")
lines(ecdf(MHP.new))

plot(ecdf(BDH.old), do.points=FALSE, main="MHP.old vs. MHP.new ECDF")
lines(ecdf(MHP.new))



## Summary stats and plots linear model for BDH.new
lin.mod.pop <- lm(population_log~BDH.new,epi.results.sub)
plot(population_log~BDH.new,epi.results.sub)
abline(lin.mod.pop)

summary(lin.mod.pop)

plot(lin.mod.pop)


ggplot(epi.results.sub, aes(x = BDH.new, y = population_log)) +
  geom_point() +
  stat_smooth(method = "lm")

ggplot(lin.mod.pop, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  labs(title='Residual vs. Fitted Values Plot', x='Fitted Values', y='Residuals')

## Summary stats and plots linear model for MKP.new
lin.mod.pop <- lm(population_log~MKP.new,epi.results.sub)
plot(population_log~MKP.new,epi.results.sub)
abline(lin.mod.pop)

summary(lin.mod.pop)

plot(lin.mod.pop)


ggplot(epi.results.sub, aes(x = MKP.new, y = population_log)) +
  geom_point() +
  stat_smooth(method = "lm")

ggplot(lin.mod.pop, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  labs(title='Residual vs. Fitted Values Plot', x='Fitted Values', y='Residuals')

## Summary stats and plots linear model for MHP.new
lin.mod.pop <- lm(population_log~MHP.new,epi.results.sub)
plot(population_log~MHP.new,epi.results.sub)
abline(lin.mod.pop)

summary(lin.mod.pop)

plot(lin.mod.pop)


ggplot(epi.results.sub, aes(x = MHP.new, y = population_log)) +
  geom_point() +
  stat_smooth(method = "lm")

ggplot(lin.mod.pop, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  labs(title='Residual vs. Fitted Values Plot', x='Fitted Values', y='Residuals')

