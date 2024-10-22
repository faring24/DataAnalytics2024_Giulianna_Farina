# Data Analytics Fall 2024 Assignment 3 - Giulianna Farina

### set working directory
setwd("~/Documents/Data Analytics/Assignment 3")

### read in data
covid2021 <- read.csv("us-counties-2021.csv", header=TRUE)
covid2023 <- read.csv("us-counties-2023.csv", header=TRUE)
nyhouse <- read.csv("NY-House-Dataset.csv", header =TRUE)

### Subset data to Vermont state only so my computer can process the data
vt2021 <- subset(covid2021, state == "Vermont")
vt2023 <- subset(covid2023, state == "Vermont")

# Question 1
## 1A
### 2021 Year
ggplot(vt2021, aes(x = county, y = cases)) + 
  geom_boxplot() +
  labs(title = "Boxplot of Cases by County in Vermont in 2021", x = "County", y = "Cases") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(vt2021, aes(x = county, y = deaths)) + 
  geom_boxplot() +
  labs(title = "Boxplot of Deaths by County in Vermont in 2021", x = "County", y = "Deaths") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

summary(vt2021$cases)
summary(vt2021$deaths)

### 2023 Year
ggplot(vt2023, aes(x = county, y = cases)) + 
  geom_boxplot() +
  labs(title = "Boxplot of Cases by County in Vermont in 2023", x = "County", y = "Cases") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(vt2023, aes(x = county, y = deaths)) + 
  geom_boxplot() +
  labs(title = "Boxplot of Deaths by County in Vermont in 2023", x = "County", y = "Deaths") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

summary(vt2023$cases)
summary(vt2023$deaths)



## 1B
### 2021 Year
hist(vt2021$cases, breaks = 50, probability = TRUE, main = "Histogram of Cases with Normal Distribution in Vermont in 2021", 
     xlab = "Cases", col = "plum", border = "black")

cases21mean <- mean(vt2021$cases, na.rm = TRUE)
cases21sd <- sd(vt2021$cases, na.rm = TRUE)

curve(dnorm(x, mean = cases21mean, sd = cases21sd), 
      col = "seagreen1", lwd = 2, add = TRUE)


hist(vt2021$deaths, breaks = 50, probability = TRUE, main = "Histogram of Deaths with Normal Distribution in Vermont in 2021", 
     xlab = "Cases", col = "plum", border = "black")

deaths21mean <- mean(vt2021$deaths, na.rm = TRUE)
deaths21sd <- sd(vt2021$deaths, na.rm = TRUE)

curve(dnorm(x, mean = deaths21mean, sd = deaths21sd), 
      col = "seagreen1", lwd = 2, add = TRUE)


### 2023 Year
hist(vt2023$cases, breaks = 50, probability = TRUE, main = "Histogram of Cases with Normal Distribution in Vermont in 2023", 
     xlab = "Cases", col = "hotpink", border = "black")

cases23mean <- mean(vt2023$cases, na.rm = TRUE)
cases23sd <- sd(vt2023$cases, na.rm = TRUE)

curve(dnorm(x, mean = cases23mean, sd = cases23sd), 
      col = "turquoise", lwd = 2, add = TRUE)


hist(vt2023$deaths, breaks = 50, probability = TRUE, main = "Histogram of Deaths with Normal Distribution in Vermont in 2023", 
     xlab = "Cases", col = "hotpink", border = "black")

deaths23mean <- mean(vt2023$deaths, na.rm = TRUE)
deaths23sd <- sd(vt2023$deaths, na.rm = TRUE)

curve(dnorm(x, mean = deaths23mean, sd = deaths23sd), 
      col = "turquoise", lwd = 2, add = TRUE)



## 1C
### 2021 Year
plot(ecdf(vt2021$cases), main = "ECDF of Cases in Vermont in 2021", 
     xlab = "Cases", ylab = "ECDF", 
     col = "dodgerblue", lwd = 2)

qqnorm(vt2021$cases, main = "Q-Q Plot of Cases vs Normal Distribution in Vermont in 2021")
qqline(vt2021$cases, col = "dodgerblue", lwd = 2)

plot(ecdf(vt2021$deaths), main = "ECDF of Deaths in Vermont in 2021", 
     xlab = "Deaths", ylab = "ECDF", 
     col = "dodgerblue", lwd = 2)

qqnorm(vt2021$cases, main = "Q-Q Plot of Deaths vs Normal Distribution in Vermont in 2021")
qqline(vt2021$cases, col = "dodgerblue", lwd = 2)


### 2023 Year
plot(ecdf(vt2023$cases), main = "ECDF of Cases in Vermont in 2023", 
     xlab = "Cases", ylab = "ECDF", 
     col = "blueviolet", lwd = 2)

qqnorm(vt2023$cases, main = "Q-Q Plot of Cases vs Normal Distribution in Vermont in 2023")
qqline(vt2023$cases, col = "blueviolet", lwd = 2)

plot(ecdf(vt2023$deaths), main = "ECDF of Deaths in Vermont in 2023", 
     xlab = "Deaths", ylab = "ECDF", 
     col = "blueviolet", lwd = 2)

qqnorm(vt2023$cases, main = "Q-Q Plot of Deaths vs Normal Distribution in Vermont in 2023")
qqline(vt2023$cases, col = "blueviolet", lwd = 2)


## 2
moco2021 <- subset(covid2021, county == "Monmouth")

### 2A
ggplot(moco2021, aes(x = county, y = cases)) + 
  geom_boxplot() +
  labs(title = "Boxplot of Cases by County in Monmouth County, NJ in 2021", x = "County", y = "Cases") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(moco2021, aes(x = county, y = deaths)) + 
  geom_boxplot() +
  labs(title = "Boxplot of Deaths by County in Monmouth County, NJ in 2021", x = "County", y = "Deaths") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

summary(moco2021$cases)
summary(moco2021$deaths)

### 2B
hist(moco2021$cases, breaks = 50, probability = TRUE, main = "Histogram of Cases with Normal Distribution in Monmouth County, NJ in 2021", 
     xlab = "Cases", col = "lightgreen", border = "black")

cases21mean_moco <- mean(moco2021$cases, na.rm = TRUE)
cases21sd_moco <- sd(moco2021$cases, na.rm = TRUE)

curve(dnorm(x, mean = cases21mean_moco, sd = cases21sd_moco), 
      col = "magenta", lwd = 2, add = TRUE)


hist(moco2021$deaths, breaks = 50, probability = TRUE, main = "Histogram of Deaths with Normal Distribution in Monmouth County, NJ in 2021", 
     xlab = "Cases", col = "lightgreen", border = "black")

deaths21mean_moco <- mean(moco2021$deaths, na.rm = TRUE)
deaths21sd_moco <- sd(moco2021$deaths, na.rm = TRUE)

curve(dnorm(x, mean = deaths21mean_moco, sd = deaths21sd_moco), 
      col = "magenta", lwd = 2, add = TRUE)


### 2C
plot(ecdf(moco2021$cases), main = "ECDF of Cases in Monmouth County, NJ in 2021", 
     xlab = "Cases", ylab = "ECDF", 
     col = "royalblue", lwd = 2)

qqnorm(moco2021$cases, main = "Q-Q Plot of Cases vs Normal Distribution in Monmouth County, NJ in 2021")
qqline(moco2021$cases, col = "royalblue", lwd = 2)

plot(ecdf(moco2021$deaths), main = "ECDF of Deaths in Monmouth County, NJ in 2021", 
     xlab = "Deaths", ylab = "ECDF", 
     col = "royalblue", lwd = 2)

qqnorm(moco2021$cases, main = "Q-Q Plot of Deaths vs Normal Distribution in Monmouth County, NJ in 2021")
qqline(moco2021$cases, col = "royalblue", lwd = 2)


## 3
### 3A
nyhouse_model <- lm(PRICE ~ BEDS + BATH + PROPERTYSQFT, data = nyhouse)
summary(nyhouse_model)

ggplot(nyhouse_model, aes(x = PROPERTYSQFT, y = PRICE)) + geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", col = "orchid") + xlim(0, 9000) + ylim(0, 8000000) +
  ggtitle("House Price vs Square Footage of Property in NY") + xlab("Square Footage of Property") + ylab("Price")


nyhouse$residuals <- nyhouse_model$residuals
ggplot(nyhouse, aes(x = PROPERTYSQFT, y = residuals)) + geom_point(alpha = 0.5) +
  ggtitle("Residuals of the LM") + xlab("Square Footage of Property") + ylab("Residuals") +
  xlim(0, 9000) + ylim(-8000000, 8000000)


### 3B
nyhouse_subset <- subset(nyhouse, PRICE > 1000000 & BATH >= 2)

nyhouse_subset_model <- lm(PRICE ~ BATH, data = nyhouse_subset)
summary(nyhouse_subset_model)

ggplot(nyhouse_subset_model, aes(x = BATH, y = PRICE)) + geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", col = "orchid") + xlim(2, 20) + ylim(1000000, 8000000) +
  ggtitle("Houses over $1,000,000 that has at least 2 bathrooms") + xlab("Number of Bathrooms") + ylab("Price")


nyhouse_subset$residuals <- nyhouse_subset_model$residuals
ggplot(nyhouse_subset, aes(x = PROPERTYSQFT, y = residuals)) + geom_point(alpha = 0.5) +
  ggtitle("Residuals of the Subset LM") + xlab("Square Footage of Property") + ylab("Residuals") +
  xlim(0, 7500) + ylim(-6000000, 6000000)
