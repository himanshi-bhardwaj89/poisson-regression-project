# fitting of poisson regression
install.packages("MASS")      # Run only once
library(MASS)

# Load ships dataset
data(ships)

# Display dataset
View(ships)

# Structure of data
str(ships)

# Dimensions
dim(ships)

# Column names
names(ships)

# Summary statistics
summary(ships)

colSums(is.na(ships))  #to check missing values in data 
sapply(ships,class) #to understand the variables

#descriptive statistics
summary(ships$incidents)

mean(ships$incidents)

var(ships$incidents)

sd(ships$incidents)

min(ships$incidents)

max(ships$incidents)

table(ships$incidents) #frequency distribution

#histogram plot
dev.new()
hist(ships$incidents,
     main="Histogram of Ship Incidents",
     xlab="Incidents",
     col="lightblue",
     border="black")

#poisson regression fit
fit <- glm(incidents ~ type + year + period + service,
           family=poisson(link="log"),
           data=ships)

summary(fit)
coef(fit)  #coefficients of model
confint(fit) #confidence intervals
exp(coef(fit)) #Exponentiated Coefficients (Incidence Rate Ratios)

#goodness of fit
deviance(fit)

df.residual(fit)

deviance(fit)/df.residual(fit)

#residual plot
dev.new()
par(mfrow=c(2,2))
plot(fit)

#observed vs predicted plot
dev.new()
plot(ships$incidents,
     ships$Predicted,
     xlab="Observed",
     ylab="Predicted",
     main="Observed vs Predicted")

abline(0,1,col="red",lwd=2)

#interpretation template
summary(fit)