setwd("C:/Users/hp/Documents/r prog/excel data")
getwd()
likes<-read.xlsx("social media analytics.xlsx")
dim(likes)
#View(likes)
str(likes)
summary(likes)
mean(likes$Likes)
var(likes$Likes)

#fitting poisson regression
model<-glm(Likes~Platform+Content.Type+Impressions+Reach,
           family = poisson(link ="log"),data = likes)
summary(model)

coef(model)
exp(coef(model))
confint(model)

dev.new()
par(mfrow=c(2,2))
plot(model)

#plot between predicted and observed values
pred <- predict(model, type = "response")
dev.new()
par(mfrow=c(2,2))
plot(likes$Likes, pred,
     xlab = "Observed Counts",
     ylab = "Predicted Counts",
     main = "Observed vs Predicted Counts",
     pch = 19)
abline(0, 1, col = "red", lwd = 2)

# residuals vs fitted values plot
plot(fitted(model),
     residuals(model, type = "deviance"),
     xlab = "Fitted Values",
     ylab = "Deviance Residuals",
     main = "Residuals vs Fitted",
     pch = 19)

abline(h = 0, lty = 2)

#histogram plot of response variable

hist(likes$Likes,
     main = "Histogram of likes",
     xlab = "Number of likes",
     breaks = 10)