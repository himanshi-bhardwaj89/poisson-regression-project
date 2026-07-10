
setwd( "C:/Users/hp/Documents/r prog/excel data")
getwd()
df<-read.xlsx("Ornstein’s Canadian Interlocking-Directorates Data.xlsx",sheet= 1)
#View(df)
freq <- table(df$interlocks)
dev.new() #to avoid the error of "figure margins too large"
plot(freq,
     type = "h",       # "h" creates spike lines
     lwd = 2,
     xlab = "interlocks",
     ylab = "Frequency",
     main = "Spike Plot of Frequencies")
# Add circles at the end of each spike
points(as.numeric(names(freq)),
       as.numeric(freq),
       pch = 16,      # filled circles
       cex = 1.2,
       col = "red")
# fiting of a poisson regression
model <- glm(interlocks ~ assets,
             family = poisson(link = "log"),
             data = df)
summary(model)
#diagonostic plots
dev.new()
par(mfrow = c(2,2))
plot(model)

#plot between observed and predicted values
pred <- predict(model, type = "response")
dev.new()
plot(df$interlocks, pred,
     xlab = "Observed Counts",
     ylab = "Predicted Counts",
     main = "Observed vs Predicted Counts",
     pch = 19)

abline(0, 1, col = "red", lwd = 2)

# residuals vs fitted values plot
dev.new()
plot(fitted(model),
     residuals(model, type = "deviance"),
     xlab = "Fitted Values",
     ylab = "Deviance Residuals",
     main = "Residuals vs Fitted",
     pch = 19)

abline(h = 0, lty = 2)

#histogram plot of response variable
dev.new()
hist(df$interlocks,
     main = "Histogram of Interlocks",
     xlab = "Number of Interlocks",
     breaks = 10)