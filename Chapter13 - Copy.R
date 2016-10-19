##Correlation & Regresssion
library ( openintro )
head(cars)

#plot of mpgCity vs weight
plot(mpgCity ~ weight, data = cars)
with(cars, cov(x = weight, y = mpgCity))

#correlation and p-value for the same (bivariate)
with(cars, cor (weight, mpgCity, method="pearson" ))
with(cars, cor.test(weight, mpgCity, alternative="two.sided",
                    method="pearson", conf.level = 0.95))


#linear regression model construction
model <- lm( mpgCity ~ weight, data = cars)
model

#a graphical look at the regression model:
with(cars, plot(mpgCity ~ weight))
# abline() automatically extracts the intercept and slope from
# a regression model
abline(model, col = "BLUE")


summary (model)


#The value of F for the test of the regression is the square
#of the value of t used to test the regression coefficient.
summary(model)$fstatistic["value"]^2

#contrariwise is a fun word
1-summary(model)$r.squared


##need to include plots for confidence lines
library(ggplot2)

p1 <- ggplot(cars, aes(weight, mpgCity)) +
  geom_point() +
  stat_smooth(method = lm) +
  ggtitle("Linear Regression of MPG on Weight") +
  theme_bw()
p1


p2 <- ggplot(model , aes(.fitted , .resid )) +
  geom_point() +
  stat_smooth( method ="loess") +
  geom_hline( yintercept = 0, col ="red", linetype = "dashed") +
  xlab(" Fitted values ") +
  ylab(" Residuals ") +
  ggtitle(" Residual vs Fitted Plot ") +
  theme_bw()
p2

p3 <- ggplot(model , aes(sample = .stdresid)) +
  stat_qq() +
  geom_abline(intercept = 0, slope = 1) +
  xlab(" Theoretical Quantiles ") +
  ylab (" Standardized Residuals ") +
  ggtitle(" Normal Q-Q ") +
  theme_bw()
p3

p4 <- ggplot(model , aes(.stdresid)) +
  geom_histogram(binwidth = .5) +
  xlab(" Standardized Residuals ") +
  ggtitle(" Histogram of Residuals ") +
  theme_bw()
p4


library(gridExtra)
grid.arrange(p1, p2, p3, p4,main = sprintf("Linear Regression Example, Model R2 = %0.1f%%",summary(model)$r.squared * 100),ncol = 2)


##Can we do something fun, like stock prices?

##download stock price from the internet
##The below lines will read in NEW data.  You may want to 
## use OUR saved file first, to follow along with the book.
## Otherwise, who knows if this method-path make sense on new data!
#sData=read.csv(file="http://www.google.com/finance/historical?output=csv&q=T",header=TRUE)
#write.csv(sData, "stock_ch13.csv", row.names=FALSE)
sData <- read.csv("stock_ch13.csv", header = TRUE)
head(sData)

#Attach and plot our basic linear model
plot(Close ~ Index, data = sData)
abline(lm(Close ~ Index, data = sData))

results <- lm(Close ~ Index, data = sData)
summary(results)


#create and numerically inspect our quadratic model
resultsQ <- lm(Close ~ Index + I(Index^2), data = sData)
summary(resultsQ)

#Plot the quadratic model
sData$predQuad <- predict(resultsQ)
plot(Close ~ Index, data = sData, main = "quadratic model")
abline(results)
lines(sData$predQuad, col="blue")

class(resultsQ)
plot(resultsQ)

# A Note on Time Series
#install.packages("forecast")
library(forecast)
# use the auto ARIMA function
# automatically determines best of several alternate models
m <- auto.arima(sData$Close)

# forecase the next 49 days
pred <- forecast(m, h = 49)

# create a plot of original data with fitted line and forecast
# values out 49 days with 95% interval
plot(Close ~ Index, data = sData, main = "ARIMA Model of Stock Prices",
     xlim = c(1, 300), ylim = c(30, 42))
lines(fitted(m), col = "blue")
lines(252:300, pred$mean, col = "blue", lty = 2, lwd = 2)
lines(252:300, pred$lower[, 2], lty = 2, lwd = 1)
lines(252:300, pred$upper[, 2], lty = 2, lwd = 1)



#Confidence & Prediction Intervals (linear)
summary(results)

conf <- predict(results, interval = "confidence")
pred <- predict(results, interval = "prediction")
# rename columns to avoid duplicates
colnames(conf) <- c("conffit", "conflwr", "confupr")
colnames(pred) <- c("predfit", "predlwr", "predupr")

intervals <- cbind(conf, pred)
head(intervals)
intervals <- as.data.frame(intervals)

plot(Close ~ Index, data = sData, ylim = c(32, 37))
with(intervals, {
  lines(predLin)
  lines(conflwr, col = "blue")
  lines(confupr, col = "blue")
  lines(predlwr, col = "red")
  lines(predupr, col = "red")
})


##Confidence & Prediction Intervals (quadratic)
conf <- predict(resultsQ, interval = "confidence")
pred <- predict(resultsQ, interval = "prediction")
# rename columns to avoid duplicates
colnames(conf) <- c("conffit", "conflwr", "confupr")
colnames(pred) <- c("predfit", "predlwr", "predupr")

intervalsQ <- cbind(conf, pred)
head(intervalsQ)
intervalsQ <- as.data.frame(intervalsQ)

plot(Close ~ Index, data = sData, ylim = c(32, 37),
     main = "quadratic model")
with(intervalsQ, {
  lines(predQuad)
  lines(conflwr, col = "blue")
  lines(confupr, col = "blue")
  lines(predlwr, col = "red")
  lines(predupr, col = "red")
})

