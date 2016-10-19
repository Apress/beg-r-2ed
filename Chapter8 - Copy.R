#choose one to load either a fresh copy or from saved work:
#hsb <- read.csv ("http://www.ats.ucla.edu/stat/data/hsb.csv")
load ("hsb.rda")

mean(hsb$read)
median(hsb$read)

hist(hsb$read)
abline(v = mean(hsb$read), lty = 2, lwd = 2)
abline(v = median(hsb$read), col = "blue", lwd = 2)

install.packages("prettyR")
library(prettyR)
mode(hsb$read)
Mode(hsb$read)
Mode(mtcars$hp)

sort(table(hsb$read), decreasing = TRUE)
sort(table(mtcars$hp), decreasing = TRUE)

#The Range 8.2.1

range(hsb$read)
max(hsb$read) - min(hsb$read)


#8.2.2   The Variance and Standard Deviation
var(hsb$read)
sd(hsb$read)
var(hsb$math)
sd(hsb$math)

#coefficient of variation (on mtcars$wt)
#Note: cv requires ratio data
cv <- function(x){
  cv <- (sd(x)/mean(x))
  return (cv)
}

cv(mtcars$wt)
cv(mtcars$qsec)
#detach("package:raster", unload=TRUE)


#8.3 Boxplots and Stem-and-Leaf Displays
summary(hsb$math)
boxplot(hsb$math)
#install.packages("lawstat")
#library(lawstat)
#symmetry.test(hsb$math)
#detach("package:lawstat", unload=TRUE)

data(mtcars)
stem(mtcars$mpg)


#fBasics for Summary Stats
install.packages ("fBasics")
library(fBasics)
basicStats(hsb$math)