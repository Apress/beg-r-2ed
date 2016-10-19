#choose function
#Note: N choose k means that out of N total objects, we choose k items.
#This is sometimes called _unordered_ because if an item is selected, we do not distinguish _when_ it was selected.

choose (10, 6)

x <- 0:10
x
binomCoinToss <- cbind(dbinom(x,  10, .50))
rownames(binomCoinToss) <- x
binomCoinToss
class(binomCoinToss)


#Note: we keep the x variable we already defined as our x-axis measure
binomDist <- dbinom (x, 10, 0.50)
plot (x,binomDist, type = "h")
points (x, binomDist)
abline (h = 0)
lines (x, binomDist)

sum (dbinom(0:5, 10, .50))
# calculate the probability k <= 5
pbinom(5, 10, .50)
# calculate the probability k > 5
pbinom(5, 10, .50, lower.tail = FALSE)


tosses <- rbinom(1000, 1, .5)
heads <- cumsum(tosses)/1:1000
plot(heads, type = "l", main = "Proportion of Heads")
abline (h = .5)


dpois(10, 12.7)
ppois(10, 12.7)
ppois(10, 12.7, lower.tail = FALSE)

#Empirical  Rule
pnorm(3) - pnorm(-3)
pnorm(2) - pnorm(-2)
pnorm(1) - pnorm(-1)


qnorm(1 - .05/2)
qnorm(1 - .01/2)
qnorm(1 - .10/2)

pnorm(1.96) - pnorm(-1.96)


#faithful dataset
faithful
summary(faithful)

#number of simulations
attach(faithful)

#5 items selected at a time to mean
sampsize5 <- 5
means5 <- replicate(999, mean(sample(eruptions, sampsize5, replace = TRUE)))


#20 items selected at a time to mean
sampsize20 <- 20
means20 <- replicate(999, mean(sample(eruptions, sampsize20, replace = TRUE)))
par (mfrow=c(1,2))

#histograms of the above two mean selections to compare
hist (means5, breaks = 15, xlim = c(1.8, 4.7))
hist (means20, breaks = 15, xlim = c(1.8, 4.7))

detach(faithful)


#Student t distribution (which has a fascinating history of development)
qt(0.975, 18)


#Note thate 18 degrees of freedom indicates there are n = 18+1 sample items
#A more detailed discussion of degrees of freedom is either prerequisite knowledge or beyond the scope of this text.
qt (0.95, 18)
pt (1.734064, 18) - pt(-Inf, 18)


xaxis <- seq(-4, 4, .05)
y <- dnorm(xaxis)
y1 <- dt(xaxis, 1)
y4 <- dt(xaxis, 4)
y9 <- dt(xaxis, 9)
plot(xaxis, y, type = "l")
lines(xaxis, y1, col = "purple")
lines(xaxis, y4, col = "red")
lines(xaxis, y9, col = "blue")


#The F Distrubtion
xaxis <- seq(0, 8, .05) 
y1 <- df(xaxis, 3, 5)
y2 <- df(xaxis, 6, 10)
y3 <- df(xaxis, 9, 20)
y4 <- df(xaxis, 49, 49)
plot(xaxis, y1, type = "l", xlab = "Value of F", main = "PDF of F Distribution", ylim = c(0, 1.5), col = "green")
lines (xaxis, y2, col = "red")
lines (xaxis, y3, col = "blue")
lines (xaxis, y4, col = "purple")


#Chi-Squared Distribution 
xaxis <- seq(0, 20, .05)
y1 <- dchisq(xaxis, 4)
y2 <- dchisq(xaxis, 6)
y3 <- dchisq(xaxis, 10)
plot(xaxis, y1, type = "l", xlab = "Chi - square Value")
lines(xaxis, y2, col = "blue")
lines(xaxis, y3, col = "red")
xcoords <- c(3.4, 5.75, 10.6)
ycoords <- c(0.17, 0.13, 0.09)
labels <- c("df = 4", "df = 6", "df = 10")
text(xcoords, ycoords, labels, adj = c(0,0))

