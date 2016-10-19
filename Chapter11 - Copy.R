#Chapter 11 

#Data
group1 <- c(151, 78, 169, 88, 194, 196, 109, 143, 150, 85, 168)
group2 <- c(128, 122, 95, 97, 81)

#Graphs of the Data
par (mfrow=c(1,2))
plot( density(group1))
plot( density(group2))


#Summary of the data
summary(group1)
var(group1)

summary(group2)
var ( group2 )

#comparison of the variances
var.test ( group2 , group1 )


t.test(group2, group1)
t.test(group2, group1, var.equal = TRUE)

wilcox.test(group1, group2)


# Install the Wilcox package (and dependencies) from github 
#https://github.com/nicebread/WRS
# first: install dependent packages
install.packages(c("MASS", "akima", "robustbase"))

# second: install suggested packages
install.packages(c("cobs", "robust", "mgcv", "scatterplot3d", "quantreg", "rrcov", "lars", "pwr", "trimcluster", "mc2d", "psych", "Rfit"))

# third: install an additional package which provides some C functions
install.packages("devtools")
library("devtools")
install_github("mrxiaohe/WRScpp")

# fourth: install WRS
install_github("nicebread/WRS", subdir="pkg")

#use the above installed WRS package to run the Yuan test
library ( WRS )
yuenTest <- yuen ( group2 , group1)
yuenTest $p.value
yuenTest $ci


#R's built in Wilcox test

wilcox.test ( group2, group1 )



#Bootstrapping
myData <- rnorm (1000, 500, 100)
resamples <- lapply (1:1000 , function (i) sample ( myData , replace = TRUE ))
r.mean <- sapply ( resamples , mean )
ci.mean <- c( quantile (r.mean , 0.025) , quantile (r.mean , 0.975) )
ci.mean


hist (r.mean )
abline (v = quantile (r.mean , 0.025) )
abline (v = quantile (r.mean , 0.975) )

t.test ( myData )


boot.fun <- function (data , num) {
  resamples <- lapply (1: num , function (i) sample (data, replace=TRUE))
  r.median <- sapply ( resamples, median )
  r.mean <- sapply ( resamples, mean )
  std.err.median <- sqrt ( var (r.median ))
  std.err.mean <- sqrt (var (r.mean ))
  rawDataName <- 
  data.frame (std.err.median = std.err.median , std.err.mean = std.err.mean , resamples =  resamples , medians =r.median , means =r.mean )
}
boot1 <- boot.fun ( myData , 1000)



#install the gridExtra package
install.packages("gridExtra")
library ( gridExtra )
library (ggplot2)
plot1 <- ggplot (boot1, aes (means)) + geom_histogram (binwidth = 1, fill="white", color="black")
plot2 <- ggplot (boot1, aes (medians)) + geom_histogram (binwidth=1, fill ="white", color="black")
grid.arrange (plot1, plot2, nrow = 1)

#11.4 Permutation Tests
memory <- read.table("memory_ch11.txt", sep="\t", header = TRUE)
head( memory )
tail( memory )
t.test ( recall ~ cond , data = memory )
t.test ( recall ~ cond , data = memory, var.equal = TRUE)

install.packages("perm")
library(perm)
permTS ( recall ~ cond , data = memory )
permTS ( recall ~ cond , data = memory , exact = TRUE )

library( WRS)
recall1 <- memory[ memory[,"cond"]=="drug", "recall"]
recall2 <- memory[ memory[,"cond"]=="placebo", "recall"]

#something to note:
var(recall1)
var(recall2)
var.test(recall1, recall2)

yuenTest <- yuen ( recall1, recall2 )
yuenTest $p.value
yuenTest $ci