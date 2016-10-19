#choose one to load either a fresh copy or from saved work:
hsb <- read.csv ("http://www.ats.ucla.edu/stat/data/hsb.csv")
#load ("hsb.rda")

#Our Confidence Interval function:
CI <- function (x, alpha = .05) {
  sampMean <- mean (x)
  stderr <- sd(x)/ sqrt( length (x))
  tcrit <- qt (1- alpha /2, length (x) - 1)
  margin <- stderr * tcrit
  CI <- c( sampMean - margin , sampMean + margin )
  return (CI)
}
CI( hsb $ science )

#R's built in t.test with included CI function
t.test(hsb$ science)

#10.1.2 CI for Proportions contrasted with the prop.test
zalphahalves <- qnorm(.975)
k <- 135
n <- 200
pbar <- k/n
SEprop <- sqrt(pbar∗(1−pbar)/n)
E <- zalphahalves*SEprop
pbar + c(-E,E)
prop.test(135,200, conf.level=0.95)

#variance example with openintro cars data
library ( openintro )

varInterval <- function (data , conf.level = 0.95) {
  df <- length ( data ) - 1
  chi_left <- qchisq ((1 - conf.level )/2, df)
  chi_right <- qchisq ((1 - conf.level )/2, df , lower.tail = FALSE )
  v <- var ( data )
  c(( df * v)/chi_right, (df * v)/ chi_left )
  }
  
var ( cars $ mpgCity )
varInterval ( cars $ mpgCity )


#10.2 Hypothesis Test
t.test ( cars $ mpgCity , mu = 25)

#paired t tests
library(MASS)
pairedPoliceExpenditure <- UScrime [( UScrime $ So == 1) ,]

t.test ( pairedPoliceExpenditure $ Po1 , pairedPoliceExpenditure $ Po2 , paired = TRUE )

#extra work required to correctly use the t-test with only a single input vector
#this more closely matches the way most traditional stats texts would implement the process.
PolExpDiffs <- pairedPoliceExpenditure $ Po1 - pairedPoliceExpenditure $Po2
t.test ( PolExpDiffs , mu = 0)

#10.3 - Two Samples!
prop.test (x = c(136 , 108) , n = c(200 , 200) )
prop.test (x = c(136 , 108) , n = c(200 , 200) , correct = FALSE )

#Z Proportion Test by hand
zproptest <- function (x1 , x2 , n1 , n2 , conf.level = 0.95) {
  ppooled <- (x1 + x2)/(n1 + n2)
  qpooled <- 1 - ppooled
  p1 <- x1/n1
  p2 <- x2/n2
  zstat <- round (( p1 - p2)/ sqrt (( ppooled * qpooled )/n1 + ( ppooled * qpooled )/n2) ,4)
  pval <- round (2 * pnorm (zstat , lower.tail = FALSE ) ,4)
  print ("two - sample z test for proportions ", quote = FALSE )
  print (c(" valueof z: ",zstat ), quote = FALSE )
  print (c("p- value : ", pval ), quote = FALSE )
}
zproptest (136 , 108 , 200 , 200)


southern <- UScrime [(UScrime $ So ==1) ,]
notSouthern <- UScrime [(UScrime $ So ==0) ,]
var.test(southern $ Po1 , notSouthern $ Po1)

t.test(southern $ Po1 , notSouthern $ Po1, var.equal = TRUE)
t.test(southern $ Po1 , notSouthern $ Po1)

#exploratory data (normally this would be done first actually)
boxplot(southern $ Po1)
boxplot(notSouthern $ Po1)
summary(southern $ Po1)
summary(notSouthern $ Po1)