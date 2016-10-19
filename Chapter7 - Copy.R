data(HairEyeColor)
HairEyeColor
class(HairEyeColor)


#One-Way Tables (7.1)
hsb <- read.csv ("http://www.ats.ucla.edu/stat/data/hsb.csv")
# using the names function to see names of the variables and which column of
# data to which they correspond
names(hsb)
#You may want to save a local copy.
#Your fearless author wished he'd done so while attempting to write parts of this chapter in an aeroplane.
#save (hsb, file = "hsb.rda")
#dir ()
#load ("hsb.rda")


install.packages("Hmisc")
library(Hmisc)
hsb$mathGp <- as.numeric(cut2(hsb$math, g =5))
head(hsb)
table(hsb$math)
hsb$mathGp2 <- cut2(hsb$math, g =5)
head(hsb)
table(hsb$mathGp2)

#code to generate the histogram in the text
#hist(hsb$math)
#shapiro.test(hsb$math)

#Chi-Squared Test
table(hsb$ses)
chisq.test(table(hsb$ses))


cylinders <- c(7, 11, 14)
names(cylinders) <- c("four", "six", "eight")
cylinders
chisq.test(cylinders)

#Chi-Squared Blood
obs <- c(195, 165, 47, 15, 30, 35, 8, 5)
exp <- c(0.374, 0.357, 0.085, 0.034, 0.066, 0.063, 0.015, 0.006)
chisq.test(obs, p = exp)

#Two-way table Chi Squared
migraine <- matrix(c(19, 7, 101, 113), ncol = 2, byrow = TRUE)
colnames(migraine) <- c("female", "male")
rownames(migraine) <- c("migraine", "no migraine")
migraine <- as.table(migraine)
migraine
chisq.test(migraine)


table(hsb$female, hsb$ses)
femaleSES <- table(hsb$female, hsb$ses)
chisq.test(femaleSES)