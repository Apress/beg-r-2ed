pdf(file = "chapter3_%03d.pdf", width = 7, height = 7, onefile = FALSE)
getwd ()

setwd()

file.info("fishersays.txt")

file.exists("fishersays.txt")

#note: this may be nothing if you've started a fresh environment
ls()

dir()

yvector <- scan ("yvector.txt", sep = " ")
yvector

myName <- readline (" What shall I call you ? ")

roster <- read.csv("roster.csv")
fix(roster)


BMI <- function () {
  cat (" Please enter your height in inches and weight in pounds :","\n")
  height <- as.numeric ( readline (" height = "))
  weight <- as.numeric ( readline (" weight = "))
  bmi <- weight/(height^2)*703
  cat (" Your body mass index is:",bmi ,"\n")
  if ( bmi < 18.5) risk = " Underweight "
  else if ( bmi >= 18.5 & bmi <= 24.9) risk = "Normal"
  else if ( bmi >= 25 & bmi <= 29.9) risk = "Overweight"
  else risk = "Obese"
  cat (" According to the National Heart, Lung, and Blood Institute,","\n")
  cat (" your BMI is in the",risk ,"category.","\n")
}


cellPhones <- read.table ("cellphonetab.txt ", sep = "\t", header = TRUE )
str ( cellPhones )


hsb2.small <- read.csv ("http://www.ats.ucla.edu/stat/data/hsb2_small.csv")
# using the names function to see names of the variables and which column of
# data to which they correspond
names(hsb2.small)
(hsb3 <- hsb2.small [, c(1, 7, 8) ])

save (hsb3, file = "hsb3save.rda")
dir ()
load ("hsb3save.rda")
head (hsb3, 3)

write.csv( hsb3, file = "hsb3.csv")
file.exists ("hsb3.csv")



saveRDS(hsb3, "hsb3.RDS")
hsb3Copy <- readRDS("hsb3.RDS")
dev.off()