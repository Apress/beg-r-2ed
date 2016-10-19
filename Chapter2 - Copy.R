pdf(file = "chapter1_%03d.pdf", width = 7, height = 7, onefile = FALSE)

date ()
Sys.Date()
today <- Sys.Date()
cat ( format(today, format = "%A, %d %B %Y - "),"Hello, World!", "\n")


dir ()
fisherSays <- readLines ("fishersays.txt")
fisherSays

length (fisherSays)
nchar (fisherSays)
sub ("R. A. Fisher", "Jerzy Neyman", fisherSays )


##Mouse Things here
str(mtcars)
summary(mtcars $ mpg)
summary(mtcars $ wt)

head( mtcars $ drat)
head( mtcars [,5] )

which ( mtcars $ hp >= 300)
mtcars [31 ,]

mtcars $ hp [ mtcars $ hp >= 300] <- NA
mtcars [31 ,]


hist(mtcars$hp)


attach ( mtcars )
mpgMan <- subset ( mtcars , am == 1, select = mpg : disp )
summary ( mpgMan $ mpg)

mpgMan

mpgMan $ disp <- NULL
mpgMan


colors <- c(" black ", " white ", " gray ")
colors <- rep (colors, 5)
mpgMan $ colors <- colors[1:13]
mpgMan


#############################################
roster <- read.csv("roster.csv")
roster
sportsExample <- c("Jersey", "Class")
stackedData <- roster [ sportsExample ]
unstack(stackedData)

##GSS Example
library(foreign)
gss2012 <- read.spss("GSS2012merged_R5.sav", to.data.frame = TRUE)
View(gss2012)

myWords <- paste0 ("word", letters [1:10])
myWords
vocabTest <- gss2012 [ myWords ]
head ( vocabTest )

#text pre-motivates looping and compares and contrasts DRY vs WET:
myWords[1]
table(vocabTest[, "worda"], useNA = "ifany")
table(vocabTest[, myWords[1]], useNA = "ifany")

#If you've downloaded this, delete the pound sign and run this code - see how messy it is?!
#lapply(vocabTest, table)


#This is a function we've built.  It takes input, tables it, then finds the proportion of that table.
proportion.table <- function(x) {
  prop.table( table( x ) )
}

#here, percents stores all the table proportions from all of vocabTest's 10 words and 4820 observations.
percents <- lapply(vocabTest, proportion.table)

do.call(rbind, percents)


## histogram
hist(gss2012$wordsum)

detach(mtcars)
dev.off()