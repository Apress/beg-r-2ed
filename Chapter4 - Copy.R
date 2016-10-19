x <- 1:10
y <- 11:15
x < y | x < 10
x < y || x < 10
x < y & x < 10
x < 6 && x < 10


##loops!  The ubiquitious for loop to be precise
#start the clock (best to change 1:5 to 1:10000)
#ptm <- proc.time()
x <- 1:5
for (i in x) print (i * 10)
# Stop the clock
#proc.time() - ptm


#vecorization of the same.  Rule 2: Avoid loops when possible - they're slow :(
#Start the clock (best to change 1:5 to 1:10000)
#ptm <- proc.time()
x <- 1:5
x * 10
# Stop the clock
#proc.time() - ptm


shoppingBasket <- c("apples", "bananas", "butter", "bread", "milk", "cheese")
for(item in shoppingBasket) { 
  print(item)
}


#the while loop that does our count.  Notice this is slightly different from the book.  Experiment!
count <- 1
end <- 5
while (count <= end ) {
  print(count * 10)
  count <- count + 1
}


total <- 0
repeat {
  total <- total + 1
  print (total*10)
  if (total == 5)
    break
}



#Restore vocabTest file to what it should be
library(foreign)
gss2012 <- read.spss("GSS2012merged_R5.sav", to.data.frame = TRUE)
myWords <- paste0 ("word", letters [1:10])
vocabTest <- gss2012 [ myWords ]


vocab <- vocabTest[complete.cases(vocabTest) ,]

#Our download of GSS2012 has CORRECT & INCORRECT.  We convert those to 1 and 0 respectively here.
wordsToNum<-function(x) {
as.integer(x=="CORRECT")  
}

vocab<-apply(vocab,2,wordsToNum)
#apply gives back a matrix, we can turn this back into a data frame
vocab <- as.data.frame(vocab)
head(vocab, 3)



install.packages("rbenchmark")
library(rbenchmark)
benchmark(apply(vocab, 2, sum))
benchmark(colSums(vocab))


#sapply example
x1 <- c(55, 47, 51, 54, 53, 47, 45, 44, 46, 50)
x2 <- c(56, 57, 63, 69, 60, 57, 68, 57, 58, 56, 67, 56)
x3 <- c(68, 63, 60, 71, 76, 67, 67, 78, 69, 69, 66, 63, 78, 72)
tests <- list (x1 , x2 , x3)
tests
sapply (tests , mean )
lapply (tests , mean )


QuizScores <- read.table("QuizScores.txt", sep = " ", header = TRUE)
QuizScores

attach(QuizScores)
tapply(score, approach, mean)
tapply(score, group, mean)
tapply (score, list(approach, group), mean)

aggregate ( score ~ approach + group, data = QuizScores, mean)



rejectNull <- function(pValue, alpha = .05) {
  if (pValue <= alpha) print("Reject Null")
}
rejectNull(.05)
rejectNull (.051)



rejectNull <- function(pValue, alpha = .05) {
  if (pValue <= alpha) print("Reject Null")
  else print ("Do not Reject")
}
rejectNull (.07)
rejectNull(.05)
rejectNull(.05, .01)



x <- -5:5
x
sqrt (x)
sqrt(ifelse(x >= 0, x, NA))

