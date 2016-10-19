mad

sapply (mtcars [, 3:5], function (x) sd(x)/ mean (x))

formals(function(x) sd(x)/mean(x))
body(function(x) sd(x)/mean(x))
environment (function(x) sd(x)/mean(x))

take.root <- function(n) {
  root <- function(x) {
    x ^(1/n)
  }
  root
}

square.root <- take.root(2)
cube.root <- take.root(3)
square.root(81)

cube.root(27)

ls(environment(square.root))

get ("n", environment(square.root))

ls(environment(cube.root))

get("n", environment(cube.root))

x <- 10
.y <- 20
ls ()
ls(all.names = TRUE)



myFun <- function (x,y) {
  print (x ^2)
}
myFun(,10)
myFun (10)
myFun (10 ,20)
myFun(20,x=10)


##guessing game function
guessIt <- function(){
  cat ("I am thinking of a number between 1 and 100","\n")
  computerPicks <- as.integer(round(runif(1,1,100),0))
  attempts <- as.integer(readline("How many guesses do you want? "))
  count = 0
  while (count < attempts){
    count <- count + 1
    userGuess <- as.integer(readline("Enter your guess: "))
    if (count == attempts && userGuess != computerPicks) {
      cat("Sorry, out of tries. My number was ",computerPicks,"\n")
      break
    }
    if (userGuess == computerPicks) {
      cat("You got it in ", count, "tries.","\n")
      break
    }
    if (userGuess < computerPicks ) {
      cat("Your guess is too low.","\n")
    }
    if (userGuess > computerPicks){
      cat ("Your guess is too high.","\n")
    }
  }
}



# function for finding the real root(s) of a quadratic equation
quadratic <- function (a, b, c) {
  discrim <- b^2 - 4*a*c
  cat("The discriminant is: ", discrim, "\n")
  if(discrim < 0){
    cat("There are no real roots. ","\n")}else {
    root1 <- (-b+ sqrt ( discrim )) / (2*a)
    root2 <- (-b- sqrt ( discrim )) / (2*a)
    cat("root1: ",  root1,   "\n")
    cat("root2: ",  root2,   "\n")
    }
}

quadratic (2, -1, -8)
quadratic (1, -2, 1)
quadratic (3, 2, 1)



#S3 Class
info <- list(name = "Jon", gender = "male", donation = 100)
class(info) <- "member"
attributes(info)

print ( info )


print.member <- function(person) {
  cat("Name: ", person $name , "\n")
  cat("Gender: ", person $ gender, "\n")
  cat("Donation: ", person $ donation, "\n")
}

print ( info )



## t-test
results <- t.test(mpg ~ am, data = mtcars)

results

#this will generate an error:
plot( results )


class ( results )
methods( plot )

## t-test plot method
plot.htest <- function(object, digits.to.round = 4) {

  rounded.pvalue <- round(object$p.value, digits.to.round)

  barplot(object$estimate,
          ylim = c(0, max(object$estimate) * 1.1),
          main = paste(object$method, "of", object$data.name),
          sub = paste("p =", rounded.pvalue))
}

plot( results )
