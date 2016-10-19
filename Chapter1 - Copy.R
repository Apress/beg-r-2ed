myName <- "Joshua Wiley"
myAlmaMater <- "University of California, Los Angeles"
myURL <- "www.JoshuaWiley.com"
myPhone <- "1.260.673.5518"
myData <- list(myName, myAlmaMater, myURL, myPhone)
myData
x <- 1:10
y <- 5
z <- rnorm(33,70,10)
z <- round(z,2)
x
x[1]
x[2:4]
x[-10]
x[2:4] <- c(98, 99, 100)
x
x<-1:10
names(x) <- c("A","B","C","D","E","F","G","H","I","J")
x
x<-1:10
names(x) <- toupper(letters[1:10])
x
w <- c (10 , NA , 10 , 25 , 30 , 15 , 10 , 18 , 16 , 15 )
mean(w)
mean(w, na.rm =TRUE)
x <- 1:10
x[11]<-"A"
x
str(x)
is.character(x)
x <- x[-11]
x <- as.integer(x)
is.integer(x)
x+y
round(z/x,2)


A <- c(1.00, 0.14, 0.35, 0.14, 1.00, 0.09, 0.35, 0.09, 1.00)
dim(A)<-c(3,3)
A <- matrix(A , nrow=3 , ncol=3)
A
Ainv <- solve(A)
MatProd <- A*Ainv
round(MatProd)


section1 <- c(57.3, 70.6, 73.9, 61.4, 63.0, 66.6, 74.8, 71.8, 63.2, 72.3, 61.9, 70.0)
section2 <- c(74.6, 74.5, 75.9, 77.4, 79.6, 70.2, 67.5, 75.5, 68.2, 81.0, 69.6, 75.6, 69.5, 72.4, 77.1)
section3 <- c(80.5, 79.2, 83.6, 74.9, 81.9, 80.3, 79.5, 77.3, 92.7, 76.4, 82.0, 68.9, 77.6, 74.6)
allSections <- list(section1,section2,section3)
allSections
section_means <- sapply(allSections, mean)
round(section_means, 2)
section_sdev <- sapply(allSections, sd)
round(section_sdev,2)
lapply(allSections,var)


roster <- read.csv("roster.csv")
roster
str(roster)
View(roster)