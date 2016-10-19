## High Performance Computing

## First, we will discuss working with larger datasets.  The R package 'nycflights13' has some datasets with a few hundred thousand observations.
install.packages("nycflights13")
install.packages("parallel")
install.packages("iterators")
install.packages("foreach")
install.packages("doSNOW")


library(nycflights13)
library(iterators)
library(foreach)
library(doSNOW)
library(ggplot2)

head(flights)

## example of how ave() works
ave(1:9, c(1, 1, 1, 2, 2, 2, 3, 3, 3), FUN = mean)

## timing using base R
system.time(
flights <- within(flights, {
  ArrDelaySD <- ave(arr_delay, dest, FUN = function(x) sd(x, na.rm = TRUE))
})
)

## timing using base R
system.time(
  mean(subset(flights, month < 7)$arr_delay)
)


install.packages("devtools")
library(devtools)
install_github("Rdatatable/data.table")


## using the data.table package
library(data.table)
flights2 <- as.data.table(flights)

## timing using the data.table package
system.time(
flights2[, ArrDelaySD := sd(arr_delay, na.rm = TRUE), by = dest]
)
all.equal(flights2$ArrDelaySD, flights$ArrDelaySD)

system.time(
mean(flights2[month < 7]$arr_delay)
)


## smart printing
flights2

## subsetting data / selecting rows in data.table
flights2[carrier == "DL"]

## subsetting data / selecting rows in base R
head(flights[flights$carrier == "DL", ])
head(subset(flights, carrier == "DL"))

## data.table select rows and one variable
table(flights2[carrier == "DL", dest])

## create a new variable and see it is added
flights2[, NewVariable := dep_delay - arr_delay]
colnames(flights2)

## recode a variable and count number of cases by level of a variable
flights2[, NewVariable := ifelse(arr_delay > 120, "Delayed", "No Delay")]
flights2[, .N, by = NewVariable]

flights2[order(NewVariable, arr_delay), arr_delay[c(1:2, .N - 1:0)], by = NewVariable]


## To remove a variable, just set it to NULL
flights2[, NewVariable := NULL]
colnames(flights2)

## create new variable for specific cases only (missing otherwise)
flights2[carrier == "DL", NewVariable := "Test"]
table(is.na(flights2[carrier == "DL", NewVariable]))
table(is.na(flights2[carrier != "DL", NewVariable]))
flights2[, NewVariable := NULL]

## calculate a summary by a variable
flights2[, mean(arr_delay, na.rm=TRUE), by = month]

## calculate multiple summarise by a variable
flights2[, .(M = mean(arr_delay, na.rm=TRUE),
             SD = sd(arr_delay, na.rm=TRUE)),
         by = month]

## calculate multiple sumaries by more than one variable
flights2[, .(M = mean(arr_delay, na.rm=TRUE),
             SD = sd(arr_delay, na.rm=TRUE)),
         by = .(month, dest)]


## calculate sumaries by a variable with operations
flights2[, .(M = mean(arr_delay, na.rm=TRUE),
             SD = sd(arr_delay, na.rm=TRUE)),
         by = .(Winter = month %in% c(9:12, 1:3))]

## create more than one new variable simultaneously
flights2[, c("MonthDelayM", "MonthDelaySD") := list(
  mean(arr_delay, na.rm=TRUE), sd(arr_delay, na.rm=TRUE)), by = month]

## select multiple variables to view results
flights2[, .(month, MonthDelayM, MonthDelaySD)]

## set a key to sort and join by
setkey(flights2, month)

## select by key rather than by variable (timing in data.table and base R)
system.time(flights2[J(3:7)])
system.time(subset(flights, month %in% 3:7))

## data.table objects can be used with functions expecting data.frames
summary(lm(arr_delay ~ dep_time, data = flights2))


## data.tables can have calculations done by variables in addition to subsetting
flights2[arr_delay > 60*12, .N, by = month]

## example of how to merge
airlines2 <- as.data.table(airlines)
setkey(airlines2, carrier)
setkey(flights2, carrier)

## join the data.tables by their key
flights3 <- flights2[airlines2]
## view just three variables
flights3[, .(year, carrier, name)]

## join by multiple keys
weather2 <- as.data.table(weather)

setkey(flights2, month, day, origin)
setkey(weather2, month, day, origin)

## collapse weather across hours
weather2b <- weather2[, .(temp = mean(temp, na.rm=TRUE),
                         precip = mean(precip, na.rm=TRUE),
                         visib = mean(visib, na.rm=TRUE)),
                         by = .(month, day, origin)]
weather2b


## use .SD to get mean of all columns
weather2c <- weather2[, lapply(.SD, mean, na.rm=TRUE),
                         by = .(month, day, origin)]
weather2c


flights4 <- weather2c[flights2]

dim(flights2)
dim(flights4)


## operations within a data.table
## intercept and coefficient for relationship between arival delay and visibility by carrier
flights4[, as.list(coef(lm(arr_delay ~ visib))), by = carrier]

par(mfrow = c(4, 3))
flights4[, plot(density(na.omit(arr_delay)), main = "Arival Delay", xlab = "", ylab = "Density"),
         by = month]

########################################
## Parallel Processing

## load parallel package and make a cluster (assumes 4 cores available)
library(parallel)
cl <- makeCluster(4)

## timine for sequential and parallel analysis for trivial computation
system.time(lapply(1:1000, function(i) i + 1))
system.time(parLapply(cl, 1:1000, function(i) i + 1))

## timine for sequential and parallel analysis for more demanding computation

time1 <- system.time(lapply(1:1000, function(i) mean(rnorm(4e4))))
time2 <- system.time(parLapply(cl, 1:1000, function(i) mean(rnorm(4e4))))

## how much slower sequential is compared to parallel
time1["elapsed"] / time2["elapsed"]


## parallel processing bootstrap example

library(boot)
library(VGAM)
library(foreign)

gss2012 <- read.spss("GSS2012merged_R5.sav", to.data.frame = TRUE)
gssr <- gss2012[, c("age", "sex", "marital", "educ", "income06", "satfin", "happy", "health")]
gssr <- na.omit(gssr)
gssr <- within(gssr, {
  age <- as.numeric(age)
  Agec <- (gssr$age - 18) / 10
  educ <- as.numeric(educ)
  # recode income categories to numeric
  cincome <- as.numeric(income06)
  satfin <- factor(satfin,
                   levels = c("NOT AT ALL SAT", "MORE OR LESS", "SATISFIED"),
                   ordered = TRUE)
})

m <- vglm(satfin ~ Agec + cincome * educ,
          family = cumulative(link = "logit", parallel = TRUE, reverse = TRUE),
          data = gssr)

## write function to pass to boot()
model_coef_predictions <- function(d, i) {

  m.tmp <- vglm(satfin ~ Agec + cincome * educ,
                family = cumulative(link = "logit", parallel = TRUE, reverse = TRUE),
                data = d[i, ])
  newdat <- expand.grid(
    Agec = seq(from = 0, to = (89 - 18)/10, length.out = 50),
    cincome = mean(d$cincom),
    educ = c(12, 16, 20))
  bs <- coef(m.tmp)
  predicted.probs <- predict(m.tmp, newdata = newdat,
                             type = "response")
  out <- c(bs, predicted.probs[, 1], predicted.probs[, 2], predicted.probs[, 3])
  return(out)
}

## need to load packages on the cluster in order to use them

clusterEvalQ(cl, {
  library(VGAM)
})

## add extra arguments to boot() function for parallel
clusterSetRNGStream(cl, iseed = 1234)
boot.res <- boot(
  data = gssr,
  statistic = model_coef_predictions,
  R = 5000,
  parallel = "snow",
  ncpus = 4,
  cl = cl)


## load boot package and export data to cluster to calculate 95% bootstrapped CIs
clusterEvalQ(cl, {
  library(boot)
})
clusterExport(cl, varlist = "boot.res")


boot.res2 <- parLapply(cl, 1:6, function(i) {
  cis <- boot.ci(boot.res, index = i, type = "bca")
  data.frame(Estimate = boot.res$t0[i],
             LL = cis$bca[1, 4],
             UL = cis$bca[1, 5])
})
##combine row-wise
boot.res2 <- do.call(rbind, boot.res2)
round(boot.res2, 3)


## using the foreach package examples
## need to register a parallel backend --- we use our cluster

registerDoSNOW(cl)

## sequential versus parallel timing
system.time(
  res1 <- foreach(i = 1:1000, .combine = 'c') %do% mean(rnorm(4e4))
)

system.time(
  res2 <- foreach(i = 1:1000, .combine = 'c') %dopar% mean(rnorm(4e4))
)

## histograms show similar results
par(mfrow = c(1, 2))
hist(res1)
hist(res2)

## also special iterators
foreach(x=iter(mtcars, by='col'), .combine = rbind) %dopar% (mean(x) / var(x))

## more advanced example

prettyout <- function(object) {
  cis <- confint(object)
  bs <- coef(object)
  out <- sprintf("%0.2f [%0.2f, %0.2f]", bs, cis[, 1], cis[, 2])
  names(out) <- names(bs)
  return(out)
}

data("diamonds")
continuous.vars <- sapply(diamonds, is.numeric)

results <- foreach(dv=iter(diamonds[, continuous.vars], by='col'), .combine = cbind) %dopar% {
  prettyout(lm(dv ~ cut + color + clarity, data = diamonds))
}

print(results[, 1:2], quote = FALSE)


## cross validated R squared
drop.index <- tapply(1:nrow(gssr),
  rep(1:10, each = ceiling(nrow(gssr)/10))[1:nrow(gssr)],
  function(x) x)

CV <- foreach(i = drop.index, .combine = 'c', .final = mean) %dopar% {
  m <- lm(cincome ~ educ, data = gssr[-i, ])
  cor(gssr[i, "cincome"], predict(m, newdata = gssr[i, ]))^2
}

summary(lm(cincome ~ educ, data = gssr))
CV

## nested parallel loops

CV2 <-
  foreach(x = iter(gssr[, 1:4], by = "col"), .combine = "cbind") %:%
    foreach(i = drop.index, .combine = 'rbind') %dopar% {
      x2 <- x[-i]
      m <- lm(gssr$cincome[-i] ~ x2)
      cor(gssr$cincome[i], predict(m, newdata = data.frame(x2 = x[i])))^2
    }

colnames(CV2) <- colnames(gssr)[1:4]
round(CV2, 2)

## when statements

results <- foreach(dv=iter(diamonds, by='col'), .combine = cbind) %:%
  when(is.numeric(dv)) %dopar%
  prettyout(lm(dv ~ cut + color + clarity, data = diamonds))

print(results[, 1:2], quote = FALSE)

## when done, need to shut down the cluster.
stopCluster(cl)
