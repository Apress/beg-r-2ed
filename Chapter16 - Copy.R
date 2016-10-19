## Nonparametric
##install new packages
install.packages("coin")
library(ggplot2)
library(GGally)
library(grid)
library(reshape2)
library(coin)
library(scales)


##To get started, let’s build a non-normal data set.
## Wilcoxon Signed Rank Test
set.seed(4)
untreated <- runif(10, 20, 75)
treated <- runif(10,20,75)
differences = treated - untreated
xydat <- data.frame(treated, untreated)
shapiro.test(differences)
hist(differences)
wilcoxsign_test(treated ~ untreated,data = xydat)

##Spearman's Rho
wolfpackH <- c(1:10)
wolfkills <- c(23, 20, 19, 19, 19, 15, 13, 8, 2, 2)
spearman_test(wolfkills~wolfpackH)
plot(wolfpackH, wolfkills, type="p")


##Kruskal-Wallis
hist(mtcars$disp)

## ANOVA comparision
summary(aov(disp ~ factor(carb), data = mtcars))
stuff<-aov(disp ~ factor(carb), data = mtcars)
plot(stuff)

##Kruskal-Wallis test (Hollander & Wolfe, 1973).
kruskal.test(disp ~ factor(carb), data = mtcars)

##kruskal_test() in the coin package
kruskal_test(disp ~ factor(carb), data = mtcars)

## two or k independent sample data, and uses a permutation test
oneway_test(disp ~ factor(carb), data = mtcars)

##plotting the means and medians
p <- ggplot(mtcars, aes(carb, disp)) +
  stat_summary(fun.y = mean, geom = "point", colour = "black", size = 3) +
  stat_summary(fun.y = median, geom = "point", colour = "blue", size = 3) +
  theme_bw()
p



###############################################################
## BOOTSTRAPPING
library(boot)

## set the random number seed
set.seed(1234)
## Draw 5,000 bootstrap samples for distribution of mean differences
bootres <- boot(
  data = mtcars, 
  statistic = function(d, i) {
    as.vector(diff(tapply(d$disp[i], d$vs[i], mean)))
  }, 
  R = 5000)

## plot of the distribution
plot(bootres)


## plot distribution with an added line for mean in the raw data
hist(bootres$t[,1])
abline(v = bootres$t0, col = "blue", lwd = 3)

## bootstrapping medians
set.seed(1234)
bootres2 <- boot(
  data = mtcars, 
  statistic = function(d, i) {
    as.vector(diff(tapply(d$disp[i], d$vs[i], median)))
  }, 
  R = 5000)
##plot of medians
hist(bootres2$t[,1], breaks = 50)
abline(v = bootres2$t0, col = "blue", lwd = 3)


## bootstrapping the log of the variance
set.seed(1234)
bootres3 <- boot(
  data = mtcars, 
  statistic = function(d, i) {
    as.vector(diff(tapply(d$disp[i], d$vs[i], function(x) log(var(x)))))
  }, 
  R = 5000)
##plot of log of the variance
hist(bootres3$t[,1], breaks = 50)
abline(v = bootres3$t0, col = "blue", lwd = 3)

## bootstrapped confidence intervals
round(quantile(bootres3$t[,1], probs = c(.025, .975)), 3)

#directly 
boot.ci(bootres3, type = "perc")

#visual 
hist(bootres3$t[,1], breaks = 50)
abline(v = bootres3$t0, col = "blue", lwd = 3)
abline(v = quantile(bootres3$t[,1], probs = c(.025)), col = "yellow", lwd = 3)
abline(v = quantile(bootres3$t[,1], probs = c(.975)), col = "yellow", lwd = 3)

##'basic' bootstrap
round((2 * bootres3$t0) - quantile(bootres3$t[,1], probs = c(.975, .025)), 3)
##directly
boot.ci(bootres3, type = "basic")

## "normal" interval
bias <- mean(bootres3$t) - bootres3$t0
sigma <- sd(bootres3$t[,1])
##manually 
round(bootres3$t0 - bias - qnorm(c(.975, .025), sd = sigma), 3)
#directly
boot.ci(bootres3, type = "norm")

##bias corrected
boot.ci(bootres3, type = "bca")


## GSS Example
library(foreign)
library(VGAM)

gss2012 <- read.spss("GSS2012merged_R5.sav", to.data.frame = TRUE)
gssr <- gss2012[, c("age", "sex", "marital", "educ", "income06", "satfin", "happy", "health")]
gssr <- na.omit(gssr)

## some data management
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
summary(m)


###write function to pass to boot()
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


##This is the code will take quite some time depending on your system
##In a later chapter, we will discuss more efficient ways to write this
## dependent, naturally, upon your system.
set.seed(1234)
boot.res <- boot(
  data = gssr,
  statistic = model_coef_predictions,
  R = 5000)

boot.res2 <- lapply(1:length(boot.res$t0), function(i) {
  cis <- boot.ci(boot.res, index = i, type = "bca")
  data.frame(Estimate = boot.res$t0[i],
             LL = cis$bca[1, 4],
             UL = cis$bca[1, 5])
})
##Above is the code that will take some time to run

##combine row-wise
boot.res2 <- do.call(rbind, boot.res2)
head(round(boot.res2, 3), 10)


##Label Rows
boot.res2$Type <- rep(c("coef", "Not Satisfied", "More/Less Satisfied", "Satisified"), 
                      c(6, 150, 150, 150))

##repeat and create final dataset
newdat <- expand.grid(
  Agec = seq(from = 0, to = (89 - 18)/10, length.out = 50),
  cincome = mean(gssr$cincom), 
  educ = c(12, 16, 20))

finaldat <- cbind(boot.res2[-(1:6), ], do.call(rbind, rep(list(newdat), 3)))


##plot
p<- ggplot(finaldat, aes(Agec, Estimate, colour = Type, linetype = Type)) + 
  geom_ribbon(aes(ymin = LL, ymax = UL, colour = NULL, fill = Type), alpha = .25) +
  geom_line(size = 1.5) + 
  scale_x_continuous("Age", breaks = (c(20, 40, 60, 80) - 18)/10,
                     labels = c(20, 40, 60, 80)) +
  scale_y_continuous("Probability", labels = percent) + 
  theme_bw() + 
  theme(legend.key.width = unit(1.5, "cm"),
        legend.position = "bottom",
        legend.title = element_blank()) + 
  facet_wrap(~educ) +
  ggtitle("Financial Satisfaction")
p


##Normal theory CI vs bootstrapped.
(coef.tab <- coef(summary(m)))

coef.res <- cbind(boot.res2[1:6, -4],
                  NormalLL = coef.tab[, 1] + qnorm(.025) * coef.tab[, 2],
                  NormalUL = coef.tab[, 1] + qnorm(.975) * coef.tab[, 2])
## show output
coef.res

subset(finaldat, Agec %in% c(0, 7.1) & educ == 16 & Type != "More/Less Satisfied")

index <- c(51, 100, 51 + 300, 100 + 300)
finaldat[index, ]

##bootstrap
tmp.bootres <- boot.res$t0[index + 6]
btmp.bootres <- boot.res$t[, index + 6]


## test differences
deltaSatisfied <- tmp.bootres[4] - tmp.bootres[3]
deltaNotSatisfied <- tmp.bootres[2] - tmp.bootres[1]

bdeltaSatisfied <- btmp.bootres[, 4] - btmp.bootres[, 3]
bdeltaNotSatisfied <- btmp.bootres[, 2] - btmp.bootres[, 1]

test <- deltaSatisfied + deltaNotSatisfied
btest <- bdeltaSatisfied + bdeltaNotSatisfied

## generate histogram
hist(btest, breaks = 50)
abline(v = test, col = "blue", lwd = 5)
abline(v = quantile(btest, probs = .025), col = "yellow", lwd = 5)
abline(v = quantile(btest, probs = .975), col = "yellow", lwd = 5)