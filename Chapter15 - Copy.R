## Logistic Regression

library(foreign)
library(ggplot2)
library(GGally)
library(grid)
library(arm)
library(texreg)

##GSS Example Data
gss2012 <- read.spss("GSS2012merged_R5.sav", to.data.frame = TRUE)

## reduced GSS
## Info on variables from here:
##http://sda.berkeley.edu/sdaweb/analysis/?dataset=gss12
## age = age in years
## sex = female / male
## marital = marital status
## educ = years of education
## income06 = income divided into a number of bins
## satfin = satisfaction with finances
## happy = happiness
## health = self rated health

## drop all missing cases for simplicity
gssr <- gss2012[, c("age", "sex", "marital", "educ", "income06", "satfin", "happy", "health")]
gssr <- na.omit(gssr)

## some data management
gssr <- within(gssr, {
  age <- as.numeric(age)
  educ <- as.numeric(educ)
  cincome <- as.numeric(income06)
})


## plot satfin
ggplot(gssr, aes(satfin)) +
  geom_bar() +
  theme_bw()



## append binomial recode of satfin
gssr$Satisfied <- as.integer(gssr$satfin == "SATISFIED")

table(gssr$Satisfied)


## attempt linear model on age as predictor
m.lin <- lm(Satisfied ~ age, data = gssr)

## diagnostic plots in a 2 x 2 grid
par(mfrow = c(2, 2))
plot(m.lin)


## categorical data on age for chi squared
gssr$Age45 <- as.integer(gssr$age >= 45)

## 2 x 2 contigency table and print results
(tab <- xtabs(~Satisfied + Age45, data = gssr))

## do the chi square test
chisq.test(tab)

## Logistic Regression
m <- glm(Satisfied ~ age, data = gssr, family = binomial())

## as before we can use summary() to get a summary of the model
summary(m)

## fit a null or empty model
m0 <- glm(Satisfied ~ 1, data = gssr, family = binomial())

##model test function call is anova(); here we do test=”LRT”
anova(m0, m, test = "LRT")

#discussion of axis scaling
x0 <- c(5, 15, 25, 35, 45)
x1 <- c(4, 6, 8, 10, 12)
y <- c(0, 10, 20, 30, 40)
par(mfrow = c(1,2))
plot(x0,y, type = "l")
plot(x1,y, type = "l")


## center and rescale age exploration
hist(gssr$age)
summary(gssr$age)

## start at zero rather than 18 and sort by decade
gssr$Agec <- (gssr$age - 18) / 10

## refit model
m <- glm(Satisfied ~ Agec, data = gssr, family = binomial())
summary(m)


## convert coefficients to odds ratios via exponentiating
cbind(LogOdds = coef(m), Odds = exp(coef(m)))
## ODDS are not probability - common mistake


## confidence intervals
## coefficient + 95% CIs on the log odds scale
results <- cbind(LogOdds = coef(m), confint(m))
results

## exponentiate for odds
## We said it in the book, but again: calculate CI and THEN exponentiate
exp(results)


## create a new dataset of ages, across the whole age range
newdat <- data.frame(Agec = seq(0, to = (89 - 18)/10, length.out = 200))

## want predicted probability scale so use type="response"
newdat$probs <- predict(m, newdata = newdat, type = "response")

head(newdat)

#plot
p <- ggplot(newdat, aes(Agec, probs)) +
  geom_line() +
  theme_bw() +
  xlab("Age - 18 in decades") +
  ylab("Probability of being financially satisfied")
p

#make graph more intuative to interpret
p2 <- p +
  scale_x_continuous("Age in years", breaks = (c(20, 40, 60, 80) - 18)/10,
  labels = c(20, 40, 60, 80))
p2


## find more predictors!
m2 <- update(m, . ~ . + cincome + educ)
summary(m2)

## find even more predictors!!
m3 <- update(m, . ~ . + cincome * educ)
summary(m3)

## should we have so many predictors?
## McFadden R-squared Function
McFaddenR2 <- function(mod0, mod1, ... ){
  L0 <- logLik(mod0)
  L1 <- logLik(mod1)
  MFR2 <- 1 - (L1/L0)
  return(MFR2)
}

MFR2_1 <- McFaddenR2(m0,m)
MFR2_2 <- McFaddenR2(m0,m2)
MFR2_3 <- McFaddenR2(m0,m3)

McFaddenResults <- cbind( c(MFR2_1,MFR2_2,MFR2_3))
McFaddenResults

## summary data for education and income bins
summary(gssr$educ)
summary(gssr$cincome)

## generate new data to pass to predict()
newdat <- expand.grid(
  educ = c(12, 16, 20),
  cincome = 1:25,
  Agec = mean(gssr$Agec))

## get prediction and standard errors
newdat <- cbind(newdat, predict(m3, newdata = newdat,
                                type = "link", se.fit = TRUE))
## show what our prediction and std errs look like.
head(newdat)


## create confidence intervals
newdat <- within(newdat, {
  LL <- fit - 1.96 * se.fit
  UL <- fit + 1.96 * se.fit
})

## convert predicted values & CI to probability scale
newdat <- within(newdat, {
  Probability <- plogis(fit)
  LLprob <- plogis(LL)
  ULprob <- plogis(UL)
})

## functional yet not very pretty
p3 <- ggplot(newdat, aes(cincome, Probability, color = factor(educ))) +
  geom_ribbon(aes(ymin = LLprob, ymax = ULprob), alpha = .5) +
  geom_line() +
  theme_bw() +
  xlab("Income Bins") +
  ylab("Probability of being financially satisfied")
p3


##Make the graph prettier
library(scales) # to use labels = percent

p4 <- ggplot(newdat, aes(cincome, Probability,
                         color = factor(educ), fill = factor(educ),
                         linetype = factor(educ))) +
  geom_ribbon(aes(ymin = LLprob, ymax = ULprob, color = NULL), alpha = .25) +
  geom_line(size = 1) +
  theme_bw() +
  scale_y_continuous(labels = percent) +
  scale_color_discrete("Education") +
  scale_fill_discrete("Education") +
  scale_linetype_discrete("Education") +
  xlab("Income Bins") +
  ylab("Probability of being financially satisfied") +
  theme(legend.key.size = unit(1, "cm"),
        legend.position = "bottom") +
  coord_cartesian(xlim = c(1, 25), ylim = c(0, .65))
p4

#####################################
## ordered logistic regression

## install VGAM package for ordered logistic models
##install.packages("VGAM")
library(VGAM)

## create an ordered factor
gssr$satfin <- factor(gssr$satfin,
                      levels = c("NOT AT ALL SAT", "MORE OR LESS", "SATISFIED"),
                      ordered = TRUE)

## create model using VGAM's vglm()
mo <- vglm(satfin ~ 1,
           family = cumulative(link = "logit", parallel = TRUE, reverse = TRUE),
           data = gssr)

## output summary of null model
summary(mo)

## probability of being in level 3 (satisfied)
plogis(-1.09063)

## probability of being in level 2 (more or less satisfied)
plogis(0.84478) - plogis(-1.09063)


## probability of being in level 1 (not satisfied)
1 - plogis(0.84478)

## The above should match this as m0 is a null model.
prop.table(table(gssr$satfin))


##update model
mo1 <- vglm(satfin ~ 1 + Agec,
           family = cumulative(link = "logit", parallel = TRUE, reverse = TRUE),
           data = gssr)
summary(mo1)

cbind(LogOdds = coef(mo1), Odds = exp(coef(mo1)))

#create data to input for prediction
newdat <- data.frame(Agec = seq(from = 0, to = (89 - 18)/10, length.out = 200))

#append prediction data to dataset
newdat <- cbind(newdat, predict(mo1, newdata = newdat, type = "response"))

#view new data set
head(newdat)

## convert to a "long" dataset
library(reshape2)
newdat <- melt(newdat, id.vars = "Agec")
head(newdat)

## give better names
newdat$variable <- factor(newdat$variable,
                          levels = c("NOT AT ALL SAT", "MORE OR LESS", "SATISFIED"),
                          labels = c("Not Satisfied", "More/Less Satisfied", "Satisfied"))

p5 <- ggplot(newdat, aes(Agec, value, color = variable, linetype = variable)) +
  geom_line(size = 1.5) +
  scale_x_continuous("Age", breaks = (c(20, 40, 60, 80) - 18)/10,
                     labels = c(20, 40, 60, 80)) +
  scale_y_continuous("Probability", labels = percent) +
  theme_bw() +
  theme(legend.key.width = unit(1.5, "cm"),
        legend.position = "bottom",
        legend.title = element_blank()) +
  ggtitle("Financial Satisfaction")
p5


## marginal effect for the first person
margeff(mo1, subset = 1)

## average marginal effect in the data
rowMeans(margeff(mo1)["Agec", ,])


##########################################################
## Seeing how the ordinal model works

##parallel = FALSE, predictor “Agec” allowed to be different
mo.alt <- vglm(satfin ~ 1 + Agec,
               family = cumulative(link = "logit", parallel = FALSE, reverse = TRUE),
               data = gssr)

## using non parallel (i.e., coefficients can vary)
summary(mo.alt)


## What if we used fully seperate models?
gssr <- within(gssr, {
  satfin23v1 <- as.integer(satfin != "NOT AT ALL SAT")
  satfin3v12 <- as.integer(satfin == "SATISFIED")
})

m23v1 <- glm(satfin23v1 ~ 1 + Agec,
             family = binomial(),  data = gssr)
m3v12 <- glm(satfin3v12 ~ 1 + Agec,
             family = binomial(), data = gssr)

## exploring completely separate models
summary(m23v1)
summary(m3v12)



##############################################
## Multinomial Regression
##multiple, qualitative, nonordered outcomes


## predicting marital status example
table(gssr$marital)

m.multi <- vglm(marital ~ 1 + Agec,
                family = multinomial(),
                data = gssr)
summary(m.multi)



## Change last category comparison group if not 'best'
m.multi <- vglm(marital ~ 1 + Agec,
                family = multinomial(refLevel = 1),
                data = gssr)
summary(m.multi)



## Graph the probabilities
newdat <- data.frame(Agec = seq(from = 0, to = (89 - 18)/10, length.out = 200))
newdat <- cbind(newdat, predict(m.multi, newdata = newdat, type = "response"))
head(newdat)

## long data for ggplot2
newdat <- melt(newdat, id.vars = "Agec")

ggplot(newdat, aes(Agec, value, color = variable, linetype = variable)) +
  geom_line(size = 1.5) +
  scale_x_continuous("Age", breaks = (c(20, 40, 60, 80) - 18)/10,
                     labels = c(20, 40, 60, 80)) +
  scale_y_continuous("Probability", labels = percent) +
  theme_bw() +
  theme(legend.key.width = unit(1.5, "cm"),
        legend.position = "bottom",
        legend.direction = "vertical",
        legend.title = element_blank()) +
  ggtitle("Marital Status")


## visualize using area plot
ggplot(newdat, aes(Agec, value, fill = variable)) +
  geom_area(aes(ymin = 0)) +
  scale_x_continuous("Age", breaks = (c(20, 40, 60, 80) - 18)/10,
                     labels = c(20, 40, 60, 80)) +
  scale_y_continuous("Probability", labels = percent) +
  theme_bw() +
  theme(legend.key.width = unit(1.5, "cm"),
        legend.position = "bottom",
        legend.direction = "vertical",
        legend.title = element_blank()) +
  ggtitle("Marital Status")
