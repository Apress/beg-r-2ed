#Multiple Regression

## this chapter relies on many packages
## to focus on the analysis, we install and load all upfront

#install.packages(c("GGally", "arm", "texreg"))

library(foreign)
library(ggplot2)
library(GGally)
library(grid)
library(arm)
library(texreg)

##GSS Example
gss2012 <- read.spss("GSS2012merged_R5.sav", to.data.frame = TRUE)

## reduced GSS
## http://sda.berkeley.edu/sdaweb/analysis/?dataset=gss12
## age = age in years
## sex = female / male
## marital = marital status
## educ = years of education
## hrs2 = hours worked in a usual week
## income06 = income divided into a number of bins
## satfin = satisfaction with finances
## happy = happiness
## health = self rated health
gssr <- gss2012[, c("age", "sex", "marital", "educ", "hrs2", "income06", "satfin", "happy", "health")]

## some data management
gssr <- within(gssr, {
  age <- as.numeric(age)
  educ <- as.numeric(educ)
  hrs2 <- as.numeric(hrs2)
  # recode income categories to numeric
  cincome <- as.numeric(income06)
})

## some visualizaiton


ggpairs(gssr[, c("age", "educ", "hrs2", "cincome")],
        diag = list(continuous = "bar"),
        lower = list(continuous = "smooth"),
        title = "Scatter plot of continuous variables")

## more in depth

ggplot(gssr, aes(educ, cincome)) +
  # use jitter to add some noise and alpha to set transparency more easily see data
  geom_jitter(alpha = .2) +
  stat_smooth() +
  theme_bw() +
  xlab("Years of Education") +
  ylab("Income Bins")

## More data filtering
gssr <- within(gssr, {
  ## pick the maximum value of education or 9
  ## then pick the minimum value of eduction or 18
  reduc <- pmin(pmax(educ, 9), 18)
})

## update graph
ggplot(gssr, aes(reduc, cincome)) +
  geom_jitter(alpha = .2) +
  stat_smooth() +
  theme_bw() +
  xlab("Years of (Recoded) Education") +
  ylab("Income Bins")


## first model
m <- lm(cincome ~ reduc, data = gssr)
summary( m )

## diagnostics
par(mfrow = c(2, 2))
plot( m )


## explore sex differences
ggplot(gssr, aes(reduc, cincome)) +
  geom_jitter(alpha = .2) +
  stat_smooth() +
  theme_bw() +
  xlab("Years of (Recoded) Education") +
  ylab("Income Bins") +
  facet_wrap(~ sex)

## explore education x sex interaction
## update model
m2 <- update(m, . ~ . * sex)
summary( m2 )


## visualize predicted results
## make up new data and use model to get predictions
newdata <- expand.grid(reduc = 9:18,
                       sex = levels(gssr$sex))

head(newdata)

newdata$yhat <- predict(m2, newdata = newdata)

## plot
ggplot(newdata, aes(reduc, yhat, colour = sex)) +
  geom_line(size=1.5) +
  theme_bw() +
  xlab("Recoded Years of Education") +
  ylab("Income Bin")

## explore age effect
p <- ggplot(gssr, aes(age, cincome)) +
  geom_jitter(alpha = .2) +
  stat_smooth() +
  theme_bw() +
  xlab("Age in Years") +
  ylab("Income Bins")
p

## explore age x sex interaction
p2 <- p + facet_wrap(~ sex)
p2

## update model to include age and age squared
m3 <- update(m2, . ~ . + (age + I(age^2)))
summary( m3 )

par(mfrow = c(2, 2))
plot(m3)

## visualize predicted results
## make up new data and use model to get predictions
newdata <- expand.grid(age = 20:80,
                       reduc = c(9, 12, 15, 18),
                       sex = levels(gssr$sex))

head(newdata)

## standard errors for confidence intervals
newdata <- cbind(newdata,
                 predict(m3, newdata = newdata, se.fit = TRUE))

head(newdata)

## plot
ggplot(newdata, aes(age, fit, linetype = sex)) +
  geom_ribbon(aes(ymin = fit - 1.96 * se.fit,
                  ymax = fit + 1.96 * se.fit), alpha = .3) +
  geom_line(size=1.5) +
  theme_bw() +
  theme(
    legend.title = element_blank(), # get rid of legend title
    legend.position = "bottom", # move legend to bottom of graph
    legend.key.width = unit(1, "cm")) + # make each legend bigger
  xlab("Age in Years") +
  ylab("Income Bin") +
  facet_wrap(~ reduc)


## presenting results
coef(m3)
confint(m3)

## put together
output <- cbind(B = coef(m3), confint(m3))
output

round(output, 2)

## rerun model with standardization
z.m3 <- standardize(m3, standardize.y = TRUE)
round(cbind(B = coef(z.m3), confint(z.m3)), 2)

## format output
regCI <- function(model) {
  b <- coef(model)
  cis <- confint(model)
  sprintf("%0.2f [%0.2f, %0.2f]",
          b, cis[, 1], cis[, 2])
}

regCI(m3)

data.frame(
  Variable = names(coef(m3)),
  Raw = regCI(m3),
  Std. = regCI(z.m3))



## using the texreg package

## prettier model output
screenreg(m3, single.row = TRUE)

##iterative model process
screenreg(list(m, m2, m3), single.row = TRUE)