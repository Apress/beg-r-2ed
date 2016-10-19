#Inline Calculations motivating ANOVA
choose(8,2)
1-(.95^28)


#ANOVA
##Read in the data for one-way ANOVA

myData <- read.table("ANOVA001.txt", sep="\t", header=TRUE)

## relevel the factors to be in chronological order
myData$Term <- factor(myData$Term, levels = c("FA12", "SP13", "FA13"))


summary(myData)


#EDA boxplots

plot(Score ~ Term, data=myData)

#Visual Inspection of Data to 'test' for normality
#Author's Note: NEVER trust your eyes.

#install.packages ("gridExtra")
library ( gridExtra )
library ( ggplot2 )

ggplot(myData, aes(Score, color = Term)) +
  geom_histogram(fill = "white", binwidth = 10) +
  facet_wrap(~ Term) +
  theme(legend.position = "none")


#Shapiro-Wilk Normality Test loop
#if this prints any factor values, ANOVA is not necessarily indicated
#This for loop would print any p-value as well as the name of the term if it were not normal
for (n in levels(myData$Term)) {
  test <- with(subset(myData, Term == n), shapiro.test(Score))
  if (test$p.value < 0.05) {
    print(c(n, test$p.value))
  }
}

#Are the variances the same?  Note: Bartlett is sensitive to normality.  We tested for normal with shapiro.

#Bartlett is better if we believe our data are normal.

bartlett.test(Score ~ Term, data = myData)


#run an ANOVA and generate a summary table
results <- aov(Score ~ Term, data = myData)
summary(results)


#TukeyHSD
TukeyHSD(results)



#3+ Samples with Netherland School Data
library(MASS)
twoway <- aov(IQ ~ SES * lang, data = nlschools)
summary ( twoway )

## update the model to drop interaction
twoway.reduced <- update(twoway, . ~ . - SES:lang)
summary ( twoway.reduced )


repeated <- read.table("repeated_fitness_Ch12.txt", sep = " ", header = TRUE)
repeated

## convert some variables in the data to factors
## to tell R they are categorical
repeated <- within(repeated, {
  id <- factor ( id )
  time <- factor ( time )
})

results <- aov ( fitness ~ time + Error (id / time ), data = repeated)
summary ( results )

library(ggplot2)
meansPlot <- ggplot(repeated, aes (as.numeric(time) , fitness)) +
  stat_summary(fun.y = mean , geom ="point") +
  stat_summary (fun.y = mean , geom = "line")
meansPlot

## add 95% confidence intervals to the means
meansPlot2 <- ggplot(repeated, aes (as.numeric(time) , fitness)) +
  stat_summary(fun.data = mean_cl_normal, geom ="pointrange") +
  stat_summary (fun.y = mean , geom = "line")
meansPlot2




mixedModel <- read.csv("mixedModel.csv")
str ( mixedModel )
mixedModel

##install.packages("ez")
##install.packages("quantreg")
library(quantreg)
library (ez)
ezANOVA ( mixedModel , score , id , distr , between = age )

with(mixedModel, interaction.plot (age , distr , score ))