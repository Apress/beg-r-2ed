#choose one to load either a fresh copy or from saved work:
hsb <- read.csv ("http://www.ats.ucla.edu/stat/data/hsb.csv")
#load ("hsb.rda")


ses <- table ( hsb $ ses )
pie ( ses , main = " Pie Chart ")

install.packages ("ggplot2")
library ( ggplot2 )
bar <- ggplot ( hsb , aes (x = factor(ses) )) + geom_bar()
bar


#boxplot
boxplots <- ggplot ( hsb , aes( factor(ses) , math )) + geom_boxplot()
boxplots

#boxplot variety
install.packages ("openintro")
library ( openintro )
head ( cars )

mpgBox <- ggplot (cars , aes( factor(0) , mpgCity )) + geom_boxplot()
mpgBox <- mpgBox + theme ( axis.title.x = element_blank() , axis.text.x = element_blank (), axis.ticks.x = element_blank())
mpgBox

#compare and contrast
boxplot(cars$mpgCity)


install.packages ("gridExtra")
library ( gridExtra )

myHist1 <- ggplot (cars , aes ( mpgCity )) + geom_histogram ( fill = " white ", color = "black ")
myHist2 <- ggplot (cars , aes ( mpgCity )) + geom_histogram ( binwidth = 5, fill = "white", color = " black ")
grid.arrange(myHist1, myHist2, ncol = 2)


#dotplot
table ( cars $ mpgCity )
dot <- ggplot (cars , aes ( mpgCity )) + geom_dotplot ()
dot

#frequency polygon
polygon <- ggplot (cars , aes ( mpgCity )) + geom_freqpoly ( binwidth = 5)
polygon


#smoothed density plot
density <- ggplot (cars , aes ( mpgCity )) + geom_density ( fill = " gray ")
density


#scatter plot w/ 95% CI
scatter <- ggplot (cars , aes (weight , mpgCity )) + geom_point ()
scatter <- scatter + geom_smooth ( method = lm)
scatter

#scatter plot w/ 95% CI and transparency
scatter <- ggplot (cars , aes (weight , mpgCity )) + geom_point (alpha = .5)
scatter <- scatter + geom_smooth ( method = lm)
scatter


#hexagonal bin plot
install.packages ("hexbin")
library(hexbin)
dplot  <- ggplot (diamonds , aes ( price , carat ))
splot <- dplot + geom_point( alpha = .25 )
hex <- dplot + geom_hex ()
grid.arrange ( splot, hex, ncol = 2 )