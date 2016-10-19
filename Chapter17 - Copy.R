install.packages(c("gridExtra", "plot3D", "cowplot", "Hmisc", "ggplot2"))
install.packages("Rcpp")


library(grid)
library(gridExtra)
library(ggplot2)
library(GGally)
library(RColorBrewer)
library(plot3D)
library(scatterplot3d)
library(scales)
library(hexbin)
library(cowplot)
library(boot)
library(Hmisc)

pdf(file = "chapter17_set1a_%03d.pdf", width = 6.5, height = 6.5, onefile = TRUE)

## Univariate Plots

p1 <- ggplot(mtcars, aes(mpg))
p1 + geom_histogram() + ggtitle("Histogram")
p1 + geom_density() + ggtitle("Density Plot")


p1 + geom_histogram(aes(y = ..density..), binwidth = 3, fill = "grey50") +
  geom_density(size = 1) +
  ggtitle("Histogram with Density Overlay")

ggplot(diamonds, aes(price)) +
  geom_histogram(aes(y = ..density..), fill = "grey50") +
  geom_density(size = 1) +
  ggtitle("Histogram with Density Overlay")


p1 + geom_dotplot() + ggtitle("Dotplot")


sumstats <- t.test(mtcars$mpg)

p1 +
  geom_histogram(aes(y = ..density..), binwidth = 3, fill = "grey50") +
  geom_point(aes(x = sumstats$estimate, y = -.001), ) +
  geom_segment(aes(x = sumstats$conf.int[1], xend = sumstats$conf.int[2], y = -.001, yend = -.001)) +
  ggtitle("Histogram with Mean and 95% CI")

ggplot(mtcars, aes("MPG", mpg)) + geom_boxplot()


ggplot(diamonds, aes(cut)) +
  geom_bar()


ggplot(diamonds, aes("Cut", fill = cut)) +
  geom_bar()


ggplot(diamonds, aes("Cut", fill = cut)) +
  geom_bar(width = 1) +
  coord_polar(theta = "y")


ggplot(diamonds, aes("Cut", fill = cut)) +
  geom_bar(aes(y = ..count.. / sum(..count..)), width = 1) +
  coord_polar(theta = "y")


ggplot(diamonds, aes(cut)) +
  geom_point(aes(y = ..count.. / sum(..count..)),
             stat = "bin", size = 4)


## Customizing and polishing plots
p1 + geom_histogram(binwidth = 3) +
  xlab("Miles per gallon (MPG)") +
  ylab("Number of Cars") +
  ggtitle("Histogram showing the distribution of miles per gallon")


p1 + geom_histogram(binwidth = 3) +
  xlab(expression(frac("Miles", "Gallon"))) +
  ylab("Number of Cars") +
  ggtitle(expression("Math Example: Histogram showing the distribution of "~frac("Miles", "Gallon")))

## dev.off()

## pdf(file = "chapter17_set1b_%03d.pdf", width = 7, height = 7, onefile = TRUE)

font.sans <- theme(
  axis.text = element_text(family = "serif", size = 12, color = "grey40"),
  axis.title = element_text(family = "serif", size = 12, color = "grey40"),
  plot.title = element_text(family = "serif", size = 16))

p1 + geom_histogram(binwidth = 3) +
  xlab("Miles per Gallon") +
  ylab("Number of Cars") +
  ggtitle("Size and Font Example: Histogram showing the distribution of MPG") +
  font.sans

## dev.off()


## pdf(file = "chapter17_set1c_%03d.pdf", width = 6.5, height = 6.5, onefile = TRUE)

p1 + geom_histogram(binwidth = 3) +
  theme_classic() +
  coord_cartesian(xlim = c(8, 38), ylim = c(0, 8)) +
  xlab("Miles per gallon (MPG)") +
  ylab("Number of cars") +
  ggtitle("Histogram showing the distribution of miles per gallon")

p1 + geom_histogram(color = "black", fill = "white", binwidth = 3) +
  theme_classic() +
  coord_cartesian(xlim = c(8, 38), ylim = c(0, 8)) +
  xlab("Miles per gallon (MPG)") +
  ylab("Number of cars") +
  ggtitle("Histogram showing the distribution of miles per gallon")

p1 + geom_histogram(color = "black", fill = "white", binwidth = 3) +
  scale_x_continuous(breaks = c(10, 20, 30), labels = c("Terrible", "Okay", "Good")) +
  theme_classic() +
  coord_cartesian(xlim = c(8, 38), ylim = c(0, 8)) +
  xlab("Miles per gallon (MPG)") +
  ylab("Number of cars") +
  ggtitle("Histogram showing the distribution of miles per gallon")

p1 + geom_histogram(color = "black", fill = "white", binwidth = 3) +
  scale_x_continuous(breaks = c(10, 20, 30), labels = c("Terrible", "Okay", "Good")) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  coord_cartesian(xlim = c(8, 38), ylim = c(0, 8)) +
  xlab("Miles per gallon (MPG)") +
  ylab("Number of cars") +
  ggtitle("Histogram showing the distribution of miles per gallon")

ggplot(diamonds, aes(price)) +
  geom_histogram()

##square root and log scales
grid.arrange(
  ggplot(diamonds, aes(price)) +
    geom_histogram() +
    scale_x_sqrt() +
    ggtitle("Square root x scale"),
  ggplot(diamonds, aes(price)) +
    geom_histogram() +
    scale_x_log10() +
    ggtitle("log base 10 x scale"))

grid.arrange(
  ggplot(diamonds, aes(price)) +
    geom_histogram() +
    scale_x_continuous(limits = range(diamonds$price), expand = c(0, 0)) +
    coord_trans(x = "sqrt") +
    ggtitle("Square root coordinate system"),
  ggplot(diamonds, aes(price)) +
    geom_histogram() +
    scale_x_continuous(limits = range(diamonds$price), expand = c(0, 0)) +
    coord_trans(x = "log10") +
    ggtitle("Log base 10 coordinate system"))


ggplot(diamonds, aes(price, color = cut)) +
  geom_density(size = 1) +
  scale_x_log10() +
  ggtitle("Density plots colored by cut")

## dev.off()

## pdf(file = "chapter17_set2_%03d.pdf", width = 6.5, height = 9, onefile = TRUE)

display.brewer.all(n = 5, type = "all", colorblindFriendly=TRUE)

## dev.off()

## pdf(file = "chapter17_set3_%03d.pdf", width = 6.5, height = 6.5, onefile = TRUE)

ggplot(diamonds, aes(price, color = cut)) +
  geom_density(size = 1) +
  scale_color_brewer(palette = "Set2") +
  scale_x_log10() +
  ggtitle("Density plots colored by cut") +
  theme(legend.position = "bottom")

ggplot(diamonds, aes(price, color = cut)) +
  geom_density(size = 1) +
  scale_color_brewer(palette = "Set2") +
  scale_x_log10() +
  scale_color_discrete("Diamond Cut") +
  ggtitle("Density plots colored by cut") +
  theme(legend.position = "bottom", legend.direction = "vertical")

ggplot(diamonds, aes(price, color = cut)) +
  geom_density(size = 1) +
  scale_color_discrete("Diamond Cut") +
  ggtitle("Density plots colored by cut") +
  theme(legend.position = c(1, 1), legend.justification = c(1, 1))

ggplot(diamonds, aes(price, color = cut)) +
  geom_density(size = 1) +
  scale_color_discrete("Diamond Cut") +
  scale_x_continuous(labels = dollar) +
  ggtitle("Density plot of diamond price by cut") +
  theme_classic() +
  theme(legend.position = c(1, 1),
        legend.justification = c(1, 1),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title = element_blank()) +
  coord_cartesian(xlim = c(0, max(diamonds$price)), ylim = c(0, 4.2e-04))

grid.arrange(
  ggplot(diamonds, aes(cut, price)) +
    geom_boxplot() +
    ggtitle("Boxplots of diamond price by cut") +
    theme_classic(),
  ggplot(diamonds, aes(cut, price)) +
    geom_boxplot() +
    ggtitle("Boxplots of diamond price by cut - flipped") +
    theme_classic() +
    coord_flip())

## dev.off()

## Multivariate Plots

## pdf(file = "chapter17_set4_%03d.pdf", width = 6.5, height = 6.5, onefile = TRUE)

ggplot(mtcars, aes(mpg, hp)) +
  geom_point(size = 3)

ggplot(mtcars, aes(mpg, hp)) +
  geom_text(aes(label = rownames(mtcars)), size = 2.5)

ggplot(mtcars, aes(mpg, hp, color = factor(cyl))) +
  geom_point(size = 3)

ggplot(mtcars, aes(mpg, hp, color = disp)) +
  geom_point(size=3)

ggplot(mtcars, aes(mpg, hp, color = disp, shape = factor(cyl))) +
  geom_point(size=3)

ggplot(mtcars, aes(mpg, hp)) +
  geom_point(size=3) +
  stat_smooth()

ggplot(mtcars, aes(mpg, hp)) +
  geom_point(size=3) +
  stat_smooth(method = "lm")

ggplot(mtcars, aes(mpg, hp, color = factor(cyl))) +
  geom_point(size=3) +
  stat_smooth(method = "lm", se = FALSE, size = 2)

##one overall summary line
ggplot(mtcars, aes(mpg, hp, color = factor(cyl))) +
  geom_point(size=3) +
  stat_smooth(aes(color = NULL), se = FALSE, size = 2)

##points smaller and semi-transparent.
ggplot(diamonds, aes(price, carat)) +
  geom_point(size = 1, alpha = .25)

##bin and color by density
ggplot(diamonds, aes(price, carat)) +
  geom_hex(bins = 75)


##bin the data and use a boxplot
diamonds <- within(diamonds, {
  pricecat <- cut(price, breaks = quantile(price, probs = seq(0, 1, length.out = 11)), include.lowest = TRUE)
})

ggplot(diamonds, aes(pricecat, carat)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

##violin plot
ggplot(diamonds, aes(pricecat, carat)) +
  geom_violin() +
  scale_y_log10() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

##line plot
ggplot(Indometh, aes(time, conc, group = Subject)) +
  geom_line()

ggplot(Indometh, aes(time, conc, group = Subject)) +
  geom_line() +
  geom_point()

ggplot(Indometh, aes(time, conc, group = Subject)) +
  geom_line() +
  stat_summary(aes(group = NULL), fun.y = mean, geom = "line", color = "blue", size = 2)

##mean and confidence interval
ggplot(Indometh, aes(time, conc)) +
  stat_summary(fun.data = mean_cl_normal, geom = "pointrange")

##bar plot of the means of the continuous variable by the discrete variable.
ggplot(diamonds, aes(cut, price)) +
  stat_summary(fun.y = mean, geom = "bar", fill = "white", color = "black")


ggplot(diamonds, aes(cut, price)) +
  stat_summary(fun.y = mean, geom = "bar", fill = "white", color = "black") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = .2)


##Bootstrap Function
median_cl_boot <- function(x, ...) {
  require(boot)
  args <- list(...)
  # if missing, default to 1000 bootstraps
  if (is.null(args$R)) {
    args$R <- 1000
  }
  result <- boot(x, function(x, i) {median(x[i])}, R = args$R)
  cis <- boot.ci(result, type = "perc")
  data.frame(y = result$t0,
             ymin = cis$percent[1, 4],
             ymax = cis$percent[1, 5])
}

##plotting median CI using bootstrap function
ggplot(diamonds, aes(cut, price)) +
  stat_summary(fun.y = median, geom = "bar", fill = "white", color = "black") +
  stat_summary(fun.data = median_cl_boot, geom = "errorbar", width = .2)

## dev.off()

################################
## pdf(file = "chapter17_set5_%03d.pdf", width = 3.5, height = 4.5, onefile = TRUE)

##  Waterfall plot set up
company <- data.frame(
  Month = months(as.Date(paste0("2015-", 1:12, "-01"))),
  NetIncome = c(-6, 7, -5, 5, 13, -3, -4, -1, 11, 4, -10, 8))
grid.newpage()
grid.table(company)

## dev.off()

## pdf(file = "chapter17_set6_%03d.pdf", width = 6.5, height = 4.5, onefile = TRUE)

company <- within(company, {
  MonthC <- as.numeric(factor(Month, levels = Month))
  MonthEnd <- c(head(cumsum(NetIncome), -1), 0)
  MonthStart <- c(0, head(MonthEnd, -1))
  GainLoss <- factor(as.integer(NetIncome > 0), levels = 0:1, labels = c("Loss", "Gain"))
})
grid.newpage()
grid.table(company)

## dev.off()

## pdf(file = "chapter17_set7_%03d.pdf", width = 6.5, height = 6.5, onefile = TRUE)

##actual waterfall plot
ggplot(company, aes(MonthC, fill = GainLoss)) +
  geom_rect(aes(xmin = MonthC - .5, xmax = MonthC + .5,
                ymin = MonthEnd, ymax = MonthStart)) +
  geom_hline(yintercept = 0, size = 2, linetype = 2) +
  scale_fill_manual(values = c("Loss" = "orange", "Gain" = "blue")) +
  scale_x_continuous(breaks = company$MonthC, labels = company$Month) +
  xlab("") +
  scale_y_continuous(labels = dollar) +
  ylab("Net Income in Thousands") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        legend.position = c(0, 1),
        legend.justification = c(0, 1),
        legend.title = element_blank())


#######################################################
## Multiple Plots


ggplot(diamonds, aes(carat, price, color = color)) +
  geom_point() +
  stat_smooth(method = "loess", se = FALSE, color = "black") +
  facet_grid(clarity ~ cut) +
  theme_bw() +
  theme(legend.position = "bottom",
        legend.title = element_blank())


ggplot(diamonds, aes(carat, color = cut)) +
  geom_density() +
  facet_wrap(~clarity, scales = "free") +
  theme_bw() +
  theme(legend.position = "bottom",
        legend.title = element_blank())

##scatter plot matrix
ggscatmat(mtcars[, c("mpg", "disp", "hp", "drat", "wt", "qsec")])


##heatmap
ggcorr(mtcars[, c("mpg", "disp", "hp", "drat", "wt", "qsec")])


plota <- ggplot(mtcars, aes(mpg, hp)) +
  geom_point(size = 3) +
  stat_smooth(se = FALSE)
plotb <- ggplot(mtcars, aes(mpg)) +
  geom_density() +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank(),
        axis.line.y = element_blank())
plotc <- ggplot(mtcars, aes(hp)) +
  geom_density() +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank(),
        axis.line.y = element_blank())


##Using the cowplot package
ggdraw() +
  draw_plot(plota, 0, 0, 2/3, 1) +
  draw_plot(plotb, 2/3, .5, 1/3, .5) +
  draw_plot(plotc, 2/3, 0, 1/3, .5) +
  draw_plot_label(c("A", "B", "C"), c(0, 2/3, 2/3), c(1, 1, .5), size = 15)

##########################################
## Three Dimensional Graphs

#####Contour plots
m <- lm(price ~ (carat + I(carat^2)) * (x + I(x^2)), data = diamonds)

newdat <- expand.grid(carat = seq(min(diamonds$carat), max(diamonds$carat), length.out = 100),
x = seq(min(diamonds$x), max(diamonds$x), length.out = 100))

newdat$price <- predict(m, newdata = newdat)

ggplot(newdat, aes(x = x, y = carat, z = price)) +
geom_contour(aes(color = log(..level..)), bins = 30, size = 1)

## dev.off()


## pdf(file = "chapter17_set8_%03d.pdf", width = 6.5, height = 3.5, onefile = TRUE)

####quick look
ggplot(diamonds, aes(carat, x)) +
geom_point(alpha = .25, size = 1)

## dev.off()

## three dimensional scatterplot.

## pdf(file = "chapter17_set9_%03d.pdf", width = 6.5, height = 6.5, onefile = TRUE)

with(mtcars, scatter3D(hp, wt, mpg, pch = 16, type = "h", colvar = NULL))

## dev.off()

##pdf(file = "chapter17_set10_%03d.pdf", width = 4.5, height = 4.5*2, onefile = TRUE)

par(mfrow = c(2, 1), mar = c(3, 2, 3, 2))

with(mtcars, scatter3D(hp, wt, mpg,
colvar = cyl, col = c("blue", "orange", "black"),
colkey = FALSE,
pch = 16, type = "h",
theta = 0, phi = 30,
ticktype = "detailed",
main = "Three-dimensional colored scatterplot"))

with(mtcars, scatter3D(hp, wt, mpg,
colvar = cyl, col = c("blue", "orange", "black"),
colkey = FALSE,
pch = 16, type = "h",
theta = 220, phi = 10,
ticktype = "detailed",
xlab = "Horsepower", ylab = "Weight", zlab = "Miles per Gallon",
main = "Three-dimensional colored scatterplot"))

##dev.off()
