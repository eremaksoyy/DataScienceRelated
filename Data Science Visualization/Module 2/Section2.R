
library(dslabs)
library(ggplot2)
library(dplyr)
data(murders)


ggplot(data = murders)

murders %>% ggplot() + geom_point(aes(x=population/10^6, y=total))

# add points layer to predefined ggplot object
p <- ggplot(data=murders)
p + geom_point(aes(population/10^6, total))


# add text layer to the scatter plot
p_test <- p + geom_point(aes(population/10^6, total)) + geom_text(aes(population/10^6, total, label=abb))
p_test

# error - as "abb" is not a globally defined variable and cannot be found outside of aes
p_test <- p + geom_point(aes(population/10^6, total)) + geom_text(aes(population/10^6, total), label=abb)

# change the size of the points
p_test <- p + geom_point(aes(population/10^6, total), size=3) + geom_text(aes(population/10^6, total, label=abb))
p_test

# move text labels slightly to the right
p_test <- p + geom_point(aes(population/10^6, total), size=3) + geom_text(aes(population/10^6, total, label=abb), nudge_x=1)
p_test

# simplify code by adding global aesthetic
p <- murders %>% ggplot(aes(population/10^6, total, label=abb))
p + geom_point(size=3) + geom_text(nudge_x=1.5) 

# local aesthetics override global aesthetics
p + geom_point(size=3) + geom_text(aes(x=10, y=800, label="Hello there!"))

# log 10 scale the x-axis and y-axis
p + geom_point(size=3) + geom_text(nudge_x=0.05) + scale_x_continuous(trans="log10") + scale_y_continuous(trans="log10") 

# efficient log scaling of the axes
p + geom_point(size=3) + geom_text(nudge_x=0.075) + scale_x_log10() + scale_y_log10()


# add labels and title
p + geom_point(size=3) + geom_text(nudge_x=0.075) + scale_x_log10() + scale_y_log10() + 
  xlab("Population in millions (log scale)") + ylab("Total number of murders (log scale)") + ggtitle("US Gun Murders in 2010")


# change color of the points
# redefine p to be everything except the points layer
p <- murders %>% ggplot(aes(population/10^6, total, label=abb)) + geom_text(nudge_x=0.075) + scale_x_log10() + scale_y_log10() +
  xlab("Population in millions (log scale)") + ylab("Total number of murders (log scale)") + ggtitle("US Gun Murders in 2010")

# make all points blue
p + geom_point(size=3, color="blue")

# color points by region
p + geom_point(aes(col=region), size=3)


# add a line with average murder rate
# define average murder rate
r <- murders %>% summarize(rate=sum(total) / sum(population)*10^6) %>% pull(rate)

# basic line with average murder rate for the country
p <- p + geom_point(aes(col=region), size=3) + geom_abline(intercept=log10(r))   # slope is default of 1

# change line to dashed and dark grey line under points
p <- p + geom_abline(intercept=log10(r), lty=2, color="darkgrey") + geom_point(aes(col=region), size=3)
p

# change legend title
p <- p + scale_color_discrete(name="Region")   # capitalize legend title
p

ds_theme_set()

# themes from ggthemes
install.packages("ggthemes")
library(ggthemes)
p + theme_economist()   # style of the Economist magazine
p + theme_fivethirtyeight()   # style of the FiveThirtyEight website


# PUTTING ALL TOGETHER FROM SCRATCH TO ASSEMBLE THE PLOT
# load libraries
library(ggthemes)
install.packages("ggrepel")
library(ggrepel)
library(dslabs)
data(murders)

# define the intercept
r <- murders %>% summarize(rate=sum(total) / sum(population) * 10^6) %>%.$rate

# make the plot combining all elements
murders %>% ggplot(aes(population/10^6, total, label=abb)) + geom_abline(intercept=log10(r), lty=2, color="darkgrey") +
  geom_point(aes(col=region), size=3) + geom_text_repel(max.overlaps=Inf) + scale_x_log10() + scale_y_log10() + 
  xlab("Population in millions (log scale)") + ylab("Total number of murders (log scale)") + ggtitle("US Gun Murders in 2010") + 
  scale_color_discrete(name="Region") + theme_economist()
  

# HISTOGRAM IN GGPLOT2
# load heights data
data(heights)

# define p
p <- heights %>% filter(sex=="Male") %>% ggplot(aes(x=height))

# basic histograms 
p + geom_histogram()
p + geom_histogram(binwidth=1)

# histogram with blue ill, black outline, labels and title
p + geom_histogram(binwidth=1, fill="blue", col="black") + xlab("Male heights in inches") + ggtitle("Histogram")


# SMOOTH DENSITY PLOT IN GGPLOT2
p <- heights %>% ggplot(aes(x=height, group=sex, fill=sex))
p + geom_density()
p + geom_density(alpha=0.3)  # alpha blending to show the crossing area of two density graphs
# p + geom_density(fill="blue")


# QUANTILE-QUANTILE PLOT IN GGPLOT2
# basic QQ-plot
p <- heights %>% filter(sex=="Female") %>% ggplot(aes(sample=height))
p + geom_qq()

# QQ-plot against a normal distr. with same mean/sd as data
params <- heights %>% filter(sex=="Female") %>% summarize(mean=mean(height), sd=sd(height))
p + geom_qq(dparams = params) + geom_abline()

# QQ-plot of scaled data against the standard normal distr.
heights %>% ggplot(aes(sample=scale(height))) + geom_qq() + geom_abline()


# GRIDS OF PLOTS WITH THE GRIDEXTRA PACKAGE
# define plots p1, p2, p3
p <- heights %>% filter(sex=="Male") %>% ggplot(aes(x=height))
p1 <- p + geom_histogram(binwidth = 1, fill="blue", col="black") + xlab("Male heights in inches")
p1
p2 <- p + geom_histogram(binwidth = 2, fill="red", col="black") + xlab("Male heights in inches")
p2
p3 <- p + geom_histogram(binwidth = 3, fill="green", col="black") + xlab("Male heights in inches")
p3

# arrange plots next to each other in 1 row, 3 columns
library(gridExtra)
grid.arrange(p1, p2, p3, ncol=3)















