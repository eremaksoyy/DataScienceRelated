## Week 4 - Exploratory Data Analysis

# Scatterplot

# Exercises 1
data(nym.2002, package="UsingR")
library(dplyr)
males <- filter(nym.2002, gender=="Male")
females <- filter(nym.2002, gender=="Female")
cor(males$age, males$time)


# Exercises 2
cor(females$age, females$time)


# Exercises 3
library(rafalib)
mypar(2, 2)
plot(females$age, females$time)
plot(males$age, males$time)
group <- floor(females$age/5)*5
boxplot(females$time~group)
group <- floor(males$age/5)*5
boxplot(males$time~group)


# Symmetry of Log Ratios
time = sort(nym.2002$time)

# Exercises 1
result <- min(time)/median(time) # The fastest time divided by the median time
result 

# Exercises 2
result <- max(time)/median(time) # The slowest time divided by the median time
result


# Plots to Avoid
library(dslabs)
data("divorce_margarine")
plot(divorce_margarine$margarine_consumption_per_capita, divorce_margarine$divorce_rate_maine)
cor(divorce_margarine$margarine_consumption_per_capita, divorce_margarine$divorce_rate_maine)


# Median, MAD, and Spearman Correlation 
data(ChickWeight)
head(ChickWeight)
plot( ChickWeight$Time, ChickWeight$weight, col=ChickWeight$Diet)
# To facilitate the comparison of weights at different time points and across the different chicks, 
# we will reshape the data so that each row is a chick.

chick = reshape(ChickWeight, idvar=c("Chick","Diet"), timevar="Time",direction="wide")
# The meaning of this line is: reshape the data from long to wide, where the columns Chick and Diet 
# are the ID's and the column Time indicates different observations for each ID.

head(chick)

chick = na.omit(chick)
# We also want to remove any chicks that have missing observations at any time points (NA for "not available"). 
# The line of code above identifies these rows and then removes them.

# Exercises 1
mean(c(chick$weight.4, 3000))/mean(chick$weight.4)  # How much does the average of chick weights at day 4 increase if we add an outlier measurement of 3000 grams?

# Exercises 2
median(c(chick$weight.4, 3000))/median(chick$weight.4)

# Exercises 3
sd(c(chick$weight.4, 3000))/sd(chick$weight.4)


# Exercises 4
mad(c(chick$weight.4, 3000))/mad(chick$weight.4)


# Exercises 5
cor(c(chick$weight.4, 3000), c(chick$weight.21,3000))/cor(chick$weight.4, chick$weight.21)


# Mann-Whitney-Wilcoxon Test
chick = reshape(ChickWeight, idvar=c("Chick","Diet"), timevar="Time",direction="wide")
head(chick)
chick = na.omit(chick)

# Exercises 1
x <- filter(chick, Diet==1)$weight.4
y <- filter(chick, Diet==4)$weight.4
t.test(x,y) # This line performs a t-test comparing x and y.
wilcox.test(x,y) # This line performs a Wilcoxon test of x and y.
x <- c(x, 200) # Adding a single chick of weight 200 grams to x.
t.test(x,y)$p.value

# The lines of code from the solution:
x <- chick$weight.4[chick$Diet==1]
y <- chick$weight.4[chick$Diet==4]
t.test(c(x,200),y)$p.value


# Exercises 2
wilcox.test(c(x,200),y)$p.value # Now we compute a wilcox test here with the same method(after adding 200 grams of a chick). 
                                # The Wilcoxon test is robust to the outlier.

# Exercises 3
library(rafalib)
mypar(1,3)
boxplot(x,y)
boxplot(x,y+10)
boxplot(x,y+100)
add_10 <- t.test(x,y+10)$statistic
add_100 <- t.test(x,y+100)$statistic
difference <- add_10 - add_100
difference


## Week 4 Quiz

# Question 1
# Scatter plots are designed to show how one set of data relates to another. Bar and pie plots only use one data set, 
# and box plots require multiple y points for each x location.


# Question 2
wilcox.test(c(1,2,3),c(4,5,6))
wilcox.test(c(1,2,3),c(4,5,6))$p.value


# Question 3
wilcox.test(c(1,2,3),c(400,500,600))$p.value


# Question 4 
data(nym.2002, package="UsingR")
time = sort(nym.2002$time)

plot(time/median(time), ylim=c(1/4,4)) # A plot of the ratio of times to the median time, with horizontal lines at twice as fast as the median time, and twice as slow as the median time.
abline(h=c(1/2,1,2))

plot(log2(time/median(time)),ylim=c(-2,2)) # A plot of the log2 ratio of times to the median time. The horizontal lines indicate the same as above: twice as fast and twice as slow.
abline(h=-1:1)



x <- c(1,2,3)
y <- c(3,4)
z <- x*y
z







