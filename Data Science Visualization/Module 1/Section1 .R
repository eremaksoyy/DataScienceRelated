# load the dataset
library(dslabs)
data(heights)

# make a table of category proportions
prop.table(table(heights$sex))

a <- seq(min(my_data), max(my_data), length = 100)    # define range of values spanning the dataset
cdf_function <- function(x) {    # computes prob. for a single value
  mean(my_data <= x)
}
cdf_values <- sapply(a, cdf_function)
plot(a, cdf_values)

library(tidyverse)
library(dslabs)
data(heights)
index <- heights$sex=="Male"
x <- heights$height[index]

# calculate the mean and standard deviation manually

average <- sum(x)/length(x)
SD <- sqrt(sum((x - average)^2)/length(x))
SD
# built-in mean and sd functions - note that the audio and printed values disagree
average <- mean(x)
SD <- sd(x)
c(average = average, SD = SD)

# calculate standard units
z <- scale(x)

# calculate proportion of values within 2 SD of mean
mean(abs(z) < 2)

1 - pnorm(70.5, mean(x), sd(x)) # We can estimate the probability that a male is taller than 70.5 inches with this line

# plot distribution of exact heights in data
plot(prop.table(table(x)), xlab = "a = Height in inches", ylab = "Pr(x = a)")

# probabilities in actual data over length 1 ranges containing an integer
mean(x <= 68.5) - mean(x <= 67.5)
mean(x <= 69.5) - mean(x <= 68.5)
mean(x <= 70.5) - mean(x <= 69.5)

# probabilities in normal approximation match well
pnorm(68.5, mean(x), sd(x)) - pnorm(67.5, mean(x), sd(x))
pnorm(69.5, mean(x), sd(x)) - pnorm(68.5, mean(x), sd(x))
pnorm(70.5, mean(x), sd(x)) - pnorm(69.5, mean(x), sd(x))

# probabilities in actual data over other ranges don't match normal approx as well
mean(x <= 70.9) - mean(x <= 70.1)
pnorm(70.9, mean(x), sd(x)) - pnorm(70.1, mean(x), sd(x))



library(dslabs)
data(heights)
p <- seq(0.01, 0.99, 0.01)
percentiles <- quantile(heights$height, p)
first <- percentiles[names(percentiles)=="25%"]
second <- percentiles[names(percentiles)=="50%"]
third <- percentiles[names(percentiles)=="75%"]
first
second
third


theoretical_quantiles <- qnorm(p, 69, 3) # to determine the theoretical quantiles of a dataset: that is, the theoretical value of 
                                         # quantiles assuming that a dataset follows a normal distribution.
library(tidyverse)
library(dslabs)
data(heights)
index <- heights$sex=="Male"
x <- heights$height[index]
z <- scale(x)

mean(x <= 69.5)  # proportion of data below 69.5

# calculate observed and theoretical quantiles
p <- seq(0.05, 0.95, 0.05)  
observed_quantiles <- quantile(x, p)
theoretical_quantiles <- qnorm(p, mean=mean(x), sd=sd(x))

# make QQ-plot
plot(theoretical_quantiles, observed_quantiles)
abline(0,1)

# make QQ-plot with scaled values
observed_quantiles <- quantile(z, p)
theoretical_quantiles<- qnorm(p)
plot(theoretical_quantiles, observed_quantiles)
abline(0,1)
