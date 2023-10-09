## Week 2 - Central Limit Theorem Exercises

library(downloader)
url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/mice_pheno.csv"
filename <- basename(url)
download(url, destfile=filename)
dat <- na.omit( read.csv(filename) )
dat


## Exercises 1
?pnorm
pnorm(1) - pnorm(-1)  # What proportion of these numbers(a list of #s that normally distributed)
                      # are within one standard deviation away from the list's average?

## Exercises 2
pnorm(2) - pnorm(-2)  # ... within two standard deviation away from the list's average?


## Exercises 3
pnorm(3) - pnorm(-3)  # ... within three standard deviation away from the list's average?


## Exercises 4
# Define y to be the weights of males on the control diet.
# What proportion of the mice are within one standard deviation away from the average weight?
y <- filter(dat, Diet=="chow" & Sex=='M')%>%select(Bodyweight)%>%unlist
z <- (y - mean(y)) / popsd(y)
mean(abs(z) <= 1)


## Exercises 5
mean(abs(z) <= 2)


## Exercises 6
mean(abs(z) <= 3)


## Exercises 7
qqnorm(z)
abline(0,1)
?qqplot
qqplot(z,abline(0,1))


## Exercises 8
# We'll use the function replicate() to learn about the distribution of random variables.
y <- filter(dat, Diet=="chow" & Sex=='M')%>%select(Bodyweight)%>%unlist
set.seed(1)
avgs <- replicate(10000, mean(sample(y, 25)))
mypar(1,2)
hist(avgs)
qqnorm(avgs)
qqline(avgs)
mean(avgs)


## Exercises 9
popsd(avgs)

