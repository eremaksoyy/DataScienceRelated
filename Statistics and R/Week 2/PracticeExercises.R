## Week 2 - CLT and t-distribution in Practice Exercises

library(dplyr)
library(downloader)
url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/femaleMiceWeights.csv"
filename <- "femaleMiceWeights.csv"
if(!file.exists("femaleMiceWeights.csv")) download(url,destfile=filename)
dat <- read.csv(filename)

### Formula: Z = sqrt(12) * (X - muX)/sd(X)

# Exercises 1
x = sample(1:6, n, replace=TRUE)
mean(x==6)  # When the proportion we are interested in is expressed as an average.As the die rolls 
            # are independent, the CLT applies.
set.seed(1)
n <- 100
sides <- 6
p <- 1/sides
zs <- replicate(10000, {
  x <- sample(1:sides, n, replace=TRUE) 
  (mean(x==6) - p) / sqrt(p*(1-p)/n)
  })
qqnorm(zs)
abline(0,1) # To confirm that it's well approximated with normal distribution.
mean(abs(zs) > 2)


# Exercises 2
# The CLT is an asymptotic result, meaning it is closer and closer to being a perfect approximation 
# as the sample size increases.p=0.5 and n=30 gives the best normal approximation among other options.
ps <- c(0.5,0.5,0.01,0.01)
ns <- c(5,30,30,100)
library(rafalib)
mypar(4,2)
for(i in 1:4){
  p <- ps[i]
  sides <- 1/p
  n <- ns[i]
  zs <- replicate(10000, {
    x <- sample(1:sides,n,replace=TRUE)
    (mean(x==1) - p) / sqrt(p * (1-p)/n)
  })
  hist(zs,nclass=7)
  qqnorm(zs)
  abline(0,1)
}


# Exercises 3
X <- filter(dat, Diet=="chow")%>%select(Bodyweight)%>%unlist
Y <- filter(dat, Diet=="hf")%>%select(Bodyweight)%>%unlist
mean(X)


# Exercises 6
sd(X)


# Exercises 7
2 * (1 - pnorm(2/sd(X) * sqrt(12)))


# Exercises 8
muX <- mean(X)
muY <- mean(Y)
M <- 12
N <- 12
varX <- sd(X)**2
varY <- sd(Y)**2
SE <- sqrt((varX)/M + (varY)/N)
SE


# Exercises 9
differenceYX <- muY - muX
tTest <- differenceYX / SE
tTest
t.test(X, Y)  # Calculates t-statistic, mean for X and Y, p-value, df, and 95% confidence interval.


# Exercises 11
pValue <- 2 * (1 - pnorm(tTest))
pValue


# Exercises 12
t.test(Y, X)  # p-value under the t-distribution approximation.


# Exercises 13
# With the CLT distribution, we obtained a p-value smaller than 0.05 and with the t-distribution, 
# one that is larger. They can't both be right. What best describes the difference?
  
  # These are two different assumptions. The t-distribution accounts for the variability 
  # introduced by the estimation of the standard error and thus, under the null, large values are 
  # more probable under the null distribution.

