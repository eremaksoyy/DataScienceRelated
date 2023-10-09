## Week 2 - Normal Distribution Exercises

library(downloader) 
url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/femaleControlsPopulation.csv"
filename <- basename(url)
download(url, destfile=filename)
x <- unlist( read.csv(filename) ) # x here represents the weights for the entire population.

set.seed(1)
n <- 1000
averages5 <- vector("numeric",n)
for(i in 1:n){
  X <- sample(x,5)
  averages5[i] <- mean(X)
}


## Exercises 1
hist(X)


## Exercises 2
set.seed(1)
n <- 1000
averages50 <- vector("numeric",n)
for(i in 1:n){
  X <- sample(x,50)
  averages50[i] <- mean(X)
}
mean(averages50 < 25 & averages50 > 23)
hist(X)


## Exercises 3
# The proportion of observations below a cutoff x given a normal dist with mean mu and standard dev sigma with pnorm(x, mu, sigma) or pnorm((x-mu)/sigma).
# The question is asking for the proportion of observations of between 23 and 25 in a normal dist with average 23.9 and standard dev 0.43 .
a <- pnorm(23, 23.9, 0.43)  
b <- pnorm(25, 23.9, 0.43)
b-a
