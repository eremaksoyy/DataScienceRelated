## Week 2 - Population, Samples, and Estimates Exercises

library(downloader)
url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/mice_pheno.csv"
filename <- basename(url)
download(url, destfile=filename)
dat <- read.csv(filename) 
dat <- na.omit(dat) #To remove the lines that contain missing values.
dat

library(dplyr)
x <- filter(dat, Diet=="chow" & Sex=='M')%>%select(Bodyweight)%>%unlist
x

## Exercises 1
a <- mean(x)


## Exercises 2
library(rafalib)  # Use the rafalib package and 
popsd(x)           # the popsd() function to compute the population standard deviation.


## Exercises 3
set.seed(1)
X <- sample(x, 25)  # Taking a random sample X of size 25 from x.
A <- mean(X)


## Exercises 4
y <- filter(dat, Diet=="hf" & Sex=='M')%>%select(Bodyweight)%>%unlist
y
b <- mean(y)


## Exercises 5
popsd(y)


## Exercises 6
set.seed(1)
Y <- sample(y, 25)
B <- mean(Y)


## Exercises 7
c <- b-a
C <- B-A
c-C


## Exercises 8
x <- filter(dat, Diet=="chow" & Sex=='F')%>%select(Bodyweight)%>%unlist
a <- mean(x)
set.seed(2)
X <- sample(x, 25)  # Taking a random sample X of size 25 from x.
A <- mean(X)
y <- filter(dat, Diet=="hf" & Sex=='F')%>%select(Bodyweight)%>%unlist
b <- mean(y)
set.seed(2)
Y <- sample(y, 25)
B <- mean(Y)
c <- b-a
C <- B-A
c-C

