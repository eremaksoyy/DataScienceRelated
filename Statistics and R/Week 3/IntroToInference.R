## Week 3 - Inference I: P-values, Confidence Intervals and Power Calculations

library(dplyr)
library(downloader)
url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/babies.txt"
filename <- basename(url)
download(url, destfile=filename)
babies <- read.table("babies.txt", header=TRUE)
babies

bwt.nonsmoke <- filter(babies, smoke==0) %>% select(bwt) %>% unlist 
bwt.smoke <- filter(babies, smoke==1) %>% select(bwt) %>% unlist

library(rafalib)
mean(bwt.nonsmoke)-mean(bwt.smoke)
popsd(bwt.nonsmoke)
popsd(bwt.smoke)


## T-test Exercises 

# Exercises 1
set.seed(1)
dat.ns <- sample(bwt.nonsmoke,25)
dat.s <- sample(bwt.smoke,25)
difference <- mean(dat.ns) - mean(dat.s)
se <- sqrt((var(dat.ns)/25) + (var(dat.s)/25)) 
tval <- difference / se
tval


# Exercises 2
# The standard procedure is to examine the probability a t-statistic that actually does follow the 
# null hypothesis would have larger absolute value than the absolute value of the t-value we 
# just observed -- this is called a two-sided test.
pval <- 1-(pnorm(abs(tval))-pnorm(-abs(tval)))
pval



