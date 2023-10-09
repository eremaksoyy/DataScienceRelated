## Week 2 - T-tests in Practice

library(downloader) 
url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/femaleMiceWeights.csv"
filename <- basename(url)
download(url, destfile=filename)

library(dplyr)
dat <- read.csv("femaleMiceWeights.csv")
control <- filter(dat, Diet=="chow") %>% select(Bodyweight)%>%unlist
treatment <- filter(dat, Diet=="hf")%>%select(Bodyweight)%>%unlist

N <- length(treatment)
obs <- mean(treatment) - mean(control)
se <- sqrt( var(treatment)/N + var(control)/N ) # We need to estimate the standard error here.

tstat <- obs / se
1 - pnorm(tstat)  # p-value 
2*(1 - pnorm(tstat))

library(downloader) 
url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/femaleControlsPopulation.csv"
filename <- basename(url)
download(url, destfile=filename)
population <- read.csv("femaleControlsPopulation.csv")
population <- unlist(population)


# t-tests in practice I
n <- 10000
nulls <- vector("numeric", n)
for(i in 1:n){
  control <- sample(population, N)
  treatment <- sample(population, N)
  se <- sqrt( var(treatment)/N + var(control)/N ) # We need to estimate the standard error here.
  nulls[i] <- (mean(treatment) - mean(control)) / se
}

library(rafalib)
mypar()
qqnorm(nulls)
abline(0,1) # Meaning that make a line with intercept 0 and slope 1, and it should go right through the data.
            # And as it's pretty close, telling us that the CLT here is, in fact, giving us a pretty good approximation.
            # So the p-value we obtained is a good approximation of the actual p-value.
qqline(nulls)
# When we change N value to 3, instead of 12, the plot will also change and we'll see that in this case, 
# the normal approximation is not good at all with this particular data set. If the original data follows a normal distribution, 
# then in fact, the t-approximation should work.


## t-tests in practice II
library(dplyr)
dat <- read.csv("femaleMiceWeights.csv")
control <- filter(dat, Diet=="chow") %>% select(Bodyweight)%>%unlist
treatment <- filter(dat, Diet=="hf")%>%select(Bodyweight)%>%unlist

ttest <- t.test(treatment,control)
ttest

qqnorm(control)
qqline(control)
qqnorm(treatment)
qqline(treatment)

