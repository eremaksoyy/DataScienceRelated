## Week 2 - Week 2 Quiz

# To be sure that you are using the correct random number generator (RNG) settings:
RNGkind("Mersenne-Twister", "Inversion", "Rejection") 


# Question 1
library(downloader) 
url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/femaleControlsPopulation.csv"
filename <- basename(url)
download(url, destfile=filename)
x <- unlist( read.csv(filename) )
set.seed(1)
n <- 1000
avgs <- vector("numeric", n)
differences <- vector("numeric",n)
for(i in 1:n){
  samples <- sample(x,50)
  avgs[i] <- mean(samples)
  differences[i] <- avgs[i] - mean(x)
}
mean(abs(differences)>1)


# Question 2
library(dplyr)
library(gapminder)
data(gapminder)
head(gapminder)
LifeExp <- filter(gapminder,year==1952)%>%select(lifeExp)%>%unlist
mean(LifeExp>40 & LifeExp<60)


# Question 3
# When we examined mouse weights, we found that our sample estimates for females were closer to the population 
# difference than with males. What is a possible explanation for this?
  
  # The population variance of the females is smaller than that of the males; thus, the sample variable has less variability.


# Question 4
library(dplyr)
dat <- read.csv("mice_pheno.csv")
dat <- na.omit(dat)
mypar(2,2)
y <- filter(dat, Sex=='M' & Diet=="chow") %>% select(Bodyweight) %>% unlist
z <- ( y - mean(y) ) / popsd(y)
qqnorm(z)
abline(0,1)
y <- filter(dat, Sex=="F" & Diet=="chow") %>% select(Bodyweight) %>% unlist
z <- ( y - mean(y) ) / popsd(y)
qqnorm(z);abline(0,1)
y <- filter(dat, Sex=="M" & Diet=="hf") %>% select(Bodyweight) %>% unlist
z <- ( y - mean(y) ) / popsd(y)
qqnorm(z);abline(0,1)
y <- filter(dat, Sex=="F" & Diet=="hf") %>% select(Bodyweight) %>% unlist
z <- ( y - mean(y) ) / popsd(y)
qqnorm(z);abline(0,1)


