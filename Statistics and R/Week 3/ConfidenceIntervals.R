## Week 3 - Confidence Intervals

set.seed(1)
chowPopulation <- read.csv("femaleControlsPopulation.csv")
chowPopulation <- unlist(chowPopulation)

mu_chow <- mean(chowPopulation)
print(mu_chow)

N <- 3  # Also try for N <- 5
# When we try it with N=5, there will be a lot of red lines. There's much more than 5%, and that's because we didn't make 
# intervals big enough.So the Q we computed isn't right anymore. That Q was based on the CLT, and we said that it's a random
# variable.It's normally distributed, so we can compute the Q. We used qnorm as CLT tells us it's normal but it's not the
# case anymore, so these confidence intervals are not right.So now, instead of finding Q with qnorm, we can use the t
# distribution approximation. (Go to the comment with lines to see)
chow <- sample(chowPopulation, N)
print(mean(chow))


se <- sd(chow) / sqrt(N)
print(se)

(mean(chow) - mean(chowPopulation)) /se  # This value here is between -2 and 2, 95% of the time. 
pnorm(2) - pnorm(-2)  #95% confidence interval

Q <- qnorm(1 - 0.05/2)  
Q
-Q < (mean(chow) - mean(chowPopulation)) /se < Q  # 95% of the time. After simplifying the equation, we get the equation above.
interval <- c(mean(chow) - Q*se, mean(chow) + Q*se)
interval
interval[1] < mu_chow & interval[2] > mu_chow   # To check if it fells between that interval. So it checks if it's true or not.


library(rafalib)
B <- 250
mypar()
plot(mean(chowPopulation) + c(-7,7),c(1,1),type="n",xlab="weight",ylab="interval",ylim=c(1,B))
abline(v=mean(chowPopulation))

for(i in 1:B){
  chow <- sample(chowPopulation,N)
  se <- sd(chow) / sqrt(N)
  interval <- c(mean(chow) - Q*se, mean(chow) + Q*se)
  covered <- mean(chowPopulation)<=interval[2] & mean(chowPopulation)>=interval[1]
  color <- ifelse(covered,1,2)
  lines(interval, c(i,i), col=color)
}
# Red lines shows the ones they are not on top. So the 95% of the data (green lines) worked out pretty well.


# --------------------------------------------------------------------------------------- #

Q <- qt(1 - 0.05/2, df=4)
Q # We can see that it's bigger than 1.95 which is the value we get by using qnorm.
N <- 5
for(i in 1:B){
  chow <- sample(chowPopulation,N)
  se <- sd(chow) / sqrt(N)
  interval <- c(mean(chow) - Q*se, mean(chow) + Q*se)
  covered <- mean(chowPopulation)<=interval[2] & mean(chowPopulation)>=interval[1]
  color <- ifelse(covered,1,2)
  lines(interval, c(i,i), col=color)
}
# We see from the graph that we get bigger intervals and we get less red. So it's about 5% red now, as it should be.
# That's how the confidence intervals are constructed when we can't use CLT and instead we use the t distribution.

# Confidence Intervals Exercises
library(downloader)
url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/babies.txt"
filename <- basename(url)
download(url, destfile=filename)
babies <- read.table("babies.txt", header=TRUE)
babies

library(dplyr)
bwt.nonsmoke <- filter(babies, smoke==0) %>% select(bwt) %>% unlist 
bwt.smoke <- filter(babies, smoke==1) %>% select(bwt) %>% unlist

library(rafalib)
mean(bwt.nonsmoke)-mean(bwt.smoke)
popsd(bwt.nonsmoke)
popsd(bwt.smoke)


# Exercises 1
# If instead of CLT, we use the t-distribution approximation, what do we add and subtract to obtain a 99% confidence 
# interval (use 2*N-2 degrees of freedom)?
set.seed(1)
N <- 25
dat.ns <- sample(bwt.nonsmoke, N)
dat.s <- sample(bwt.smoke, N)
se <- sqrt(sd(dat.ns)**2/N + sd(dat.s)**2/N)
Q <- qt(1 - 0.01/2, df=2*(N-2))
Q*se


# Exercises 2


