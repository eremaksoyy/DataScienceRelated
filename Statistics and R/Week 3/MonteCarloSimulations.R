## Week 3 - Monte Carlo Simulations
ttestgenerator <- function(n){
  cases <- sample(bwt.nonsmoke, n) # We take samples from nonsmokers for cases and controls. It's the cases for the null is true, and it gives back the t-statistics.
  controls <- sample(bwt.nonsmoke, n)
  tstat <- (mean(cases) - mean(controls)) / sqrt(var(cases)/n + var(controls)/n)
  return (tstat)
}
ttests <- replicate(1000, ttestgenerator(10)) # A 1000 t-statistics with a sample size of 10.
# If we try to simulate with the sample size of 3, we could see that the approximation is not as good as like the one with a sample size of 10.
hist(ttests)
qqnorm(ttests)
abline(0, 1)

ps <- (seq(0,999)+0.5)/1000
qqplot(qt(ps, df=2*3-2), ttests, xlim=c(-6,6), ylim=c(-6,6))
abline(0, 1)

qqnorm(bwt.nonsmoke)
qqline(bwt.nonsmoke)

controls <- rnorm(5000, mean=24, sd=3.5)

ttestgenerator <- function(n, mean=24, sd=3.5){ # This is using a parametric random number generator to create the population data.
  cases <- rnorm(n, mean, sd)
  controls <- rnorm(n, mean, sd)
  tstat <- (mean(cases) - mean(controls)) / sqrt(var(cases)/n + var(controls)/n)
  return (tstat)
}

ttests <- replicate(1000, ttestgenerator(3))
qqnorm(ttests)
abline(0, 1)
## So here, we have two kinds of Monte Carlo simulations and they both are quite useful


# Exercises 1
set.seed(1)
N <- 5
X <- rnorm(N)
t <- sqrt(N)*mean(X) / sd(X)
t


# Exercises 2
set.seed(1)
N <- 5
B <- 1000

tstats <- replicate(B, {
  X <- rnorm(N)
  sqrt(N)*mean(X)/sd(X)
})
mean(tstats>2)
# The answer to this question is very similar to the theoretical prediction: 1-pt(2,df=4). We can check several such quantiles using the qqplot function.


# Exercises 3
library(rafalib)
mypar(3,2)

Ns<-seq(5,30,5)
B <- 1000
mypar(3,2)
LIM <- c(-4.5,4.5)
for(N in Ns){
  ts <- replicate(B, {
    X <- rnorm(N)
    sqrt(N)*mean(X)/sd(X)
  })
  ps <- seq(1/(B+1),1-1/(B+1),len=B)
  qqplot(qt(ps,df=N-1),ts,main=N,
         xlab="Theoretical",ylab="Observed",
         xlim=LIM, ylim=LIM)
  abline(0,1)
} 


# Exercises 4
Ns<-seq(5,30,5)
B <- 1000
mypar(3,2)
LIM <- c(-4.5,4.5)
for(N in Ns){
  ts <- replicate(B,{
    x <- rnorm(N)
    y <- rnorm(N)
    t.test(x,y, var.equal = TRUE)$stat
  })
  ps <- seq(1/(B+1),1-1/(B+1),len=B)
  qqplot(qt(ps,df=2*N-2),ts,main=N,
         xlab="Theoretical",ylab="Observed",
         xlim=LIM, ylim=LIM)
  abline(0,1)
}  


# Exercises 5
set.seed(1)
N <- 15
B <- 10000
tstats <- replicate(B,{
  X <- sample(c(-1,1), N, replace=TRUE)
  sqrt(N)*mean(X)/sd(X)
})
ps=seq(1/(B+1), 1-1/(B+1), len=B) 
qqplot(qt(ps,N-1), tstats, xlim=range(tstats))
abline(0,1)

# The population data is not normal thus the theory does not apply. We check with a Monte Carlo simulation. The qqplot shows a large tail. 
# Note that there is a small but positive chance that all the X are the same. In this case the denominator is 0 and the t-statistics is not defined.


# Exercises 6
set.seed(1)
N <- 1000
B <- 10000
tstats <- replicate(B,{
  X <-  sample(c(-1,1), N, replace=TRUE)
  sqrt(N)*mean(X)/sd(X)
})
qqnorm(tstats)
abline(0,1)
# With N=1000, CLT kicks in and the t-statistic is approximated with normal 0,1. Furthermore, t-distribution with df=999 and normal are practically the same.


# Exercises 7
set.seed(1)
Ns <- seq(5,45,5)
library(rafalib)
mypar(3,3)
for(N in Ns){
  medians <- replicate(10000, median ( rnorm(N) ) )
  title <- paste("N=",N,", avg=",round( mean(medians), 2) , ", sd*sqrt(N)=", round( sd(medians)*sqrt(N),2) )
  qqnorm(medians, main = title )
  qqline(medians)
}

# There is an asymptotic result that says SD is sqrt(N*4*dnorm(0)^2). The sample median is approximately normal with mean 0 and SD larger than 1/sqrt(N).

