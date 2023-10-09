#Power Calculations Exercises
library(dplyr)
bwt.nonsmoke <- filter(babies, smoke==0) %>% select(bwt) %>% unlist 
bwt.smoke <- filter(babies, smoke==1) %>% select(bwt) %>% unlist

mu_nonsmoke <- mean(bwt.nonsmoke)
mu_smoke <- mean(bwt.smoke)
print(mu_nonsmoke - mu_smoke)
print((mu_nonsmoke - mu_smoke) / mu_smoke * 100) # Percent increase

set.seed(1)
N <- 5
dat.ns <- sample(bwt.nonsmoke, N)
dat.s <- sample(bwt.smoke, N)
t.test(dat.ns, dat.s)$p.value # We get this value by using the cut-offs, and we don't reject.We say that there is not enough evidence 
# here to reject the null and we miss this result. We don't find that they're different. We call it Type II error.
# There is a difference but we fail to reject. So it'S a mistake but it happens because it's random data. Our sample size is too small.
# We get a value of 0.1843 here
N <- 12 # We get a bigger sample size this time
dat.ns <- sample(bwt.nonsmoke, N)
dat.s <- sample(bwt.smoke, N)
t.test(dat.ns, dat.s)$p.value # Now the value becomes 0.00301

alpha <- 0.05
# It's possible to make a Type II error and it's quite possible to make it when we have a small sample size, and power helps us quantify that.

B <- 2000

reject <- function(N, alpha=0.05){
  dat.ns <- sample(bwt.nonsmoke, N)
  dat.s <- sample(bwt.smoke, N)
  pval <- t.test(dat.ns, dat.s)$p.value
  pval < alpha
}

reject(12)
rejections <- replicate(B, reject(N))
mean(rejections) # This is power. It's the probability of rejecting the hypothesis when the alternative is true. It's 22% in this case, which is considered not that high.
# If we want to have more power, we would have selected a larger N.

Ns <- seq(5, 50, 5)
power <- sapply(Ns, function(N){
  rejections <- replicate(B, reject(N))
  mean(rejections)
})

plot(Ns, power, type="b")

N <- 30 # Try with a different sample size as well, N=30 in this case


# Exercises 1
set.seed(1)
N <- 5
dat.ns <- sample(bwt.nonsmoke, N)
dat.s <- sample(bwt.smoke, N)
t.test(dat.ns, dat.s)$p.value


#Exercises 2
set.seed(1)
alpha <- 0.05
B <- 10000
N <- 120
rejects <- replicate(B, {
  dat.ns <- sample(bwt.nonsmoke, N)
  dat.s <- sample(bwt.smoke, N)
  t.test(dat.s, dat.ns)$p.value < alpha
})
mean(rejects)


# Exercises 3
Ns = c(30, 60, 90, 120)
res <- sapply(Ns, function(N){
  set.seed(1)
  rejects <- replicate(B, {
    dat.ns <- sample(bwt.nonsmoke, N)
    dat.s <- sample(bwt.smoke, N)
    t.test(dat.s, dat.ns)$p.value < alpha
  })
  mean(rejects)
})
Ns[which.min(abs(res - .8))]


# Exercises 4
alpha <- 0.01
Ns = c(30, 60, 90, 120)
res <- sapply(Ns, function(N){
  set.seed(1)
  rejects <- replicate(B, {
    dat.ns <- sample(bwt.nonsmoke, N)
    dat.s <- sample(bwt.smoke, N)
    t.test(dat.s, dat.ns)$p.value < alpha
  })
  mean(rejects)
})
Ns[which.min(abs(res - .8))]
