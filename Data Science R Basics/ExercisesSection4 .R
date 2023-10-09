
# Session 4 Assessment

# Question 1
library(dslabs)
data(heights)
sum(ifelse(heights$sex=='Female', 1, 2))

# Question 2 
mean(ifelse(heights$height>72, heights$height, 0))

# Question 3
inches_to_ft <- function(x){
  no <- x/12
  no
}
inches_to_ft(144)
sum(inches_to_ft(heights$height)<5)

# Question 4
any(TRUE, TRUE, TRUE)
any(TRUE, TRUE, FALSE)
any(TRUE, FALSE, FALSE)
all(TRUE, TRUE, TRUE)

# Question 5
m <- 10
f_n <- vector(length = m)
for (n in 1:m){
  f_n[n] <- factorial(n)
}
f_n


