## Week 3 - Association Tests

# Association Tests Exercises
d = read.csv("assoctest.csv")

# Exercises 1
tab <- table(d$allele, d$case)
chisq.test(tab) # Pearson's Chi-squared test with Yates' continuity correction

# Exercises 2
fisher.test(tab) # Fisher's exact test for count data


## Week 3 - Quiz
# Question 1
pval <- 1-(pnorm(abs(tval))-pnorm(-abs(tval)))
# Because of the symmetry of the standard normal distribution, there is a simpler way to calculate the probability that a t-value under the null
# could have a larger absolute value than tval. The simplified calculation is: 2*pnorm(-abs(tval))

