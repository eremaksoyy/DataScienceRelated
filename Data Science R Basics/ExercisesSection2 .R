
# Session 2 Assessment

# Question 1
x <- c(2, 43, 27, 96, 18)
sort(x)
order(x)
rank(x)

# Question 2
min(x)
which.min(x)
max(x)
which.max(x)

# Question 3
name <- c("Mandi", "Amy", "Nicole", "Olivia")
distance <- c(0.8, 3.1, 2.8, 4.0)
time <- c(10, 30, 40, 50)

hours <- time/60
speed <- distance/hours
hours[4] # To calculate how many hours Olivia ran
speed[1]
speed[3]
which.max(speed)
