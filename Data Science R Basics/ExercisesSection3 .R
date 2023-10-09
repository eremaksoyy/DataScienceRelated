
# Session 3 Assessment

library(dslabs)
data(heights)
options(digits = 3)    # report 3 significant digits for all answers

# Question 1
avg_height <- mean(heights$height)
ind <- heights$height > avg_height
ind
sum(ind)

# Question 2
females <- (heights$height > avg_height) & heights$sex=="Female"
sum(females)

# Question 3 
mean(heights$sex=="Female")

# Question 4
min_height <- min(heights$height)
min_height
index <- match(min_height, heights$height)
index
subset <- heights$sex[index]
subset

# Question 5
max_height <- max(heights$height)
max_height
x <- as.integer(max_height): as.integer(min_height)

sum(! x%in% heights$height)

# Question 6
library(dplyr)
heights2 <- mutate(heights, ht_cm=heights$height*2.54)
heights2$ht_cm[18]
ht_cm_values <- heights2$ht_cm
mean(ht_cm_values)

# Question 7
females <- filter(heights2, heights2$sex=="Female")
females
nrow(females)
mean(females$ht_cm)

# Question 8
library(dslabs)
data(olive)
head(olive)

plot(olive$palmitic, olive$palmitoleic)

# Question 9 
hist(olive$eicosenoic)

# Question 10
boxplot(palmitic~region,data=olive)
which.max(median(olive$palmitic))
olive$region[1]
values <- olive %>% select(region, palmitic)
south <- sum(olive$palmitic & olive$region=="Southern Italy")
north <- sum(olive$palmitic & olive$region=="Northern Italy")
sardinia <- sum(olive$palmitic & olive$region=="Sardinia")
values <- c(south, north, sardinia)
which.max(values)

