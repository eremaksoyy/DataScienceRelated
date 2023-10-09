
# Section 1 Assessment

# Question 1
solutions <- function(a,b,c){
  root1 <- (-b+(sqrt(b**2-4*a*c)))/(2*a)
  root2 <- (-b-(sqrt(b**2-4*a*c)))/(2*a)
  roots <- c(root1, root2)
  return (roots)
}

solutions(2,-1,-4)


# Question 2
log2(1024)/2


# Question 3
install.packages("dslabs")
library(dslabs)
data(movielens)

nrow(movielens)
length(movielens)
class(movielens$title)
class(movielens$genres)


# Question 4
nlevels(movielens$genres)
