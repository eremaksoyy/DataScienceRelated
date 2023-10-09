## Week 2 - Probability Distributions Exercises

install.packages("gapminder")
library(gapminder)
data("gapminder")
head(gapminder)
class(gapminder)
library(dplyr) 
x <- filter(gapminder,year==1952) %>% select(lifeExp) %>% unlist
hist(x)


## Exercises 1
mean(x<=40)

plot(ecdf(x)) #It's a built in function that gives an empirical cumulative distribution function graph for this particular list.

prop = function(q){   #Building a function that does the same with the built in function above.
  mean(x<=q)
}
prop(40)


## Exercises 2
qs = seq(from=min(x), to=max(x), length=20) #Building a range of qs that we can apply the function to.
qs
props = sapply(qs, prop)
plot(qs, props)

# Or writing all the code lines above in one line (by defining the prop function inside of sapply() but without naming it):
props = sapply(qs, function(q) mean(x<=q)) # This style is called using an "inline" function or an "anonymous" function.
plot(ecdf(x))
