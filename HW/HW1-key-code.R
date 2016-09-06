
  
# 1. (3pts) Define the pmf, find $\mu = E(Y)$, $\sigma^2 = Var(Y)$, and $\sigma = SD(Y)$.  Show all your work.
y <- 1:6
py <-  c(1/36,3/36,5/36,7/36,9/36,11/36)
mu <- sum(y*py)
sigma2 <- sum(y^2*py)-mu^2
sigma <- sqrt(sigma2)
mu
sigma2
sigma

#2
mydata <- data.frame(yvals = y, probs = py)
ggplot(aes(x=yvals,y=probs),data=mydata) + geom_bar(stat='identity') + xlab('y') + ylab('P(Y=y)')

#3  Consider the random variable $Z=2Y+1$.  What is $E(Z)$ and $Var(Z)$?  Show all work. 

Ez <- 2*mu + 1
Ez
VarZ <- 2^2*sigma2
VarZ


one.Y <- function() {
 ss <- 1:6
 red <- sample(ss,1)
 white <- sample(ss,1)
 y <- max(red,white)
 return(y)
}
##Trying it out; each run of this should yield a simulated maximum:
one.Y()
one.Y()



# 4. (2pts) Use `replicate()` to simulate the results of 1000 pairs of rolls.  These are 1000 realizations of the random variable $Y$. Save the 1000 realizations in an object called `many.Y`.
manyY <- replicate(1000,one.Y())
#5.  (2pts) Use `ggplot()` to create the empirical (i.e., observed) pmf of your simulation.  See Handout 1 for example R code.  How does it compare with your theoretical pmf?
dd <- data.frame(y <- as.factor(manyY))
ggplot(aes(x=y),data=dd) + geom_bar(aes(y=..count../(sum(..count..)))) +
  ylab('P(y)')
#6. (1pt)  What is the mean of the 1000 realizations?
mean(manyY)
#7.(1pt)  What is the variance of the 1000 realizations?
var(manyY)
#8.  (1pt) What is the standard deviation of the 1000 realizations?
sd(manyY)
#9. (1pt)  Create a new object called `many.Z` that creates 1000 realizations of $Z$.
many.Z <- 2*manyY +1
#10. (1pt)  What is the mean of $Z$?
mean(many.Z)
#11. (1pt)  What is the variance of $Z$?
var(many.Z)
#12. (1pt) Note that your simulated results should be similar to the theoretical quantities; 
#if they aren't, re-check your R code!  What is the reason for any differences?

#The reason for the difference is that simulated quantities will only be approximately equal to theoretical quantities,
#due to the finite nature of any sample.  If we had an infinite, instead of 1000, realizations of Y and Z,
# the theoretical and simulated quantities would be identical (but of course such a simulation is impossible to carry out)




