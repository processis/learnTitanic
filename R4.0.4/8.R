# The mean of a vector:
x <- c(1,2,3,4,5,60)
mean(x)

# Missing values will block the override the result:
x <- c(1,2,3,4,5,60,NA)
mean(x)

# Missing values can be ignored with na.rm = TRUE:
mean(x, na.rm = TRUE)

# This works also for a matrix:
M <- matrix(c(1,2,3,4,5,60), nrow=3)
mean(M)

v <- c(1,2,3,4,5,6000)
mean(v)

mean(v, trim = 0.2)

returns <- c(0.5,-0.5,0.5,-0.5)
# Arithmetic mean:
aritmean <- mean(returns)
# The ln-mean:
log_returns <- returns
for(k in 1:length(returns)) {
  log_returns[k] <- log( returns[k] + 1)
}
logmean <- mean(log_returns)
exp(logmean) - 1

# What is the value of the investment after these returns:
V_0 <- 1
V_T <- V_0
for(k in 1:length(returns)) {
  V_T <- V_T * (returns[k] + 1)
}
V_T

# Compare this to our predictions:

V_0 * (exp(logmean) - 1)

k
k

V_0 * (aritmean + 1)

#8.1.2The Median
x <- c(1:5,5e10,NA)
x

median(x)

median(x,na.rm = TRUE)
# Note how the median is not impacted by the outlier,
# but the outlier dominates the mean:
mean(x, na.rm = TRUE)

#8.1.3mode

# my_mode
# Finds the first mode (only one)
# Arguments:
#v -- numeric vector or factor
# Returns:
#the first mode
my_mode <- function(v) {
  uniqv <- unique(v)
  tabv <- tabulate(match(v, uniqv))
  uniqv[which.max(tabv)]
}

# now test this function
x <- c(1,2,3,3,4,5,60,NA)
my_mode(x)

x1 <- c("relevant", "N/A", "undesired", "great", "N/A",
        "undesired", "great", "great")
my_mode(x1)
## [1] "great"
# text from https://www.r-project.org/about.html
t <- "R is available as Free Software under the terms of the
Free Software Foundation's GNU General Public License in
source code form. It compiles and runs on a wide variety of
UNIX platforms and similar systems (including FreeBSD and
Linux), Windows and MacOS."
v <- unlist(strsplit(t,split=" "))
my_mode(v)


# my_mode
# Finds the mode(s) of a vector v
# Arguments:
#v -- numeric vector or factor
#return.all -- boolean -- set to true to return all modes
# Returns:
#the modal elements
my_mode <- function(v, return.all = FALSE) {
  uniqv <- unique(v)
  tabv<- tabulate(match(v, uniqv))
  if (return.all) {
    uniqv[tabv == max(tabv)]
  } else {
    uniqv[which.max(tabv)]
  }
}
# example:
x <- c(1,2,2,3,3,4,5)
my_mode(x)

my_mode(x, return.all = TRUE)

#8.2 Measures of Variation or Spread
t <- rnorm(100, mean=0, sd=20)
var(t)

sd(t)


sqrt(var(t))

sqrt(sum((t - mean(t))^2)/(length(t) - 1))

mad(t)

mad(t,constant=1)

#8.3 Measures of Covariation
cor(mtcars$hp,mtcars$wt)

d <- data.frame(mpg = mtcars$mpg, wt = mtcars$wt, hp = mtcars$hp)
# Note that we can feed a whole data-frame in the functions.
var(d)
cov(d)
cor(d)
cov2cor(cov(d))

x <- c(-10:10)
df <- data.frame(x=x, x_sq=x^2, x_abs=abs(x), x_exp=exp(x))
cor(df)
cor(rank(df$x), rank(df$x_exp))

x <- c(-10:10)
cor(rank(x), rank(x^2))

# we use the dataset mtcars from MASS
df <- data.frame(mtcars$cyl,mtcars$am)
chisq.test(df)

#8.4 Distributions
obs <- rnorm(600,10,3)
hist(obs,col="khaki3",freq=FALSE)
x <- seq(from=0,to=20,by=0.001)
lines(x, dnorm(x,10,3),col="blue",lwd=4)

library(MASS)
hist(SP500,col="khaki3",freq=FALSE,border="khaki3")
x <- seq(from=-5,to=5,by=0.001)
lines(x, dnorm(x,mean(SP500),sd(SP500)),col="blue",lwd=2)

library(MASS)
qqnorm(SP500,col="red"); qqline(SP500,col="blue")

pbinom(5,10,0.5)
# visualize this for one to 10 numbers of tosses
x <- 1:10
y <- pbinom(x,10,0.5)
plot(x,y,type="b",col="blue", lwd=3,
     xlab="Number of tails",
     ylab="prob of maxium x tails",
     main="Ten tosses of a coin")

qbinom(0.25,10,1/2)

rbinom(20,10,.5)