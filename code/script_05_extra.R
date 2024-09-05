
# setup -------------------------------------------------------------------

rm(list = ls())

library(tidyverse)

iris <- as_tibble(iris)


#function ---------------------------------------------------------------------

## function () is to create a new function

x <- rnorm(100, mean = 10, sd = 100)
y <- rnorm(100, mean = 10, sd = 100)
z <- rnorm(100, mean = 10, sd = 100)
hist(x)
mean(x)
sd(x) / mean(x) #coeffieicnt variation

## without functiom
sig <- sd(x)
mu <- mean(x)
cv <- sig / mu
print(cv)

#with function <- helps reduce error from copy and paste
fun_cv <- function (x) {
  cv <- sd(x) / mean(x)
  return(cv)
}

fun_cv(x)
fun_cv(y)
fun_cv(z)

## standardization 
x0 <- x - mean(x) #centering
z <- x0 / sd(x0) #scaling
print(z)
mean(x0)
sd(x0)
sd(z)

scl <- function(k) {
  z <- (k - mean(k)) / sd(k)
  return(z)
}
z <- scl(k=x) #x came from above
mean(z)
sd(z)

## random function with two arguments 

f0 <- function (phi, zeta) {
  cout <- 2 * phi + rnorm(1) * zeta
  return(cout)
}
f0(phi = 2, zeta = 3)


# apply family ------------------------------------------------------------

m <- matrix(rnorm(25), nrow = 5, ncol = 5)

## apply() <- more matrix, mainly
apply(m, MARGIN = 1, FUN = mean) #FUN means function this is mean for each row = MARGIN 1
apply(m, MARGIN = 2, FUN = mean) # (MARGIN 2 = mean for each column

# FUN can be a function that you defined
apply(m, MARGIN = 1, FUN = fun_cv)

## for datframe
apply(iris %>% select (1:4), 
     MARGIN = 2, FUN = mean)

## sapply () <- for list
x <- rnorm(10)
y <- rnorm(100)
z <- rnorm(5)

l_xyz <- list(x, y, z)
l_xyz

sapply(l_xyz, FUN = mean)
#checking work
mean(x) 
mean(y) 
mean(z)

## lapply() <- for list but output is list too
x <- rpois(10, lambda = 5)
y <- rpois(10, lambda = 5)
z <- rep(letters[1:3], 10)

## create a list of xyz
l_xyz <- list(x, y, z)
 
## re,pve duplicates, or get a unique element
unique(x)
unique(y)
unique(z)

lapply(l_xyz, FUN = unique)

## try to get only the first element from each vector. Do this by defining a new function
# 1st eample
lapply(l_xyz, 
       FUN = function(x) {
         x[1]
       })

## 2nd example 
first <- function(x) {
  return(x[1])
}

lapply(l_xyz, FUN = first)


# for loop ----------------------------------------------------------------

## for loop is to repeat work inside {}
# i is the most common index

##multiple for 2 for each element
x <- seq(0, 10, by = 0.25)
y <- NULL
for (i in 1:10) {
  y[i] <- 2 * x[i] # i is used to repeat 1-10
}

#only multiplies by 2 for the last number. 
y <-NULL
for (i in 1:10) {
  y <- 2 * x[i] # i is used to repeat 1-10
}

