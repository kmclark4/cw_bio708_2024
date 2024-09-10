
# set up ------------------------------------------------------------------

rm(list = ls())
library(tidyverse)

# central tenancy -------------------------------------------------------------------------

# construct vectors x and y
x <- c(15.9, 15.1, 21.9, 13.3, 24.4)
y <- c(15.9, 15.1, 21.9, 53.3, 24.4)


# calculate arithematic mean for x and y
sum_x <- sum(x)
mu_x <- sum_x / length(x)
print(mu_x) 

mean(x)

# for vector y; we can calculate directly too
mu_y <- sum(y) / length(y)
print(mu_y) 

mean(y)

## Geometric Mean always smaller than arithematic mean. 
#less influenced by outliers. 

# for vector x

# prod() multiply all elements in a vector
prod(x)^(1 / length(x)) 


# for vector y
prod(y)^(1 / length(y))

## or mean in a log scale then transform back to an ordinary scale
log_y <- log(y)
exp(mean(log_y))

## Median

# for vector x
median(x)

# median with out median function

x <- sort(x) # sort x from small to large
index <- (length(x) + 1) / 2 # (N + 1)/2 th index as length(x) is an odd number
med_x <- x[index]
print(med_x)

# for vector y
y <- sort(y) # sort y from small to large
index <- (length (x) + 1) /2
med_y <- y[index]
print(med_y)

median(y)


# Variation ---------------------------------------------------------------

## calculate variance for x and y
## manual calulation
## use sum(), length(), ^, and sqrt()

# variance for x
(x - mean(x))^2 
sum((x - mean(x))^2)
sig2_x <- sum((x - mean(x))^2) / length(x)

# standard deviation 
sqrt(sig2_x)

# for y
sig2_y <- sum((y - mean(y))^2) / length(y)


# standard deviation
sqrt(sig2_y)


## Inter-Quantile Range (IQR)
## quantile() 

x25 <- quantile(x, 0.25)
x25
x75 <- quantile(x, 0.75)
x75
iqr_x <- abs(x25 - x75) #abs stands for absolute value
iqr_x

yq <- quantile(y, c(0.25, 0.75))
iqr_y <- abs(yq[1] - yq[2]) # yq[1] = 0.25; yq[2] = 0.75 
iqr_y


# relative variance -------------------------------------------------------

## coefficient of variation (CV)
# cv for x and y
## sd over mean 

(cv_x <- sqrt(sig2_x) / mean(x)) # putting the whole code in () prints the value


(cv_y <- sqrt(sig2_y / mean(y)))

##IQR / median
abs(diff(quantile(x, c(0.25, 0.75)) / median(x)))
abs(diff(quantile(y, c(0.25, 0.75)) / median(y)))

