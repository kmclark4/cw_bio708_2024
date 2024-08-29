

# setup -------------------------------------------------------------------

#refreshes environment
rm(list = ls())

# exercise 1 - vector -----------------------------------------------------

nv1 <- c(1, 3, 9)
nv1 #check work
nv2 <- seq(1, 100, length = 6)
nv2 #check work
nv3 <- rep(1,20)
nv3 #check work

cv1 <- c("a", "b", "c")
cv1 #check work
cv2 <- letters [1:6]
cv2 #check work
cv3 <- letters [1:20]
cv3 #check work

set.seed(1)
x <- rnorm(100)
which(x>2)
x[x>2]

# exercise 2 - matrix -----------------------------------------------------

nm1 <- cbind(rep(1,4), rep(2, 4), rep(3, 4), rep(4, 4))
nm1 #check work

nm2 <- matrix(rep(1:4, each = 4), nrow = 4, ncol = 4, byrow = TRUE)
nm2 #check work

cm1 <- matrix (rep(letters [1:4], each = 4), nrow = 4, ncol = 4)
cm1 #check work

cm2 <- cbind(rep("a",4), rep("b", 4), rep("c", 4), rep("d", 4))
cm2 #check work

set.seed(1)
x <- matrix(rnorm(100), nrow = 10, ncol = 10)
which(x>2)
x[x>2]

# exercise 3 - data frame -------------------------------------------------

x <- c(letters[1:10])
y <- c(1:10)
z <- c(11:20)
df1 <- data.frame(character = x, numerals.1 = y, numerals.2 = z)

df1 #check work

df1$character
df1$numerals.1
df1$numerals.2

set.seed(1)
x <- rnorm(100, mean = 10, sd = 3)
y <- rpois(100, lambda = 10)
z <- rep(c("VA", "NC"), 50)
df0 <- data.frame(temperature = x, abundance = y, state = z)
df0

v_va <- df0$temperature[df0$state=="VA"]
v_nc <- df0$temperature[df0$state=="NC"]
v_va
v_nc

mu_va <- mean (v_va)
mu_nc <- mean (v_nc)
mu_va
mu_nc

#extra exercises 
v_va <- with(df0, temperature [state=="VA"]) #with lets you get rid of "df0$" So you don't have to write the same thing over and over. 

v_mu <- tapply(df0$temperature, INDEX = df0$state, FUN = mean)
v_mu <- with(df0, tapply(temperature, INDEX = state, FUN = mean))
