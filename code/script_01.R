

# test codes --------------------------------------------------------------

c(1,2)

## produce 100 random numbers that follows a normal distribution
x <- rnorm(n = 100, mean = 0, sd = 1)

## estimate mean
mean(x)

## estimate SD
sd(x)


# vector exercise ---------------------------------------------------------

# ex.1a manually create a vector using c()
x <- c(1,3,4,8)
x

# ex.1b character
x <- c("a", "b", "c")
x

# ex.1c logical
x <- c(TRUE, FALSE, FALSE)
x

## [1] TRUE FALSE FALSE

# ex.2 sequence of numbers 
x <- 1:5
x

## [1] 1 2 3 4 5

# ex.3a replicate same numbers or characters

x <- rep(2, 5) # replicate 2 five times
x

# ex.3b replicate same numbers or characters
x <- rep("a", 5) # replicate "a" five times
x

# ex.4a use seq() function
x <- seq(1, 5, by = 1)
x

#ex.4b use seq() function
x <- seq(1, 5, by = 0.1)
x

#ex.4c use seq() function
x <- seq(1, 5, length = 7)
x

## check features -- numeric vectors

x <- c(1.2, 3.1, 4.0, 8.2)
x

class(x)
typeof(x)
length(x)
sum(x)
mean(x)

y <- c(1L, 2L) #L after number means integer

class(y)
typeof(y)
length(y)
sum(y)
mean(y)

## element ID

x <- c(2,2,3,2,5)
x[2] # access element #2

x[c(2,4)] # access elements #2 and 4

x[2:4] # access elements #2-4


# matrix ------------------------------------------------------------------


# ex.1 cbind: combine objects by column
x <- cbind(c(1,2,3), c(4,5,6))
x

# ex.2 rbind: combine objects by row
x <- rbind(c(1,2,3), c(4,5,6))
x

# ex.3 matrix: specify elements and the number of rows (nrow) and columns (ncol)
x <- matrix(1:9, nrow = 3, ncol = 3)
x

## Check features

#integer matrix

x <- matrix(1:9, nrow = 3, ncol = 3)
x

class(x)
typeof(x)
dim(x)
rowSums(x)
colSums(x)

#access
x[1, 2] #specific element in row 1 second column


# dataframe ---------------------------------------------------------------


# Create data frame
x <- c("Pristine", "Pristine", "Disturbed", "Disturbed", "Pristine") # Lake type
y <- c(1.2, 2.2, 10.9, 50.0, 3.0) # TSS: total suspended solids (mg/L)
df0 <- data.frame(LakeType = x, TSS = y) # x is named as "LakeType" while y is named as "TSS"
df0

# call column names
colnames(df0) # call column names

#Access by columns
df0$LakeType # access LakeType
df0$TSS #access TSS

df0[,1] # access column #1
df0[1,] # access row #1
df0[c(2,4),] # access row #2 and 4
