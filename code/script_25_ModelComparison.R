# set up ------------------------------------------------------------------

rm(list = ls())
source(here::here("code/set_library.R"))


# Model Fit and Complexity --------------------------------------------------------

set.seed(1)

#hypothetical sample size
n <- 100
# true intercept = 0.1 and true slope = 0.5
b <- c(0.1, 0.5)

x1 <- rnorm(n = n, mean = 0, sd = 1)
# reorganize x1 in a matrix format
X <- model.matrix(~x1)

# y_hat is the expected value 
# %*% means matrix multiplication
y_hat <- X %*% b
#adding normal errors (or deviation from the mean)
y <- rnorm(n = n, mean = y_hat, sd = 0.5)

# plot
df0 <- tibble(y = y, x1 = x1)

df0 %>% 
  ggplot(aes(y = y,
             x = x1)) + 
  geom_point(color = "maroon")

# correct model used to generate the data
m1 <- lm(y ~ x1, data = df0)
(sm1 <- summary(m1))

# add a new variable, x2
df0 <- df0 %>% 
  mutate(x2 = rnorm(n = nrow(.),
                    mean = 0, sd = 1))

# add x2 to the model
m2 <- lm(y ~ x1 + x2, data = df0)
(sm2 <- summary(m2))


# Comparison Metrics ------------------------------------------------------

# comparing r squared
c(sm1$r.squared, sm2$r.squared)
# comparing adjusted r squared
c(sm1$adj.r.squared, sm2$adj.r.squared)


# likelihood ratio test ---------------------------------------------------

logLik(m1)
logLik(m2)

# test = "Chisq" specifies a chi-square distribution
# as a distribution of LR
anova(m1, m2, test = "Chisq") 
# because there was insignificance, this means that adding x2 to the model is useless


# AIC ---------------------------------------------------------------------

# good for prediction but bad for causation
# lower AIC is better. 
# AIC is good for feild data but bad for experimental data
AIC(m1)
#AIC: incorrect model 
AIC(m2)
