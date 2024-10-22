
# set up ------------------------------------------------------------------

rm(list = ls())
library(tidyverse)


# regression --------------------------------------------------------------

df0 <- read_csv("data_raw/data_algae.csv")

df0 %>% 
  ggplot(aes(x = conductivity,
             y = biomass)) +
  geom_point(color = "maroon")

##linear formula
m <- lm(biomass ~ conductivity,
        data = df0)
summary(m)

alpha <- coef(m)[1]
beta <- coef(m)[2]

df0 %>% 
  ggplot(aes(x = conductivity,
             y = biomass)) +
  geom_point( color = "maroon") +
  geom_abline(intercept = alpha,
              slope = beta, color = "forestgreen")

## Least squares

v_x <- df0 %>% pull(conductivity)
X <- cbind(1, v_x)


Y <- df0 %>% pull(biomass)

theta <- solve(t(X) %*% X) %*% t(X) %*% Y
print(theta)

m <- lm(biomass ~ conductivity, data = df0)
coef(m)

## Standard error amd t value

theta <- coef(m)


se <- sqrt(diag(vcov(m)))


t_value <- theta / se
print(t_value)


p_alpha <- (1 - pt(t_value[1], df = 48)) + pt(-t_value[1], df = 48)

p_beta <- (1 - pt(t_value[2], df = 48)) + pt(-t_value[2], df = 48)

print(p_alpha)
print(p_beta)

## unexplained variation
eps <- resid(m)
head(eps)

v_x <- df0 %>% pull(conductivity)
v_y <- df0 %>% pull(biomass)

error <- v_y - (theta[1] + theta[2] * v_x)

head(cbind(eps, error))

eps <- round(eps, 5)
error <- round(error, 5)

all(eps == error)

## Visual errors
df0 <- df0 %>% 
  mutate(eps = eps)

df0 %>% 
  ggplot(aes(x = conductivity,
             y = biomass)) +
  geom_point(color = "black") +
  geom_abline(intercept = alpha,
              slope = beta, color = "skyblue") + 
  geom_segment(aes(x = conductivity, # start-coord x
                   xend = conductivity, # end-coord x
                   y = biomass, # start-coord y
                   yend = biomass - eps), # end-coord y
               linetype = "dashed", color = "maroon")

## Coeffiecien of determination
ss <- sum(resid(m)^2)
ss_0 <-sum((v_y - mean(v_y))^2)
r2 <- 1 - ss / ss_0
print(r2)

summary(m)
