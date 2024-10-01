# set up ------------------------------------------------------------------

rm(list = ls())
library(tidyverse)


# Exercise 1 --------------------------------------------------------------


set.seed(8)
x <- rnorm(50)
df1 <- as_tibble(x)

mu <- mean(df1$value)
sigma <- sd(df1$value)
pd <- dnorm(x, mean = mu, sd = sigma)


tibble(y = pd, x = x) %>% 
  ggplot(aes(x = x, y = y)) +
  geom_line() 


x_min <- floor(min(df1$value))
x_max <- ceiling(max(df1$value))
bin <- seq(x_min, x_max, by = 1)

p <- NULL 
for (i in 1:(length(bin) - 1)) {
  p[i] <- pnorm(bin[i+1], mean = mu, sd = sigma) - pnorm(bin[i], mean = mu, sd = sigma)
}
  
df_prob <- tibble(p = p,
                  bin = bin [-length(bin)] + 0.5) %>% 
  mutate(freq = p * nrow(df1))

df1 %>% 
  ggplot(aes(x = value)) + 
  geom_histogram(binwidth = 1, 
                 center = 0.5, fill = "black") + 
  geom_point(data = df_prob,
             aes(y = freq,
                 x = bin),
             color = "skyblue") +
  geom_line(data = df_prob,
            aes(y = freq,
                x = bin),
            color = "skyblue")


# Exercise 2 --------------------------------------------------------------

set.seed(5)
z <- rpois(1000, lambda = 20)
df0 <- as_tibble(z)
lambda_hat <- (20)
z_min <- min(z)
z_max <- max(z)
bin <- seq(z_min, z_max, by = 1)



df0 %>% 
  ggplot(aes(x = value)) +
  geom_histogram(binwidth = 0.5, # define binwidth
                 center = 0) # relative position of each bin
pm <- dpois(z, lambda = lambda_hat)

df_prob <- tibble(x = z, y = pm) %>% 
  mutate(freq = y * nrow(df0))

df0 %>% 
  ggplot(aes(x = value)) +
  geom_histogram(binwidth = 0.5, # must be divisible number of one; e.g., 0.1, 0.25, 0.5...
                 center = 0) +
  geom_line(data = df_prob,
            aes(x = x,
                y = freq),
            linetype = "dashed") +
  geom_point(data = df_prob,
             aes(x = x,
                 y = freq))