# set up ------------------------------------------------------------------

rm(list = ls())
library(tidyverse)


# exercise 1 --------------------------------------------------------------

# Create a new vector z with length 1000
# as exp(rnorm(n = 1000, mean = 0, sd = 0.1)), 
# and calculate the arithmetic mean, geometric mean, and median.

z <- exp(rnorm(n = 1000, mean = 0, sd = 1))
mu_z <- mean(z) #arithemetic mean
gmu_z <- prod(z)^(1 / length(z)) #geometric mean
med_z <-median(z) #median


# draw a histogram of z using functions tibble(), ggplot(), and geom_(histogram)
df_z <- as_tibble(z)

g_hist <- df_z %>% #original histogram
  ggplot(aes(x = z)) + 
  geom_histogram()

# draw vertical lines of arithmetic mean, geometric mean, 
#and median on the histogram with different colors using a function geom_vline()

g_hist2 <- df_z %>% 
  ggplot(aes(x = value)) + 
  geom_histogram() +
  geom_vline(xintercept = mu_z, color = "red", size = 0.75) + 
  geom_vline(xintercept = gmu_z, color = "blue", size = 2, alpha = 0.7) + 
  geom_vline(xintercept = med_z, color = "orange", size = 0.75)

# create a new vector z_rev as -z + max(z) + 0.1 ad repeat 1-4
z <- exp(rnorm(n = 1000, mean = 0, sd = 1))
z_rev <- -z + max(z) + 0.1

(mu_rz <- mean (z_rev))
(gmu_rz <- exp(mean(log(z_rev))))
(med_rz <- median(z_rev))

df_zrev <- as_tibble(z_rev)

g_histr <- df_zrev %>% 
  ggplot(aes(x = z_rev)) + 
  geom_histogram()
g_histr

# draw vertical lines of arithmetic mean, geometric mean, 
#and median on the histogram with different colors using a function geom_vline()

df_mu <- tibble(mu = c(mu_rz, gmu_rz, med_rz), 
                type = c("Arithemetic", "Geometric", "Median"))

g_hist + 
  geom_vline(data = df_mu, 
             aes(xintercept = mu, color = type)) + 
  theme_bw()


# Comparing variation measures --------------------------------------------

w <- rnorm(100, mean = 10, sd = 1)
head(w) # show first 6 elements in w

m <- w * 1000

# standard deviation
var_w <- sum((w - mean(w))^2) / length(w)
(sd_w <- sqrt(var_w))

(var_m <- sum((m - mean(m))^2) / length(m))
(sd_m <- sqrt(var_m))

# MAD
ad_w <- abs(w - median(w))
(mad_w <- median(ad_w))

ad_m <- abs(w - median(m))
(mad_m <- median(ad_m))

# coefficient of variation
(cv_w <- sqrt(var_w) / mean(w))
(cv_m <- sqrt(var_m) / mean(m))

#MAD/Median
# for x
(mm_w <- mad_w / median(w))
(mm_m <- mad_m / median(m))
