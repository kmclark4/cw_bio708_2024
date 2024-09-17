# set up ------------------------------------------------------------------

rm(list = ls())
library(tidyverse)


# the garden plant example ------------------------------------------------

## using the data set in the book to analyze plant height
h <- c(16.9, 20.9, 15.8, 28, 21.6, 15.9, 22.4, 23.7, 22.9, 18.5)

df_h <- tibble(plant_id = 1:10, # a vector from 1 to 10 by 1
               height = h, 
               unit = "cm")

## add columns
(df_h <- df_h %>% 
  mutate(mu_height = mean(height), 
         var_height = sum((height - mu_height)^2) / nrow(.)))

## checking to see if the data are different with different individuals

g <- c(27.6, 21.9, 16.9, 8.9, 25.6, 19.8, 19.9, 24.7, 24.1, 23)

(df_g <- tibble(plant_id = 11:20, # a vector from 11 to 20 by 1
               height = h, 
               unit = "cm") %>% 
  mutate(mu_height = mean(height), 
         var_height = sum((height - mu_height)^2) / nrow(.)))


# 2.2 linking part to whole -----------------------------------------------

# load csv data on R
df_h0 <- read_csv("data_raw/data_plant_height.csv")
# show the first 10 rows
print(df_h0)

# find true mean and variance
(mu <- mean(df_h0$height))
(sigma2 <- sum((df_h0$height - mu)^2) / nrow(df_h0))

# simulate sampling of 10 plant individuals by random selection
(df_i <- df_h0 %>% 
  sample_n(size = 10)) 

# obtain 100 sets of 10 plant individual that were randomly selected and estimate the mean and variance of each.
## for reproducibility
set.seed(3)

mu_i <- var_i <- NULL # create empty objects

# repeat the work in {} from i = 1 to 1 = 100
for (i in 1:100) {
  df_i <- df_h0 %>% 
    sample_n(size = 10) # random samples of 10 individuals.
  #save mean for sample set i 
  mu_i[i] <- mean(df_i$height)
  #save variance for sample set i
  var_i[i] <- sum((df_i$height - mean(df_i$height))^2) / nrow(df_i)
}
print(mu_i)
print(var_i)

# install.packages("patchwork")
library(patchwork)

df_sample <- tibble(mu_hat = mu_i, var_hat = var_i)

# histogram for mean 
(g_mu <- df_sample %>% 
  ggplot(aes(x = mu_hat)) +
  geom_histogram(fill = "darkblue") +
  geom_vline(xintercept = mu, color = "salmon"))

# histogram for variance
(g_var <- df_sample %>% 
    ggplot(aes(x = var_hat)) +
    geom_histogram(fill = "purple") +
    geom_vline(xintercept = sigma2, color = "limegreen"))

# layout vertically
(g_mu / g_var)

## redo and compare with corrected unbiased estimates

# for reproducibility
set.seed(3)

# redo simulations ----
mu_i <- var_i <- var_ub_i <- NULL 

# repeat the work in {} from i = 1 to i = 100
for (i in 1:100) {
  
  df_i <- df_h0 %>% 
    sample_n(size = 10) # random samples of 10 individuals
  
  # save mean for sample set i
  mu_i[i] <- mean(df_i$height)
  
  # save variance for sample set i
  var_i[i] <- sum((df_i$height - mean(df_i$height))^2) / nrow(df_i) 
  
  var_ub_i[i] <- var(df_i$height)
}

#  redo histograms
df_sample <- tibble(mu_hat = mu_i,
                    var_hat = var_i,
                    var_ub_hat = var_ub_i)

# histogram for mu
(g_mu <- df_sample %>% 
  ggplot(aes(x = mu_hat)) +
  geom_histogram(fill = "salmon") +
  geom_vline(xintercept = mu, color = "darkblue"))

# histogram for variance
# scale_x_continuous() adjusts scale in x-axis
(g_var <- df_sample %>% 
  ggplot(aes(x = var_hat)) +
  geom_histogram(fill = "limegreen") +
  geom_vline(xintercept = sigma2, color = "purple") +
  scale_x_continuous(limits= c(min(c(var_i, var_ub_i)),
                               max(c(var_i, var_ub_i)))))

(# histogram for unbiased variance
g_var_ub <- df_sample %>% 
  ggplot(aes(x = var_ub_hat)) +
  geom_histogram(fill = "skyblue") +
  geom_vline(xintercept = sigma2, color = "orange") +
  scale_x_continuous(limits= c(min(c(var_i, var_ub_i)),
                               max(c(var_i, var_ub_i)))))

(g_mu / g_var / g_var_ub)