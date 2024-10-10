# set up ------------------------------------------------------------------
rm(list = ls())
library(tidyverse)


# exercise 1 --------------------------------------------------------------

df0 <- tibble(PlantGrowth)
## plot based off of 4.1
df0_mu <- df0 %>%
  group_by(group) %>% 
  summarize(mu_0 = mean(weight),
            sd_0 = sd(weight))

df0 %>% 
  ggplot(aes(x = group,
             y = weight)) +
  geom_jitter(width = 0.1, 
              height = 0,
              alpha = 0.25, color = "maroon") +
  geom_segment(data = df0_mu,
               aes(x = group,
                   xend = group,
                   y = mu_0 - sd_0,
                   yend = mu_0 + sd_0)) +
  geom_point(data = df0_mu,
             aes(x = group,
                 y = mu_0),
             size = 3, color = "maroon") +
  labs(x = "Group",
       y = "Weight")

##Violin plot
df0 %>% 
  ggplot(aes(x = group,
             y = weight)) +
  geom_violin(draw_quantiles = 0.5, 
              alpha = 0.2) + 
  geom_jitter(alpha = 0.2, color = "maroon")
# exercise 2 --------------------------------------------------------------

## Manually calculate F value
mu <- mean(df0$weight)


df_g <- df0 %>% 
  group_by(group) %>% 
  summarize(mu_g = mean(weight), 
            dev_g = (mu_g - mu)^2, 
            n = n()) 

df_g <- df_g %>% 
  mutate(ss = dev_g * n)

s_b <- sum(df_g$ss)

df_i <- df0 %>% 
  group_by(group) %>% 
  mutate(mu_g = mean(weight)) %>% 
  ungroup() %>% 
  mutate(dev_i = (weight - mu_g)^2) 

df_i_g <- df_i %>% 
  group_by(group) %>% 
  summarize(ss = sum(dev_i))

s_w <- sum(df_i_g$ss)

n_g <- n_distinct(df0$group)
s2_b <- s_b / (n_g - 1)

s2_w <- s_w / (nrow(df0) - n_g)

## f-statistic
f_value <- s2_b / s2_w
f_value

##Check work with aov()
x <- aov(formula = weight ~ group,
    data = df0)
summary(x)

TukeyHSD(x)

## create a function to calculate f value
plant_f <- function(data) {
  mu <- mean(data$weight)
  
  
  df_g <- data %>% 
    group_by(group) %>% 
    summarize(mu_g = mean(weight), 
              dev_g = (mu_g - mu)^2, 
              n = n()) 
  
  df_g <- df_g %>% 
    mutate(ss = dev_g * n)
  
  s_b <- sum(df_g$ss)
  
  df_i <- data %>% 
    group_by(group) %>% 
    mutate(mu_g = mean(weight)) %>% 
    ungroup() %>% 
    mutate(dev_i = (weight - mu_g)^2) 
  
  df_i_g <- df_i %>% 
    group_by(group) %>% 
    summarize(ss = sum(dev_i))
  
  s_w <- sum(df_i_g$ss)
  
  n_g <- n_distinct(df0$group)
  s2_b <- s_b / (n_g - 1)
  
  s2_w <- s_w / (nrow(data) - n_g)
  
  ## f-statistic
  f_value <- s2_b / s2_w
  
  return(f_value)
}

plant_f(df0)
