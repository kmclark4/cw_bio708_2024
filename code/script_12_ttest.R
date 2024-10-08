# set up ------------------------------------------------------------------

rm(list = ls())
library(tidyverse) 

# explore data structure --------------------------------------------------


df_fl <- read_csv("data_raw/data_fish_length.csv")
print(df_fl)

# unique returns unique values as a vector
unique(df_fl$lake)

# distinct returns unique values as a tibble
distinct(df_fl, lake)

# group mean and sd
df_fl_mu <- df_fl %>% 
  group_by(lake) %>% # group operation
  summarize(mu_l = mean(length), # summarize by mean()
            sd_l = sd(length)) # summarize with sd()

# plot
# geom_jitter() plot data points with scatter
# geom_segment() draw lines
# geom_point() draw points
df_fl %>% 
  ggplot(aes(x = lake,
             y = length)) +
  geom_jitter(width = 0.1, # scatter width
              height = 0, # scatter height (no scatter with zero)
              alpha = 0.25) + # transparency of data points
  geom_segment(data = df_fl_mu, # switch data frame
               aes(x = lake,
                   xend = lake,
                   y = mu_l - sd_l,
                   yend = mu_l + sd_l)) +
  geom_point(data = df_fl_mu, # switch data frame
             aes(x = lake,
                 y = mu_l),
             size = 3) +
  labs(x = "Lake", # x label
       y = "Fish body length") # y label


# t-test ------------------------------------------------------------------

x <- df_fl %>% 
  filter(lake == "a") %>% 
  pull(length)

y <- df_fl %>%
  filter(lake == "b") %>% # subset lake b
  pull(length)
t.test(x, y, var.equal = TRUE)

## test statistic
(v_mu <- df_fl_mu %>% 
  pull(mu_l))

#difference between lakes a and b
v_mu[1] - v_mu[2]

# group mean, variance, and sample size
df_t <- df_fl %>% 
  group_by(lake) %>% # group operation
  summarize(mu_l = mean(length), # summarize by mean()
            var_l = var(length), # summarize with var()
            n = n()) # count number of rows per group

print(df_t)

#mean, variance, and n samples
# pull values as a vector
v_mu <- pull(df_t, mu_l)
v_var <- pull(df_t, var_l)
v_n <- pull(df_t, n)

#pooled variation
var_p <- ((v_n[1] - 1)/(sum(v_n) - 2)) * v_var[1] +
  ((v_n[2] - 1)/(sum(v_n) - 2)) * v_var[2]

## calculating the t value that is in the t test
# larger t value means it is more reliable because there is less variation
t_value <- (v_mu[1] - v_mu[2]) / sqrt(var_p * ((1 / v_n[1]) + (1 / v_n[2])))

print(t_value)


# null hypothesis ---------------------------------------------------------

x <- seq(-5, 5, length = 500)

y <- dt(x, df = sum(v_n) - 2)

tibble(x, y) %>% 
  ggplot(aes(x = x,
             y = y)) +
  geom_line() +
  labs( y = "Probability density",
        x = "t-statistic")

## draw the enitre range

tibble (x, y) %>% 
  ggplot(aes(x = x,
             y = y)) +
  geom_line(color = "purple") +
  geom_vline(xintercept = t_value,
             color = "salmon") + # t_value is the observed t_value
  geom_vline(xintercept = abs(t_value),
             color = "salmon") + # t_value is the observed t_value
  labs(y = "Probability density",
       x = "t-statistic") 

# calculate area under the curve from -infinity to t_value
pr_below <- pt(q = t_value, df = sum(v_n) - 2) 

# calculate area under the curve from abs(t_value) to infinity
pr_above <- 1 - pt(q = abs(t_value), df = sum(v_n) - 2)

p_value <- pr_below + pr_above
print(p_value)