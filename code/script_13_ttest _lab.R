
# set up ------------------------------------------------------------------
rm(list = ls())
library(tidyverse)
# source(here::here("code/set_library"))

# Laboratory --------------------------------------------------------------

xs <-rnorm(n = 10, mean = 10, sd= 5)
ys <- rnorm(n = 10, mean = 12, sd = 5)
xl <- rnorm(n = 100, mean = 10, sd = 5)
yl <- rnorm(n = 100, mean = 12, sd = 5)

t.test(xs, ys, var.equal = TRUE)

t.test(xl, yl, var.equal = TRUE)

##Difference and Uncertainty
a1 <- c(13.9, 14.9 ,13.4, 14.3, 11.8, 13.9, 14.5, 15.1, 13.3, 13.9)
a2 <- c(17.4, 17.3, 20.1, 17.2, 18.4, 19.6, 16.8, 18.7, 17.8, 18.9)

b1 <- c(10.9, 20.3, 9.6, 8.3, 14.5, 12.3, 14.5, 16.7, 9.3, 22.0)
b2 <- c(26.9, 12.9, 11.1, 16.7, 20.0, 20.9, 16.6, 15.4, 16.2, 16.2)

df0 <- tibble(value = c(a1, a2, b1, b2), group = c(rep("a1", length(a1)),
                                                   rep("a2", length(a2)),
                                                   rep("b1", length(b1)),
                                                rep("b2", length(b2))))


df0_mu <- df0 %>%
  filter(group %in% c("a1", "a2")) %>% 
  group_by(group) %>% 
  summarize(mu_0 = mean(value),
            sd_0 = sd(value))

df0 %>% 
  filter(group %in% c("a1", "a2")) %>% 
  ggplot(aes(x = group,
             y = value)) +
  geom_jitter(width = 0.1, 
              height = 0,
              alpha = 0.25) +
  geom_segment(data = df0_mu,
               aes(x = group,
                   xend = group,
                   y = mu_0 - sd_0,
                   yend = mu_0 + sd_0)) +
  geom_point(data = df0_mu,
             aes(x = group,
                 y = mu_0),
             size = 3) +
  labs(x = "Group",
       y = "Value")

##comparing b1 and b2
df1_mu <- df0 %>%
  filter(group %in% c("b1", "b2")) %>% 
  group_by(group) %>% 
  summarize(mu_1 = mean(value),
            sd_1 = sd(value))

df0 %>% 
  filter(group %in% c("b1", "b2")) %>% 
  ggplot(aes(x = group,
             y = value)) +
  geom_jitter(width = 0.1, 
              height = 0,
              alpha = 0.25) +
  geom_segment(data = df1_mu,
               aes(x = group,
                   xend = group,
                   y = mu_1 - sd_1,
                   yend = mu_1 + sd_1)) +
  geom_point(data = df1_mu,
             aes(x = group,
                 y = mu_1),
             size = 3) +
  labs(x = "Group",
       y = "Value") 
## comparing all four
df2_mu <- df0 %>%
  filter(group %in% c("a1", "a2", "b1", "b2")) %>% 
  group_by(group) %>% 
  summarize(mu_2 = mean(value),
            sd_2 = sd(value))

df0 %>% 
  filter(group %in% c("a1", "a2", "b1", "b2")) %>% 
  ggplot(aes(x = group,
             y = value)) +
  geom_jitter(width = 0.1, 
              height = 0,
              alpha = 0.25) +
  geom_segment(data = df2_mu,
               aes(x = group,
                   xend = group,
                   y = mu_2 - sd_2,
                   yend = mu_2 + sd_2)) +
  geom_point(data = df2_mu,
             aes(x = group,
                 y = mu_2),
             size = 3) +
  labs(x = "Group",
       y = "Value") 

t.test(a1, a2, var.equal = FALSE)
t.test(b1, b2, var.equal = FALSE)
