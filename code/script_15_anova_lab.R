# set up ------------------------------------------------------------------
rm(list = ls())
library(tidyverse)


# exercise 1 --------------------------------------------------------------

df0 <- tibble(PlantGrowth)

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


# exercise 2 --------------------------------------------------------------

x <- aov(formula = weight ~ group,
    data = df0)
summary(x)

TukeyHSD(x)
