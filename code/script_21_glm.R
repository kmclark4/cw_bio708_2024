# set up ------------------------------------------------------------------

rm(list = ls())
library(tidyverse)

source(here::here("code/set_library.R"))


# proportional data -------------------------------------------------------

df_mussel <- read_csv("data_raw/data_mussel.csv")
print(df_mussel)

#calculate the porportion of fertilized eggs
df_mussel <- df_mussel %>% 
  mutate(prop_fert = n_fertilized / n_examined)

df_mussel %>% 
  ggplot(aes(x = density,
             y = prop_fert)) +
  geom_point(color = "maroon") +
  labs(y = "Proportion of eggs fertilized",
       x = "Mussel density")

## binomial model 

df_test <- tibble(logit_x = seq(-10, 10, length = 100),
                  x= exp(logit_x) / (1 + exp(logit_x)))

df_test %>% 
  ggplot(aes(x = logit_x,
             y = x)) +
  geom_point(color = "maroon") +
  geom_line(color = "maroon") +
  labs(y = "x",
       x = "logit(x)")

m_binom <- glm(cbind(n_fertilized, n_examined - n_fertilized) ~ density,
               data = df_mussel,
               family = "binomial")

cbind(df_mussel$n_fertilized, df_mussel$n_examined - df_mussel$n_fertilized) %>% 
  head()

summary(m_binom)

df_pred <- tibble(density = seq(min(df_mussel$density),
                                max(df_mussel$density),
                                length = 100))

y_binom <- predict(m_binom, newdata = df_pred) %>%  boot::inv.logit()
df_pred <- df_pred %>% 
  mutate(y_binom)

df_mussel %>% 
  ggplot(aes(x = density,
             y = prop_fert)) +
  geom_point() +
  geom_line( data = df_pred,
             aes(y = y_binom)) +
  labs(y = "Proportion of eggs fertilized",
       x = "Mussel density")
