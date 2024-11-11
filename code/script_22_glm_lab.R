# set up ------------------------------------------------------------------

rm(list = ls())
library(tidyverse)

source(here::here("code/set_library.R"))


# glm lab -----------------------------------------------------------------

df_fish <- read_csv(here::here("data_raw/data_vpart.csv"))

with(df_fish, mean(n_sp))
with(df_fish, var(n_sp))

m_pois <- glm(n_sp ~ distance + cat_area + hull_area,
              data = df_fish,
              family = "poisson")

summary(m_pois)

b <- coef(m_pois)


df_pred <- df_fish %>% 
  reframe(distance = seq(min(distance),
                         max(distance),
                         length = 100),
          cat_area = mean(cat_area),
          hull_area = mean(hull_area)) %>% 
  mutate(log_y_pred = predict(m_pois,
                              newdata = .),
         y_pred = exp(log_y_pred))

df_fish %>% 
  ggplot(aes(x = distance,
             y = n_sp)) +
  geom_point(color = "maroon") +
  geom_line(data = df_pred,
            aes(y = y_pred)) +
  labs(y = "Fish species richness",
       x = "Distance to the sea") +
  theme_bw()

g_fish <- df_fish %>% 
  ggplot(aes(x = distance,
             y = n_sp)) +
  geom_point(color = "maroon") +
  geom_line(data = df_pred,
            aes(y = y_pred)) +
  labs(y = "Fish species richness",
       x = "Distance to the sea") +
  theme_bw()


# effect size -------------------------------------------------------------

m_std <- glm(n_sp ~ scale(distance) + scale(cat_area) + scale(hull_area),
    data = df_fish,
    family = "poisson")

summary(m_std)

df_pred2 <- df_fish %>% 
  reframe(distance = seq(min(distance),
                         max(distance),
                         length = 100),
          cat_area = mean(cat_area),
          hull_area = mean(hull_area)) %>% 
  mutate(log_y_pred = predict(m_std, 
                              newdata = .),
         y_pred = exp(log_y_pred))

df_fish %>% 
  ggplot(aes(x = distance,
             y = n_sp)) +
  geom_point(color = "maroon")+
  geom_line(data = df_pred2, 
            aes(y = y_pred))+
  labs(y = "Fish species richness",
       x = "Distance to the sea (m)")
