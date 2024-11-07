
# set up ------------------------------------------------------------------

rm(list = ls())
library(tidyverse)

source(here::here("code/set_library.R"))

# linear model ------------------------------------------------------------

df_fl <- read_csv("data_raw/data_fish_length.csv")
print(df_fl)

##calculate the different of fish body size in two lakes
(v_mu <- df_fl %>% 
  group_by(lake) %>% 
  summarize(mu = mean(length)) %>% 
  pull(mu))
#element 1 is mu of lake a and elemet 2 is mu of lake b

v_mu[2] - v_mu[1]

m <- lm(length ~ lake,
   data = df_fl)

summary(m)

# look into details of t-test
lake_a <- df_fl %>% 
  filter(lake == "a") %>% 
  pull(length)

lake_b <- df_fl %>% 
  filter(lake == "b") %>% 
  pull(length)

t.test(x = lake_b, y = lake_a, var.equal = TRUE)
summary(m)


# anova equivalence with lm -----------------------------------------------

df_anova <- read_csv("data_raw/data_fish_length_anova.csv")
print(df_anova)

# manual calculations of the estimates. 
(v_mu <- df_anova %>% 
  group_by(lake) %>%
  summarize(mu = mean(length)) %>% 
  pull(mu))

print(c(v_mu[1], v_mu[2] - v_mu[1], v_mu[3] - v_mu[1]))

# lm calculations of estimations gives same as manual calculations
m <- lm(length ~ lake, 
        data = df_anova)
summary(m)

m_aov <- aov(length ~ lake,
             data = df_anova)


# ancova ------------------------------------------------------------------

# convert the data format to tibble
iris <- as_tibble(iris)
print(iris)

distinct(iris, Species)

m_iris <- lm(Petal.Length ~ Petal.Width + Species,
             data = iris)
summary(m_iris)

n_rep <- 100
df_pred <- tibble(Petal.Width = rep(seq(min(iris$Petal.Width),
                                        max(iris$Petal.Width),
                                        length = n_rep),
                                    n_distinct(iris$Species)),
                  Species = rep(unique(iris$Species),
                                each = n_rep))
## y_pred is the predicted petal length based on petal width and species?
y_pred <- predict(m_iris,
                     newdata = df_pred)

df_pred <- df_pred %>% 
  mutate(y_pred = y_pred)

iris %>% 
  ggplot(aes(x = Petal.Width,
             y = Petal.Length,
             color = Species)) +
  geom_point(alpha = 0.5) +
  geom_line(data = df_pred,
            aes(y = y_pred))

## another way to make df_pred dataframe

### But the graphs don't match???

df_pred1 <- iris %>% 
  group_by(Species) %>% 
  reframe(Petal.Width = seq(min(Petal.Width),
                            max(Petal.Width),
                            length = 100))

y_pred1 <- predict(m_iris,
                  newdata = df_pred1)

df_pred1 <- df_pred1 %>% 
  mutate(y_pred1 = y_pred1)

iris %>% 
  ggplot(aes(x = Petal.Width,
             y = Petal.Length,
             color = Species)) +
  geom_point(alpha = 0.5) +
  geom_line(data = df_pred1,
            aes(y = y_pred1))
