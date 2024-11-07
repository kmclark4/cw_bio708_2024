# set up ------------------------------------------------------------------

rm(list = ls())
source(here::here("code/set_library.R"))


# Laboratory --------------------------------------------------------------

# normality assumption
m_iris <- lm(Petal.Length ~ Petal.Width + Species,
             data = iris)
summary(m_iris)

x <- resid(m_iris)
shapiro.test(x)

# Model Interpretation

b <- coef(m_iris)
a <- NULL

a[1] <- b[2]
a[2] <- b[1] + b[3]
a[3] <- b[1] + b[4] 


n_rep <- 100
df_pred <- tibble(Petal.Width = rep(seq(min(iris$Petal.Width),
                                        max(iris$Petal.Width),
                                        length = n_rep),
                                    n_distinct(iris$Species)),
                  Species = rep(unique(iris$Species),
                                each = n_rep))


y_pred <- predict(m_iris,
                  newdata = df_pred)

df_pred <- df_pred %>% 
  mutate(y_pred = y_pred)

p_org <- iris %>% 
  ggplot(aes(x = Petal.Width,
             y = Petal.Length,
             color = Species)) +
  geom_point(alpha = 0.5) +
  geom_line(data = df_pred,
            aes(y = y_pred)) +
  geom_point(data = tibble(Petal.Width = 0,
                           Petal.Length = a,
                           Species = c("setosa",
                                       "versicolor",
                                       "virginica")), 
             color = "maroon")

# Alternative model

m_iris0 <- lm(Petal.Length ~ Petal.Width,
              data = iris)
df_alt <- tibble(Petal.Width = with(iris,
                                     seq(min(Petal.Width),
                                         max(Petal.Width),
                                         length = 100)))
y_pred <- predict(m_iris0,
                  newdata = df_alt)
df_alt <- df_alt %>% 
  mutate(y_pred = y_pred)

iris %>% 
  ggplot(aes(x = Petal.Width,
             y = Petal.Length)) +
  geom_point(alpha = 0.5) +
  geom_line(data = df_alt,
            aes(y = y_pred))