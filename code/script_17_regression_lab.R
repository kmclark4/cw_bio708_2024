# set up ------------------------------------------------------------------

rm(list = ls())
library(tidyverse)


# exercise 1 --------------------------------------------------------------

##Develop regression models 

head(iris)
df0 <- iris

df_setosa <- iris %>% 
  filter(Species == "setosa")

df_versicolor <- iris %>% 
  filter(Species == "versicolor")

df_virginica <- iris %>% 
  filter(Species == "virginica")

# regression for setosa

m_s <- lm(Sepal.Width ~ Petal.Width,
        data = df_setosa)
summary(m_s)

# regression for versicolor

m_ve <- lm(Sepal.Width ~ Petal.Width,
        data = df_versicolor)
summary(m_ve)

# regression for virginica

m_vi <- lm(Sepal.Width ~ Petal.Width,
        data = df_virginica)
summary(m_vi)



# exercise 2 --------------------------------------------------------------

# regression for setosa sepal width ~ petal width + petal length

m1_s <- lm(Sepal.Width ~ Petal.Width + Petal.Length,
          data = df_setosa)
summary(m1_s)

# regression for versicolor sepal width ~ petal width + petal length

m1_ve <- lm(Sepal.Width ~ Petal.Width + Petal.Length,
           data = df_versicolor)
summary(m1_ve)

# regression for virginica sepal width ~ petal width + petal length

m1_vi <- lm(Sepal.Width ~ Petal.Width + Petal.Length,
           data = df_virginica)
summary(m1_vi)


# interaction  ------------------------------------------------------------

m_int <- lm(lm(Sepal.Width ~ Petal.Width + Petal.Length + Petal.Width:Petal.Length,
               data = iris))
summary(m_int)


# extra exercise ----------------------------------------------------------

##Calculate coefficient of determination for one species
ss <- sum(resid(m_vi)^2)
v_y <- df_virginica %>% pull(Sepal.Width)
ss_0 <- sum((v_y - mean(v_y))^2)
r2 <- 1 - ss / ss_0
print(r2)

