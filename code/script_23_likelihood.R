# set up ------------------------------------------------------------------

rm(list = ls())

source(here::here("code/set_library.R"))

# likelihood --------------------------------------------------------------

# Pr(y = 3) = lambda^3 exp(-lambda) / 3! 
# where lambda is the mean od the distribution and lambda = 3.5
dpois(3, lambda = 3.5) 

# write the equation 
# probability of observing 3
# is the data follows a Poisson dist. with mean 3.5
3.5^3 * exp(-3.5) / factorial(3)

# set it as a variable
p <- 3.5^3 * exp(-3.5) / factorial(3)

# change lambda from 0 to 10 by 0.1
lambda <- seq(0, 10, by = 0.1)

pr <- dpois(3, lambda = lambda)

(df_pois <- tibble(y = 3,
                  lambda = lambda,
                  pr = pr))

# make the plot
df_pois %>% 
  ggplot(aes(x = lambda,
             y = pr)) +
  geom_point(color = "maroon") +
  geom_line(color = "maroon") +
  labs(x = "lambda",
       y = "Pr(k = 3)") +
  theme_bw()

#arrange by probability
df_pois %>% 
  arrange(desc(pr))

# data y = {3, 2, 5}
(pr2 <- dpois(c(3, 2, 5), lambda = 3))

pr2[1] * pr2[2] * pr2[3]
#easier way
prod(pr2)

# change the value of lambda to find the best data point using 3, 2, 5
#likelihood for 3, 2, 5 with lambda =  0 -10 by 0.1

y <- c(3, 2, 5)
lambda <- seq(0, 10, by = 0.01)

pr <- sapply(X = lambda,
              FUN = function(z) prod(dpois(y, lambda = z)))

df_pois <- tibble(lambda = lambda,
                   pr = pr)

df_pois %>% 
  arrange(desc(pr))

# plot it
df_pois %>% 
  ggplot(aes(x = lambda,
             y = pr)) +
  geom_line(color = "maroon") +
  labs(y = "Likelihood")

#arrange the data frame again

df_pois %>%
  arrange(desc(pr))
mean(y)

## General case
df_count <- read_csv("data_raw/data_garden_count.csv")

m_pois <- glm(count ~ nitrate,
              data = df_count,
              family = "poisson")
logLik(m_pois)
