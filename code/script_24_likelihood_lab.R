# set up ------------------------------------------------------------------

rm(list = ls())
source(here::here("code/set_library.R"))


# binomial distribution ---------------------------------------------------

y <- c(2, 2, 0, 0, 3, 1, 3, 3, 4, 3)
lambda <- seq(0, 1, by = 0.01)

dbinom(x = y, size = 10, lambda)

lh <- sapply(lambda, function(x) prod(dbinom(x = y,
                                             size = 10,
                                             prob = x)))
(df_p <- tibble(prob = lambda,
               lh = lh) %>% 
  arrange(desc(lh)))

#sample mean
mean(y/10)
