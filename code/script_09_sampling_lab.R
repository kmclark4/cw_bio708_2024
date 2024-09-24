# set up ------------------------------------------------------------------

rm(list = ls())
source(here::here("code/set_library.R"))


# exercise 1 --------------------------------------------------------------
## obtain 100 sub-data sets with 50 and 100 measures each,
## and draw histograms of sample means and unbiased variances


df_h0 <- read_csv(here::here("data_raw/data_plant_height.csv"))
# for 50
mu <- sigma <- NULL

for (i in 1:100) {
  df_i <- df_h0 %>% 
    sample_n(50)  
  mu[i] <- mean(df_i$height)
  
  sigma[i] <- sum((df_i$height - mean(df_i$height))^2) / nrow(df_i)
}




df_50 <- tibble(mu = mu, sigma = sigma) 

(g50_mu <- df_50 %>% 
  ggplot(aes(x = mu)) +
  geom_histogram())

(g50_sig <- df_50 %>% 
  ggplot(aes(x = sigma)) +
  geom_histogram())

g50_mu / g50_sig

# for 100
mu <- sigma <- NULL

for (i in 1:100) {
  df_i <- df_h0 %>% 
    sample_n(100)  
  mu[i] <- mean(df_i$height)
  
  sigma[i] <- sum((df_i$height - mean(df_i$height))^2) / nrow(df_i)
}




df_100 <- tibble(mu = mu, sigma = sigma) 

(g100_mu <- df_100 %>% 
    ggplot(aes(x = mu)) +
    geom_histogram())

(g100_sig <- df_100 %>% 
    ggplot(aes(x = sigma)) +
    geom_histogram())

g100_mu / g100_sig

g50_mu / g50_sig / g100_mu / g100_sig



# # second approach 
# dp1 <- df_m %>% 
#   ggplot(aes(x = mu, 
#              color = factor(n))) +  
#     geom_density()
# 
# dp2 <- df_m %>% 
#   ggplot(aes(x = sigma, 
#              color = factor(n))) +
#   geom_density()
# 
# dp1 / dp2


##more practice - combination of lapply, function, for loop

df_m <- lapply(X = c(50, 100), 
               function(z) {
                 mu <- signma <- NULL
                 for(i in 1:100) {
                   df_i <- df_h0 %>% 
                     sample_n(z)
                   
                   mu[i] <- var(df_i$height)
                   sigma[i] <- var(df_i$height)
                 }
                 cout <- tibble(n = z, 
                                mu = mu, 
                                sigma = sigma)
                 
                 return(cout)
               }) %>% 
  bind_rows()


# exercise 2 --------------------------------------------------------------

df_h10 <- df_h0 %>% 
  filter(height >= 10)

for (i in 1:100) {
  df_i <- df_h0 %>% 
    sample_n(50)  
  mu[i] <- mean(df_i$height)
  
  sigma[i] <- sum((df_i$height - mean(df_i$height))^2) / nrow(df_i)
}



df_h10 <- tibble(mu = mu, sigma = sigma) 

(g10_mu <- df_h10 %>% 
    ggplot(aes(x = mu)) +
    geom_histogram())

(g10_sig <- df_h10 %>% 
    ggplot(aes(x = sigma)) +
    geom_histogram())

g10_mu / g10_sig

