# set up ------------------------------------------------------------------

rm(list = ls())
source(here::here("code/set_library.R"))


# Generalized Linear Model ------------------------------------------------

df_count <- read_csv("data_raw/data_garden_count.csv")
print(df_count)

#Not something Akira recommends
# fit a normal model to count data
m_normal <- lm(count ~ nitrate,
               df_count)
summary(m_normal)

alpha <- coef(m_normal)[1]
beta <- coef(m_normal)[2]

df_count %>% 
  ggplot(aes(x = nitrate,
             y = count)) +
  geom_point(color = "orange") +
  geom_abline(intercept = alpha,
              slope = beta, color = "black")

g_normal <- df_count %>% 
  ggplot(aes(x = nitrate,
             y = count)) +
  geom_point(color = "orange") +
  geom_abline(intercept = alpha,
              slope = beta, color = "black")

## Poisson Model
m_pois <- glm(count ~ nitrate,
              data = df_count,
              family = "poisson")
summary(m_pois)

## estimating parameters using the standard errors
theta <- coef(m_pois)
se <- sqrt(diag(vcov(m_pois))) # standard error of each estimate
z_value <- theta / se # ratio of estimate to standar error

print(z_value)

# figure for predicted

df_pred_pois <- df_count %>% 
  reframe(nitrate = seq(min(df_count$nitrate),
                        max(df_count$nitrate),
                        length = 100),
          y = exp(theta[1] + theta[2] * nitrate))

y_pois <- predict(m_pois, newdata = df_pred_pois) %>% exp()

g_normal +
  geom_line(data = df_pred_pois,
            aes(y = y_pois),
            color = "salmon")

# another way to 
df_pred <- tibble(nitrate = seq(min(df_count$nitrate),
                                max(df_count$nitrate),
                                length = 100))

y_normal <- predict(m_normal, newdata = df_pred)
y_pois <- predict(m_pois, newdata = df_pred) %>% exp()

df_pred <- df_pred_pois %>% 
  mutate(y_normal,
         y_pois)


df_count %>% 
  ggplot(aes(x = nitrate,
             y = count)) +
  geom_point() +
  geom_line(data = df_pred,
            aes(y = y_normal),
            linetype = "dashed") +
  geom_line(data = df_pred,
            aes(y = y_pois),
            color = "salmon")


# offset term -------------------------------------------------------------

df_count_ue <- df_count %>% 
  mutate(area = rpois(nrow(.), 10),
         count_ue = count * area)

df_count_ue %>% 
  ggplot(aes(x = area,
             y = count_ue)) +
  geom_point(color = "maroon")

m_pois_ue <- glm(count ~ nitrate + offset(log(area)),
    data = df_count_ue,
    family = "poisson")

summary(m_pois_ue)
