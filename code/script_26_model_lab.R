# set up ------------------------------------------------------------------

rm(list = ls())
source(here::here("code/set_library.R"))

#install.packages("palmerpenguins")
library(palmerpenguins)


# df cleanup ----------------------------------------------------

df_pens <- tibble(penguins_raw)

#rename columns
colnames(df_pens) <- c("study_name", "sample_number",
                       "species", "region", "island", "stage",
                       "individual_id", "clutch_completion",
                       "data_egg", "culmen_length", "culmen_depth",
                       "flipper_length", "body_mass", "sex", "delta_15n",
                       "delta_13c", "comments")

# df_pen0 <- as_tibble(penguins_raw)
# cnm <- colnames(df_pen0)
# cnm_clean <- str_to_lower(cnm) %>% 
#   str_replace_all("\\s", "_") %>% 
#   str_replace_all("_\\(mm\\)","") %>% 
#   str_replace_all("_\\(g\\)", "") %>% 
#   str_replace_all("_\\(o/oo\\)", "")

# replace yes/no with 1/0
# df_pens <- data.frame(clutch_comparison=c("Yes", "No"), stringsAsFactors = FALSE)
# ifelse(df_pens == "Yes", 1, 0)

#change species names
# df_pen0 %>% 
#   mutate(success = ifelse(clutch_completion == "Yes",
#                           yes = 1,
#                           no = 0),
#          species = case_when(species == "Adelie Penguin (Pygoscelis adeliae)" ~ "adelie",
#                              species == "Chinstrap penguin (Pygoscelis antarctica)" ~ "chinstrip",
#                              species == "Gentoo penguin (Pygoscelis papua)" ~ "gentoo"))

df_pen1 <- df_pens %>% 
  mutate(success = ifelse(clutch_completion == "Yes",
                          yes = 1,
                          no = 0),
         species = case_when(species == "Adelie Penguin (Pygoscelis adeliae)" ~ "adelie",
                             species == "Chinstrap penguin (Pygoscelis antarctica)" ~ "chinstrip",
                             species == "Gentoo penguin (Pygoscelis papua)" ~ "gentoo")) %>% 
  drop_na(culmen_length, culmen_depth, flipper_length, body_mass, sex)

# analysis ----------------------------------------------------------------

m <- glm(success ~ species + 
      culmen_length + 
      culmen_depth + 
      flipper_length +
      body_mass,
    family = binomial(),
    data = df_pen1)

## to look at multiple predictors all at once
# install.packages("MuMIn")
library(MuMIn)
options(na.action = "na.fail")
m_set <- dredge(m, rank = "AIC")
subset(m_set, delta < 4)
