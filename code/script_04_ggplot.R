
# setup -------------------------------------------------------------------

rm(list = ls())

library(tidyverse)

iris <- as_tibble(iris)
# visualization - ggplot --------------------------------------------------

# #without pipe
# ggplot(data = iris, 
#        mapping = aes (x = Sepal.Length, #define variables plotted on x and y axes through aes()
#                       y = Sepal.Width)) +
#   # '+' adds additional layers
#   
# #with pipe
# ## scatterplot 
# iris %>% 
#   ggplot (mapping = aes(x = Sepal.Length,
#                         y = Sepal.Width)) + 
#   # additional layers
# #aes() names need to match names of variables in the df. 



# visualization - point ---------------------------------------------------

##geom_point() is used to add a point layer

# scatterplot
iris %>% 
  ggplot(aes(x = Sepal.Length,
             y = Sepal.Width)) +  #don't forget to add the space after the '+'.
  geom_point()

# change color by "Species" column
iris %>% 
  ggplot(aes(x = Sepal.Length, 
             y = Sepal.Width, 
             color = Species)) + 
  geom_point()

# change color uniformly 
iris %>% 
  ggplot(aes(x = Sepal.Length, 
             y = Sepal.Width)) + 
  geom_point(color = "pink")

# visualization - line ----------------------------------------------------

## geom_line() adds a line layer

#sample data
df0 <- tibble(x = rep(1:50, 3), 
              y = x * 2)

#basic plot
df0 %>% 
  ggplot(aes(x = x, 
             y = y)) + 
  geom_line()

#change line color and add layer
df0 %>% 
  ggplot(aes(x = x, 
             y = y)) + 
  geom_line(color = "purple") +
  geom_point(color = "lightblue")

# visualization - histogram -----------------------------------------------

## geom_histogram makes a histogram 

#baisc plot; bins = 30 by default
iris %>%  
  ggplot(aes(x = Sepal.Length)) + 
  geom_histogram()

# change bin width
iris %>%  
  ggplot(aes(x = Sepal.Length)) + 
  geom_histogram(binwidth = 0.5)

# change bin number
iris %>%  
  ggplot(aes(x = Sepal.Length)) + 
  geom_histogram(bins = 50)

# change bin number and color outline
iris %>%  
  ggplot(aes(x = Sepal.Length)) + 
  geom_histogram(bins = 50, color = "pink")

#change bar color
iris %>%  
  ggplot(aes(x = Sepal.Length)) + 
  geom_histogram(bins = 50, fill = "pink", color = "darkblue")


# visualizations - boxplot ------------------------------------------------

#basic plot
iris %>%  
  ggplot(aes(x = Species, 
             y = Sepal.Length)) + 
  geom_boxplot()

# change fill by "Species"
iris %>%  
  ggplot(aes(x = Species, 
             y = Sepal.Length, 
             fill = Species)) + 
  geom_boxplot()

#change opacity of fill
iris %>%  
  ggplot(aes(x = Species, 
             y = Sepal.Length, 
             fill = Species)) + 
  geom_boxplot(alpha = 0.1)

# change fill by "Species", but consistent color
iris %>%  
  ggplot(aes(x = Species, 
             y = Sepal.Length, 
             fill = Species)) + 
  geom_boxplot(color = "darkgrey")


# in class exercise - data to viz - plot 1 --------------------------------

# comparing two density plots


# Build dataset with different distributions
data <- data.frame(type = c( rep("variable 1", 1000), rep("variable 2", 1000)),
  value = c( rnorm(1000), rnorm(1000, mean=4)))

# Represent it
data %>%
  ggplot( aes(x=value)) +
  geom_density( color="#e9ecef", alpha=0.6) +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  theme_classic() +
  labs(fill="")


# in class exercise - data to viz - plot 2 --------------------------------

##area chart


# Libraries
library(tidyverse)
library(hrbrthemes)
library(plotly)
library(patchwork)
library(babynames)
library(viridis)

# Load dataset from github
data <- read.table("https://raw.githubusercontent.com/holtzy/data_to_viz/master/Example_dataset/3_TwoNumOrdered.csv", header=T)
data$date <- as.Date(data$date)

# plot
data %>%
  ggplot( aes(x=date, y=value)) +
  geom_area(fill="#69b3a2", alpha=0.5) +
  geom_line(color="#69b3a2") +
  ggtitle("Evolution of Bitcoin price") +
  ylab("bitcoin price ($)")

