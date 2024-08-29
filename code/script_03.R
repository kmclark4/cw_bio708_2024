
# setup ------------------------------------------------------------------

rm(list = ls())


##lad library
library(tidyverse)
#iris is default data frame in R
# tibble is similar to data frame but gives you a bit more info
iris <- as_tibble(iris) 


# data manipulation - row manipulation ------------------------------------

## filter(): slect/move rows

# single match "=="
filter(iris, Species == "virginica")

#multiple match "%in%" 
#must use %in% with == you will not get all data
filter(iris, Species %in% c("virginica", "versicolor"))

#expect "!="
filter(iris, Species != "virginica")

# except multiple "!(x %in% c("a", "b"))
filter(iris, !(Species %in% c("virginica", "versicolor")))

# greater than ">"
filter(iris, Sepal.Length > 5)

#equal & greater than ">="
filter(iris, Sepal.Length >= 5)

#less than "<"
filter(iris, Sepal.Length < 5)

# equal & less than "<="
filter(iris, Sepal.Length <= 5)

## arragne(): arrange the order of rows

#arrange in an ascending order
arrange(iris, Sepal.Length)

# arrange in a descending order
arrange(iris, desc(Sepal.Length))


# data manipulation - column manipulation ---------------------------------

## select(): select/remove column(s)

#select one column
select(iris, Sepal.Length)

# select multiple columns
select(iris, c(Sepal.Length, Sepal.Width))

# remove one column
select(iris, -Sepal.Length)

# remove multiple columns
select(iris, -c(Sepal.Length, Sepal.Width))

# select/remove multiple columns with a start rule
# starts_with("x")
select(iris, starts_with("Sepal"))
select(iris, -starts_with("Sepal"))

# select/remove multiple columns with an end rule
# ends_with("x")
select(iris, ends_with("Width"))
select(iris, -ends_with("Width"))

## mutate(): add column(s)

# add new column
x <- 1:150
mutate(iris, id = x)


# data manipulation - piping ----------------------------------------------

### %>% (pipe) allows sequential operations of multiple functions. 
  # the pipe passes the object to the following function as the first argument.

# the following codes produce the same data frame
# apply functions separately
df_vir <- filter(iris, Species == "virginica")
df_vir_sl <- select(df_vir, Sepal.Length)
print(df_vir_sl)

# piping
#hot key for piping is ctl + shift + m

iris %>%
    filter(Species == "virginica") %>%
    select(Sepal.Length)


# data manipulation - reshape ---------------------------------------------

#pivot_wider(): reshape a data format to a wide format

iris_w <- iris %>%
  mutate(id = rep(1:50, 3)) %>%  # add an ID column
  select(id, Sepal.Length, Species) %>%  # selecting the columns
  pivot_wider(id_cols = "id",  #unique row ID based on
              values_from = "Sepal.Length",  # values in each cell from specified column
              names_from = "Species") # new column names from
print(iris_w) #instead of species in the rows they are now columns but only for sepal length bc that is what was selected

#pivot_longer(): reshape a data frame to a long format
iris_l <- iris_w %>%
  pivot_longer(cols = c("setosa",
                        "versicolor",
                        "virginica"),  # columns with values to be reshaped
               names_to = "Species", # column IDs move to "Species"
               values_to = "Sepal.Length") # column values move to "Sepal.Length"
print(iris_l) # making columns into rows (reverting the data frame back to original after making pivot_wider)


# data manipulation - group operation -------------------------------------
## group_by() & summarize(): group-by-group operation. summarize () does not retain individual rows.

# grouping by "Species", then take means "Sepal.Length" for each species 
iris %>%
  group_by(Species) %>%  # generally want to group by character or factor and NOT numeric
  summarize(mu_sl = mean(Sepal.Length))

# grouping by "species", then take means & SD "Sepal.Length for each species
iris %>%
  group_by(Species) %>%
  summarize(mu_sl = mean(Sepal.Length),
            sd_sl = sd(Sepal.Length))

## group_by() & mutate(): group-by-group operation. mutate() retains individual rows along with summary columns. Do not forget ungroup() to avoid errors in following operations. 

# grouping by "species", then take means "Sepal.Length" for each species 
iris %>%
  group_by(Species) %>% 
  mutate(mu_sl = mean(Sepal.Length)) %>%
  ungroup()


# data manipulation - join ------------------------------------------------

##left_join(): merge data frames based on column(s)

#matching by a single column
## left join by "species": one to one
df1 <- tibble(Species = c("A", "B", "C"),
              x = c(1, 2, 3))

df2 <- tibble(Species = c("A", "B", "C"),
              y = c(4, 5, 6))

left_join(x = df1,
          y = df2,
          by = "Species")

# matching by a single cloumn
## left join by "species": one to many

df3 <- tibble (Species = c("A", "A", "B", "C"),
               y = c(4, 5, 6, 7))
left_join(x = df1,
          y = df3,
          by = "Species")

# matching by a single column
## left join by "Species": one to missing
df4 <- tibble(Species = c("A", "A", "C"),
              y = c(4, 5, 7))

left_join(x = df1,
          y = df4,
          by = "Species")

# matching by multiple columns
## one to one
df5 <- tibble(Species = c("A", "B", "C"),
              x = c(1, 2, 3),
              z = c("cool", "awesome", "magical"))
left_join(x = df1,
          y = df5,
          by = c("Species", "x"))

# matching by multiple columns
## one to many
df6 <- tibble(Species = c("A", "A", "B", "C"),
              x = c(1, 1, 2, 3),
              z = c("cool", "cool", "awesome", "magical"))
left_join(x = df1,
          y = df6,
          by = c("Species", "x"))

# matching by multiple columns
## one to missing
df6 <- tibble(Species = c("A", "B", "C"),
              x = c(1, 2, 4),
              z = c("cool", "awesome", "magical"))
left_join(x = df1,
          y = df6,
          by = c("Species", "x"))



