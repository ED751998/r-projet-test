library(tidyverse)

tbl <- read_csv("data/piscines.csv")

class(tbl)

head(tbl, 5) # `head` est une fonction générique

p1 <- filter(tbl, Longitude>153.02)

p1$Longitude


# Exercices ----

## Iris ----
data <- iris


### Q1 ----
Q1 <- data %>% 
  select(Petal.Width,Species)

### Q2 ----
Q2 <- data %>% 
  filter(Species %in% c("versicolor","virginica"))

### Q3 ----
Q3 <- data %>% 
  filter(Species=="setosa") %>% 
  summarise(n=n())

print(Q3)

### Q4 ----
Q4 <- data %>% 
  filter(Species == "versicolor") %>% 
  summarise(Moy=mean(Petal.Width))

Q4

### Q5 ----

Q5 <- data %>% 
  mutate(Sum.Width = Petal.Width+Sepal.Length)
