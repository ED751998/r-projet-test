library(tidyverse)

tbl <- read_csv("data/piscines.csv")

class(tbl)

head(tbl, 5) # `head` est une fonction générique

p1 <- filter(tbl, Longitude>153.02)

p1$Longitude


# Exercices ----

## Iris ----
data <- iris %>% as_tibble()


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

### Q6 ----
Q6 <- data %>% 
  group_by(Species) %>% 
  summarise(Moyenne = mean(Sepal.Length),Variance = var(Sepal.Length))

## Houston flights ----

library(hflights)
hflights <- as_tibble(hflights)

### Q1 ----
library(tidyselect)

Q1 <- hflights %>% 
  select(ends_with("Time"))

### Q2 ----
Q2 <- hflights %>% 
  select(c(15:18))

### Q3 ----
Q3 <- hflights %>% 
  mutate(ActualGroundTime = ActualElapsedTime - AirTime)

### Q4 ----
Q4 <- hflights %>% 
  group_by(FlightNum) %>% 
  mutate(AverageSpeed = mean(AirTime/Distance)) %>% 
  arrange(desc(AverageSpeed))

### Q5 ----
Q5 <- hflights %>% 
  filter(Dest=="JFK")

### Q6 ----
Q6 <- nrow(Q5)

### Q7 ----
Q7 <- hflights %>% 
  summarise(n=n(),n_dest = n_distinct(Dest),n_carrier = n_distinct(UniqueCarrier))

### Q8 ----
hflights %>% 
  filter(UniqueCarrier=="AA") %>% 
  pull(ArrDelay) %>% 
  table()

Q8 <- hflights %>% 
  na.exclude() %>% 
  filter(UniqueCarrier=="AA") %>% 
  summarise(n=n(),cancel = sum(Cancelled),Delay = mean(ArrDelay))

###Q9 ----

Q9 <- hflights %>% 
  na.exclude() %>% 
  group_by(UniqueCarrier) %>% 
  summarise("Nombre total de vols"=n(),
            "Moyenne AirTime" = mean(AirTime))

### Q10 ----

Q10 <- hflights %>% 
  na.exclude() %>% 
  group_by(UniqueCarrier) %>% 
  summarise("Retard" = mean(DepDelay)) %>% 
  arrange(Retard)
