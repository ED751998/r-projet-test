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
  select(starts_with("Taxi")|matches("D.st"))

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
  group_by(UniqueCarrier) %>% 
  summarise("Retard" = mean(DepDelay,na.rm = T)) %>% 
  arrange(Retard)

## Roland Garros ----

### Q1 ----
data_tennis <-read_csv("data/rolandgarros2013.csv") %>% as_tibble()

### Q2 ----
data_tennis %>% 
  filter(Round==6) %>% 
  select(Player1,Player2)

### Q3 ----
data_tennis %>% 
  summarise(Aces=mean(ACE.1+ACE.2))

### Q4 ----
data_tennis %>% 
  group_by(Round) %>% 
  summarise(Aces=mean(ACE.1+ACE.2))

### Q5 ----
rg_joueurs <- data_tennis %>% 
  select(Joueur=Player1) %>% 
  rbind(data_tennis %>% 
          select(Joueur=Player2)) %>% 
  distinct()

### Q6 ----
Victoires <- data_tennis %>% 
  group_by(Joueur=Player1) %>% 
  summarise(Victoires=sum(Result)) %>% 
  rbind(data_tennis %>% 
          group_by(Joueur=Player2) %>% 
          summarise(Victoires=sum(Result))) %>% 
  group_by(Joueur) %>% 
  summarise(Victoires_rg=sum(Victoires))

rg_joueurs <- rg_joueurs %>% 
  left_join(Victoires,by=c("Joueur"))

### Q7 ----
Australie <- read_csv("data/openaustralie2013.csv") %>% as_tibble()

### Q8 ----
tennis <- bind_rows(list(Australie %>% 
                 mutate(Tournoi="OA",test="t"),data_tennis %>% 
                 mutate(Tournoi="RG",test="j")),.id = "test")

tennis <- Australie %>% mutate(Tournoi="OA") %>% 
  rbind(data_tennis %>%  mutate(Tournoi="RG"))

### Q9 ----
Format_long <- tennis %>% 
  group_by(Tournoi,Round) %>% 
  summarise(Aces=mean(ACE.1+ACE.2))

Format_large <- Format_long %>% 
  pivot_wider(names_from = "Tournoi",values_from = "Aces")

### Q10 ----
oa_joueurs <- Australie %>% 
  select(Joueur =Player1) %>% 
  rbind(Australie %>% 
          select(Joueur=Player2)) %>% 
  distinct()

Victoires_OA <- Australie %>% 
  group_by(Joueur=Player1) %>% 
  summarise(Victoires_oa=sum(Result)) %>% 
  rbind(Australie %>% 
          group_by(Joueur=Player2) %>% 
          summarise(Victoires_oa=sum(Result))) %>% 
  group_by(Joueur) %>% 
  summarise(Victoires_oa=sum(Victoires_oa))

oa_joueurs <- oa_joueurs %>% 
  left_join(Victoires_OA,by=c("Joueur"))

### Q11 ----

# Jointure avec toutes les observations
Final <- full_join(x = rg_joueurs,y = oa_joueurs,by=c("Joueur"))
