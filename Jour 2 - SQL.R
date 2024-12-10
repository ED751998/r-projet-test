library(DBI)

# Ex 1 ----

## Q1 - Connexion ----
con <- dbConnect(RSQLite::SQLite(), dbname="data/star.db")

## Q2 - liste des tables et des variables ----
tables <- dbListTables(con)

var_t1 <- dbListFields(con,tables[1])

var_t2 <- dbListFields(con,tables[2])

## Q3 - Contenu de la table topologie ----

Topologie <- dbGetQuery(conn = con,
                        statement = "SELECT * FROM Topologie")

## Q4 - Avec des tibles ----
Etat <- tbl(con,"Etat")
Topologie <- tbl(con,"Topologie")

## Q5 - Sélection de colonnes ----
topologie_db <- Topologie %>% 
  select(id,nom,id_proche_1) %>% 
  collect()

## Q6 - Jointure ----
topologie_db <-  Topologie %>% 
  select(id,nom,id_proche_1) %>% 
  left_join(Topologie %>% 
              select(id,Nom_proche=nom),
            by=c("id_proche_1"="id")) %>% 
  collect()

## Q7 - Ajout de la distance ----
topologie_db <- Topologie %>% 
  select(id,nom,id_proche_1) %>% 
  left_join(Topologie %>% 
              select(id,latitude,longitude),
            by=c("id")) %>% 
  left_join(Topologie %>% 
              select(id,latitude_proche=latitude,longitude_proche=longitude),
            by=c("id_proche_1"="id")) %>% 
  collect()

# garder en tête que cette distance ne peut pas être calculée directement dans SQL à moins de télécharger des packages
# Il faudrait donc avoir la vraie requête SQL pour exporter totalement les calculs
topologie_db <- topologie_db %>% 
  rowwise() %>% 
  mutate(Distance= distVincentySphere(p1 = c(latitude,longitude),p2 = c(latitude_proche,longitude_proche))) %>% 
  arrange(Distance)

## Q8 3 stations les plus proches du point GPS ----

# ma méthode
topologie_db_gps <- topologie_db %>% 
  select(id,nom, latitude,longitude) %>% 
  mutate(Distance= distVincentySphere(p1 = c(48.1179151,-1.7028661),p2 = c(latitude,longitude))) %>%
  arrange(Distance) %>% 
  left_join(Etat %>% 
              select(id,velos_disponibles) %>% 
              collect(),by=c("id"))

# grâce à chat gpt 

# Récupération de la requête SQL
Topologie %>% 
  select(id,nom, latitude,longitude) %>% 
  mutate(Distance = rnorm(83,0,1)) %>% 
  left_join(Etat %>% 
              select(id,velos_disponibles),
            by=c("id")) %>% 
  arrange(Distance) %>% 
  show_query()

# Vraie requête SQL 
Test <- dbGetQuery(conn = con,
                        statement = "SELECT `q01`.*
FROM (
  SELECT `LHS`.*, `velos_disponibles`
  FROM (
    SELECT
      `id`,
      `nom`,
      `latitude`,
      `longitude`,
      6371 * 2 * ASIN(SQRT(POWER(SIN((RADIANS(latitude) - RADIANS(48.1179151)) / 2), 2) + COS(RADIANS(48.1179151)) * COS(RADIANS(latitude)) * POWER(SIN((RADIANS(longitude) - RADIANS(-1.7028661)) / 2), 2))) AS `Distance`
    FROM `Topologie`
  ) AS `LHS`
  LEFT JOIN `Etat`
    ON (`LHS`.`id` = `Etat`.`id`)
) AS `q01`
ORDER BY `Distance`")

## Q9 - Requête SQL ----

## Q10 - On se déconnecte ----
dbDisconnect(conn = con)
