# Exercice Mongo

## Q1 ----
install.packages("mongolite")
library(mongolite)

m <- mongo("planets")

if(m$coount>0){
  m$drop()
}

m$count()

## Q2 ----
library(jsonlite)
m$import(
  file("data/planets.json")
  )

m$count()


m$find() %>% 
  tibble() %>% 
  head()

## Q3 ----
table <- m$find(
  query='{"rotation_period":"25"}'
)


## Q4 ----
table <- m$find(
  query='{"rotation_period":"25"}',
  fields='{"_id":false, "name": true, "rotation_period": true,"orbital_period":true,"diameter":true}'
) %>% head()

## Q5 ----
table %>% 
  arrange(desc(diameter))

## Q6 ----
m$drop()

table<- stream_in(file("data/planets.json"))

# transformation en numérique 
var_num <- c("rotation_period","orbital_period","diameter","surface_water","population")

table_num <- table %>% 
  mutate(across(all_of(var_num),as.numeric))

# séparation des chaînes de cacractère
table_char <- table_num %>% 
  rowwise() %>% 
  mutate(climate = list(climate %>% stringr::str_split_1(pattern = ",")),
         terrain = list(terrain %>% stringr::str_split_1(pattern = ",")))

table_fin <- table_char %>% 
  select(!c("films","gravity","residents","created","edited"))

## Q7 ----
stream_out(table_fin,con= file("planets2.json"))

m <- mongo("planets")

m$count()

m$drop()
m$import(
  file("planets2.json")
)

m$count()

m$find(
  query='{"rotation_period":25}',
  fields='{"_id":false, "name": true, "rotation_period": true,"orbital_period":true,"diameter":true}'
) %>% head()

table <-m$find(
  query='{"rotation_period":25}',
  fields='{"_id":false, "name": true, "rotation_period": true,"orbital_period":true,"diameter":true}'
) %>% head()


table %>% 
  arrange(desc(diameter))

## Q8 ----
m$find(
  query='{
  "name":{"$regex": "^T"}
  }',
  fields='{"_id":false, "name": true}'
) 

## Q9 ----


m$find(
query='{
"$and": [
{"diameter":{"$gt": 10000}},
{"terrain":"mountains"}
]
}',
fields='{"_id": false, "name": true}'
)

## Q10 ----
m$find(
  query='{
  "name":"unknown"}',
  fields = '{"name": true}'
)

m$update(
  query='{"name":"unknown"}',
  update='{
  "$unset":{"name":"unknown"}}'
) 

m$find(
  query='{
  "name":"unknown"}',
  fields = '{"name": true}'
)

## Q11 ----
m$find(
  fields='{"_id":false,
  "name":true}'
) %>% table() %>% sum()

m$count()

## Q12 ----
m$aggregate(
  
)