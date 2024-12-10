# Exercices API ----

## Exercice 1 ----
library(jsonlite)
library(dplyr)

### Q1 ----
objet <- toJSON(x = list(name="Test",valeur=42))

typeof(objet)

### Q2 ----
objet_json <- fromJSON(txt = objet)

typeof(objet_json)

### Q3 ----
iris_json <- toJSON(x = iris,pretty = T)

iris_json

### Q4 ----

iris_json_2 <- toJSON(iris_json,dataframe = "columns",pretty = T)

iris_json_2

# les autres valeurs possibles sont rows columns ou values

### Q5 ----
iris_ndjson <- stream_out(x = iris)

### Q6 ----
stream_out(x = iris,con = file("iris_json.json"))

### Q7 ----
iris_ndjson_import <- stream_in(con = file("iris_json.json"))


## Ex 2 ----

### Q1 ----
url_base <- "https://swapi-node.vercel.app"

options <- "/api/planets"

planets<- fromJSON(paste0(url_base,options)) 

### Q2 ----
t<- planets[["results"]] %>% tibble()


### Q3 ----

planets <- NULL


while(!is.null(options)) {
  data <- fromJSON(paste0(url_base,options))
  
  planets <- bind_rows(planets,data[["results"]])
  
  options <- data[["next"]]
}

### Q4 ---
stream_out(planets,con= file("planets.json"))
