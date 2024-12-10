library(rvest)
library(dplyr)

# Exercice 1 ----

## Q1 ----
url <- "https://umizenbonsai.com/shop/bonsai/coniferes/"

read_html(url)

## Q2 ----
read_html(url) %>% 
  html_nodes("#content > article > ul > li") %>% 
  length()

## Q3 ----

href <- read_html(url) %>% html_nodes("#content > article > ul > li.entry > div > div > a") %>% html_attr("href")

mafonction <- function(x){
  url <- x
  
  nom <- read_html(url) %>% 
    html_nodes("div.single-post-title.product_title.entry-title") %>% html_text()
  
  prix <- read_html(url) %>% 
    html_nodes("p.price") %>% html_text()
  
  l <- list(nom,prix)
 
}

t<- sapply(X = href,FUN = mafonction)

## Q4 ----
df <- data.frame(Nom=c(),Prix=c(),Lien=c())

for (i in seq(1,length(href),1)) {
  
  nom <- read_html(href[i]) %>% 
    html_nodes("div.product_title") %>% html_text()
  
  if(read_html(href[i]) %>% 
     html_nodes("p.price bdi") %>% 
     length() == 0){
    
  prix <- read_html(href[i]) %>% 
    html_nodes("p.price bdi") %>% html_text()} else{
      
      prix <- read_html(href[i]) %>% 
        html_nodes("p.price bdi") %>% 
        html_text()
    }
  
  df <- rbind(df,data.frame(Nom=nom,Prix=prix,Lien=href[i]))
}

# Exercice 2 ----

## Q1 ----
url_wikipedia <- "https://fr.wikipedia.org/"
url_blanchett <- "wiki/Cate_Blanchett"
data_html <- paste0(url_wikipedia, url_blanchett) %>% read_html()
film_selector <- "#mw-content-text div ul:nth-of-type(3) li i a"
film_nodes <- data_html %>% html_nodes(film_selector) %>% html_attrs()
films <- tibble()
for(k in seq_along(film_nodes)) {
  film_node <- film_nodes[[k]]
  if("class" %in% names(film_node)) next # Absence de page dédiée
  if(film_node["title"] == "Galadriel") next # Mauvais lien
  films <- rbind(
    films,
    list(titre=film_node["title"], url=film_node["href"])
  )
}

liens <- c()
url_wikipedia2 <- "https://fr.wikipedia.org"
for (i in seq(1,nrow(films),1)) {
  
  j <- read_html(paste0(url_wikipedia2,films$url[i])) %>% html_nodes("span > ul > li > a") %>% html_attr("href") %>% stringr::str_detect(pattern = ".*imdb") %>% which()
  
  l <- read_html(paste0(url_wikipedia2,films$url[i])) %>% html_nodes(paste0("span > ul > li:nth-child(",j,") > a")) %>% html_attr("href") 
  
  liens <- c(liens,l)
}

### Q2 ----
films <- films %>% cbind(data.frame(Lien=liens)) %>% 
  mutate(bon_lien = paste0("https://www.imdb.com/title/",(Lien %>% stringr::str_extract(pattern = "\\&id.*")) %>% substr(5,10000),"/","/fullcredits/?ref_=tt_cst_sm"))

## Q3 ----

acteurs <- c()
for (i in seq(1,nrow(films),1)) {
  
  a <- read_html(films$bon_lien[i]) %>% html_nodes("div.sc-cd7dc4b7-7.vCane > a") %>% html_text()
  
  acteurs <- c(acteurs,a)
}

## Q4 ----
table(acteurs) %>% sort()

