library(guaguas)
library(stringr)
library(dplyr)
library(purrr)
guaguas %>% filter(str_detect(nombre,pattern="anari")) %>% 
  count(nombre)

guaguas$sexo
guaguas %>% filter(sexo=="F" & anio==1989 & str_detect(nombre,pattern="o$")) %>% 
  count(nombre)

guaguas %>% filter(sexo=="M" & anio==1989 & str_detect(nombre,pattern="a$")) %>% 
  count(nombre)

guaguas %>% filter(anio==1989 & str_detect(nombre,pattern="(e|é)$")) %>% 
  count(nombre)

list_ano<-split(guaguas,guaguas$anio)
create_first_last=function(df,var){
  df %>% 
    mutate(first_letters = str_sub({{var}},1,2),
           last_letters = str_sub({{var}},-2,-1))
}
map(list_ano,create_first_last,nombre)
