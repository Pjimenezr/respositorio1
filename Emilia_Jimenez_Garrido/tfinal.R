library(dplyr)
library(xfun)
##
urls <- c("https://www.ine.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2021/esi-2021---personas.csv?sfvrsn=d03ae552_4&download=true",
          "https://www.ine.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2020/esi-2020---personas.csv?sfvrsn=fb1f7e0c_4&download=true",
          "https://www.ine.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2019/esi-2019---personas.csv?sfvrsn=9eb52870_8&download=true",
          "https://www.ine.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2018/esi-2018---personas.csv?sfvrsn=a5de2b27_6&download=true",
          "https://www.ine.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2017/1esi-2017---personas.csv?sfvrsn=d556c5a1_6&download=true",
          "https://www.ine.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2016/esi-2016---personas.csv?sfvrsn=81beb5a_6&download=true")

##modificamos las url
## Elimina el "1" antes de "esi-2017"
urls <- gsub("1esi", "esi", urls)

## función extraer nombre de archivos desde una url
extract_name <- function(url) {
  nombre_archivo <- basename(url)
  nombre_archivo_sin_parametros <- sub("\\?.*$", "", nombre_archivo)
  nombre_archivo_sin_numeros <- sub("^\\d+", "", nombre_archivo_sin_parametros)
  return(nombre_archivo_sin_numeros)
}

## vector con nombres de archivos que descargaremos
file_names <- map_chr(urls, extract_name)

## función descargar archivo desde url 
download_esi_data <- function(url, file_name, directory) {
  
  # Construye la ruta completa del archivo de destino
  destination_path <- file.path(directory, file_name)
  
  # Descarga el archivo desde la URL y guárdalo en el directorio de destino
  download_file(url, output = destination_path)
  
  # Verifica si la descarga fue exitosa
  if (file.exists(destination_path)) {
    cat("Descarga completada. El archivo se guardó en:", destination_path, "\n")
  } else {
    cat("La descarga falló. Por favor, verifica la URL y el directorio de destino.", "\n")
  }
}

# Ejemplo de uso de la función download_esi_data
url <- urls[1]
file_name <- file_names[1]
directory <- "/Users/Viviana/Desktop/prueba_repositorio/respositorio1/Emilia_Jimenez_Garrido"

download_esi_data(url, file_name, directory)
response <- httr::GET("https://www.ejemplo.com", config = my_ssl_config)

library(purrr)
carpeta<-dir.create("data")
funcion_descarga<-function(urls){
  nombre_bases<-extract_name(urls)
  directory<-file.path(getwd(), "data")
  descargar<-download_esi_data(urls,nombre_bases,directory)
}

descarga<-map(urls,funcion_descarga)



#####################
read_esi_data <- function(ruta_archivo) {
  library(data.table)
  
  # Intenta leer el archivo con diferentes delimitadores
  delimitadores <- c(",", ";", "\t")  # Coma, punto y coma, tabulador
  
  for (delimitador in delimitadores) {
    datos <- try(fread(ruta_archivo, sep = delimitador), silent = TRUE)
    if (!inherits(datos, "try-error")) {
      return(datos)
    }
  }
  
  stop("No se pudo leer el archivo con ningún delimitador conocido.")
}

read_esi_data("data/esi-2016---personas.csv")

#####Leemos todos los archivos de la carpeta data########
library(dplyr)
library(purrr)
library(stringr)
files_esi<-list.files("data/",full.names = T)
esi<-map(files_esi,~read_esi_data(.x))
nombres<-str_extract_all(files_esi,"(2016|2017|2018|2019|2020|2021)") %>% str_replace_all("---","-")
names(esi)<-paste0("esi_",nombres)
version<-paste0("esi_",nombres)


# EJERCICIO 3 -------------------------------------------------------------

#EJERCICIO 3.1###generamos una nueva variable llamada version
rm(esi_red)
esi<-map2(esi,version,~mutate(.x,version=.y))
select_var<-function(datos){
  datos  %>% 
     select(version,idrph,id_identificacion,fact_cal_esi,estrato,conglomerado,ing_t_p,ocup_ref) %>% 
    mutate(id_identificacion=as.numeric(id_identificacion))
    }
esi1<-map(esi, select_var)
esi_red<- bind_rows(esi1[1:6])
tabla1<-esi_red %>% group_by(version) %>% summarise(n=sum(id_identificacion),h=sum(idrph))

tabla1<-esi_red %>% group_by(version,id_identificacion) %>%
        summarise(casos=n()) %>%  
        group_by(version) %>% 
        summarise(n_personas=sum(casos),n_hogares=length(id_identificacion))
        
tabla1<-esi_red %>% group_by(version) %>%
        summarise(n_personas=n_distinct(idrph),n_hogares=n_distinct(id_identificacion))

#EJERCICIO 3.2

#Tabla que contenga mínimo, máximo, media, mediana, p10 y p90 del factor de expansión
#(fact_cal_esi) para cada versión. Debes considerar una fila por hogar (id_identificacion) 
#e incluir la columna version ¿Se observan algunos pesos de muestreo atípicos?

tabla2<-esi_red %>% 
        group_by(version) %>% 
        summarise(minimo=min(fact_cal_esi),
                  maximo=max(fact_cal_esi),
                  media=mean(fact_cal_esi),
                  mediana=median(fact_cal_esi),
                  p10=quantile(fact_cal_esi,0.1),
                  p90=quantile(fact_cal_esi,0.9))

#Tabla que contenga el número de estratos (estrato) con una sola unidad primaria de muestro
#(conglomerado). Debes incluir la columna version.

tabla3<-esi_red %>% group_by(version) %>%
  summarise(n_estratos=n_distinct(estrato),n_conglomerados=n_distinct(conglomerado))

#Tabla que contenga mínimo, máximo, media, mediana, p10 y p90 de los ingresos del trabajo
#principal (ing_t_p) para cada versión. Esta tabla debe ser construida a nivel persona,
#utilizando el factor de expansión (fact_cal_esi).

# Cargar el paquete survey
install.packages("survey")
library(survey)

# Crear el objeto de diseño de muestreo
dc <- svydesign(ids = ~conglomerado, strata = ~estrato, weights = ~fact_cal_esi, data = esi_red)
options(survey.lonely.psu = "certainty")
ingresos<-svyby(~ing_t_p,by=~version,design = dc,svymean,na.rm=TRUE)
cuantiles<-svyby(~ing_t_p,by=~version,subset(dc,ocup_ref),svyquantile,quantiles=c(0,0.1,0.5,0.9,1))
