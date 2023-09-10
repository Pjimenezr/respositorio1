

urls <- c("https://www.ine.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2021/esi-2021---personas.csv?sfvrsn=d03ae552_4&download=true",
          "https://www.ine.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2020/esi-2020---personas.csv?sfvrsn=fb1f7e0c_4&download=true",
          "https://www.ine.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2019/esi-2019---personas.csv?sfvrsn=9eb52870_8&download=true",
          "https://www.ine.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2018/esi-2018---personas.csv?sfvrsn=a5de2b27_6&download=true",
          "https://www.ine.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2017/1esi-2017---personas.csv?sfvrsn=d556c5a1_6&download=true",
          "https://www.ine.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2016/esi-2016---personas.csv?sfvrsn=81beb5a_6&download=true"
)


setwd("/Users/Viviana/Desktop/prueba_repositorio/respositorio1/Emilia_Jimenez_Garrido")
extract_name <- function(url) {
  nombre_archivo_sin_numeros <- sub("^\\d+", "", nombre_archivo_sin_parametros)
  return(nombre_archivo_sin_numeros)
}



# Instala y carga la librería 'purrr' si aún no está instalada
if (!requireNamespace("purrr", quietly = TRUE)) {
  install.packages("purrr")
}

library(purrr)

# Define la función extract_name para extraer el nombre del archivo de una URL
extract_name <- function(url) {
  nombre_archivo <- basename(url)
  nombre_archivo_sin_parametros <- sub("\\?.*$", "", nombre_archivo)
  return(nombre_archivo_sin_parametros)
}


# Usa purrr para aplicar extract_name a todas las URLs y almacenar los nombres de los archivos
file_names <- map_chr(urls, extract_name)

# Muestra los nombres de los archivos
print(file_names)



extract_name <- function(url) {
  nombre_archivo <- basename(url)
  nombre_archivo_sin_parametros <- sub("\\?.*$", "", nombre_archivo)
  nombre_archivo_sin_numeros <- sub("^\\d+", "", nombre_archivo_sin_parametros)
  return(nombre_archivo_sin_numeros)
}



# Carga la biblioteca necesaria para trabajar con URLs
library(readr)

# URL del archivo CSV que deseas leer
url <- urls[5]
# Utiliza la función read_csv() o read_csv2() para leer el archivo desde la URL
datos <- read_csv(url)



# descargar bases ---------------------------------------------------------

download_esi_data <- function(url, file_name, directory) {
  # Combinar el directorio con el nombre de archivo para obtener la ruta completa
  file_path <- file.path(directory, file_name)
  
  # Descargar el archivo desde la URL y guardarlo en la ruta especificada
  download.file(url, destfile = file_name, mode = "wb")
  
  # Verificar si la descarga fue exitosa
  if (file.exists(file_path)) {
    cat("Archivo descargado exitosamente:", file_path, "\n")
  } else {
    cat("Error al descargar el archivo desde la URL:", url, "\n")
  }
}
getwd()
# Ejemplo de uso de la función download_esi_data
url <-"https://www.ine.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2021/esi-2021---personas.csv"
file_name <- "esi_2021_personas.csv"
directory <-"/Users/Viviana/Desktop/prueba_repositorio/respositorio1/Emilia_Jimenez_Garrido"
download_esi_data(url, file_name, directory)
Asegúrate de reemplazar "tu_directorio_de_destino" con la ruta del directorio 
donde deseas guardar el archivo descargado. Esta función toma la URL, el 
nombre de archivo y el directorio como parámetros y descarga el archivo desde 
la URL a la ubicación especificada. También verifica si la descarga fue exitosa
o si se produjo algún error.

download.file("https://www.ine.gob.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2021/esi-2021---personas.csv?sfvrsn=d03ae552_6&download=true", destfile = file_name)

download.file(
  url = "https://archive.ics.uci.edu/ml/machine-learning-databases/iris/iris.data", 
  destfile = "iris.data"
)
https://www.ine.gob.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2021/esi-2021---personas.csv?sfvrsn=d03ae552_6&download=true
https://www.ine.gob.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2021/esi-2021---personas.csv?sfvrsn=d03ae552_6&download=true




# Instalar y cargar el paquete 'httr' si aún no está instalado
if (!requireNamespace("httr", quietly = TRUE)) {
  install.packages("httr")
}

library(httr)

url <- "https://www.ine.gob.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2021/esi-2021---personas.csv?sfvrsn=d03ae552_6&download=true"
file_name <- "esi-2021---personas.csv"
directory <- "tu_directorio_de_destino"  # Reemplaza con el directorio donde deseas guardar el archivo

response <- GET(url, config(ssl_verifypeer = FALSE))
if (http_status(response)$status_code == 200) {
  writeBin(content(response, "raw"), file.path(directory, file_name))
  cat("Archivo descargado exitosamente en:", file.path(directory, file_name), "\n")
} else {
  cat("Error al descargar el archivo desde la URL:", url, "\n")
}



