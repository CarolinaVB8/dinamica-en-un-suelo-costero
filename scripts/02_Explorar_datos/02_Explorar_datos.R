#EXPLORAR DATOS
# Fase inicial para conocer qué tipo de datos tenemos en nuestro poder
# Para trabajar los datos fue necesario convertir los valores de EC a numerico
# Se utilizó el codigo enseñado en clases 

# Cargar librerías necesarias
library(readxl)
library(dplyr)
library(lubridate)
library(janitor)

#Explorar datos
glimpse(estacion)
glimpse(teros)

#Ajustar tipo de dato de cada variable (en este caso la EC está en booleano y lo cambiamos a numerico)
teros <- teros %>%
  mutate(mS_cm_EC = as.numeric(`mS/cm Saturation Extract EC`))

#Dejar todos los nombres depurados (más facil su utilización)
estacion <- estacion %>%
  clean_names()
teros <- teros %>%
  clean_names()

#Eliminamos columnas de teros que no usaremos
teros <- teros[,-c(2,3,4,5,6)] #solo nos quedamos con timestamps (fecha y hora) y EC

