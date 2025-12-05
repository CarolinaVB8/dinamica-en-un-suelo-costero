#CARGA DE DATOS
# Se utiliza librería readxl ya que contamos con datos .xlsx

#Cargar librerias
library(readxl)

#Cargar datos
estacion <- read_xlsx("datos/raw/Estacion_meteorologica.xlsx")
teros <- read_xlsx("datos/raw/Teros-12_20cm.xlsx")

