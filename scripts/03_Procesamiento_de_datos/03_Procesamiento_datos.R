# PROCESAMIENTO DE DATOS
# Una vez comprendidas las variables presentes en los datos, se procede al procesamiento de estos
# Se utilizó el codigo enseñado en clases 
# Se usó chatgpt para depurar, ordenar, modificar y optimizar los códigos propuestos inicialmente

# Cargar librerías necesarias
library(readxl)
library(dplyr)
library(lubridate)
library(writexl)

# ============================================================================
# 1. PREPARACIÓN DE TIMESTAMPS
# ============================================================================
# Convertir columna fecha a formato fecha 
convertir_fecha <- function(x) {
  fecha <- as.POSIXct(x, format = "%d-%m-%Y", tz = "UTC")
    error = function(e) NULL
  return(fecha)
}

# ============================================================================
# 2. UNIÓN DE DATOS POR TIMESTAMPS
# ============================================================================
datos_completos <- merge(estacion, teros, by = "timestamps", all = TRUE)

# Identificar columnas de variables meteorológicas y CE
columnas_meteo <- c("mm_precipitation", "m_s_wind_speed", "c_air_temperature")
columnas_ce <- c("m_s_cm_ec")

# ============================================================================
# 3. ELIMINACIÓN DE FILAS SIN DATOS
# ============================================================================
# Contar NAs por fila
datos_completos$na_count_meteo <- rowSums(is.na(datos_completos[, columnas_meteo, drop = FALSE]))
datos_completos$na_count_ce <- rowSums(is.na(datos_completos[, columnas_ce, drop = FALSE]))

# Eliminar filas donde:
# 1. No hay datos de CE (todas las columnas de CE son NA)
# 2. No hay datos de variables meteorológicas (todas son NA)
datos_limpios <- datos_completos %>%
  filter(na_count_ce == 0) %>%  # Al menos una columna de CE tiene datos
  filter(na_count_meteo == 0)  # Al menos una variable meteo tiene datos

# Eliminar columnas auxiliares de conteo
datos_limpios <- datos_limpios %>%
  select(-na_count_meteo, -na_count_ce)

# Verificar porcentaje de datos completos
cat("\n=== RESUMEN DE DATOS COMPLETOS ===\n")
for (col in c(columnas_meteo, columnas_ce)) {
  if (col %in% names(datos_limpios)) {
    pct_completo <- (1 - sum(is.na(datos_limpios[[col]])) / nrow(datos_limpios)) * 100
    cat(sprintf("%s: %.1f%% de datos completos\n", col, pct_completo))
  }
}

# ============================================================================
# 4. AGREGACIÓN DIARIA DE DATOS
# ============================================================================
# Se aplicará sumatoria para precipitación y promedio para el resto de variables
# Precipitación: suma diaria
col_precip <- "mm_precipitation"
# Resto: promedio diario
cols_promedio <- c("c_air_temperature", "m_s_wind_speed", "m_s_cm_ec")

datos_limpios <- datos_limpios%>%
  mutate(fecha = as.Date(timestamps))

# Agregar datos por día
datos_diarios <- datos_limpios %>%
  group_by(fecha) %>%
  summarise(
    # Precipitación: suma diaria
    !!col_precip := sum(!!sym(col_precip), na.rm = TRUE),
    
    # Temperatura: promedio diario
    !!cols_promedio[1] := mean(!!sym(cols_promedio[1]), na.rm = TRUE),
    
    # Velocidad del viento: promedio diario
    !!cols_promedio[2] := mean(!!sym(cols_promedio[2]), na.rm = TRUE),
    
    # CE del suelo: promedio diario
    !!cols_promedio[3] := mean(!!sym(cols_promedio[3]), na.rm = TRUE),
    
    # Contar número de observaciones por día (para validación)
    n_obs = n(),
    
    .groups = "drop"
  ) %>%
    # Eliminar días donde todas las variables son NA o 0 (precipitación puede ser 0)
  filter(
    !is.na(!!sym(cols_promedio[1])) |  # Al menos temperatura tiene datos
    !is.na(!!sym(cols_promedio[2])) |  # O velocidad del viento
    !is.na(!!sym(cols_promedio[3]))    # O CE
  )

# Reemplazar datos_limpios con datos_diarios para el resto del análisis
datos_limpios <- datos_diarios %>%
  select(-n_obs)  # Eliminar columna auxiliar

# Actualizar columnas para el resto del análisis
columnas_meteo <- c(col_precip, cols_promedio[1], cols_promedio[2])
columnas_ce <- cols_promedio[3]

# ============================================================================
# 5. CREAR VARIABLES TEMPORALES PARA ANÁLISIS
# ============================================================================
datos_limpios <- datos_limpios %>%
  mutate(
    Año = year(fecha),
    Mes = month(fecha, label = TRUE, abbr = FALSE),
    Mes_num = month(fecha),
    Estacion = case_when(
      month(fecha) %in% c(12, 1, 2) ~ "Verano",
      month(fecha) %in% c(3, 4, 5) ~ "Otoño",
      month(fecha) %in% c(6, 7, 8) ~ "Invierno",
      month(fecha) %in% c(9, 10, 11) ~ "Primavera"
    ),
    Estacion = factor(Estacion, levels = c("Verano", "Otoño", "Invierno", "Primavera"))
  )

write_xlsx(datos_limpios, "Variables_y_EC.xlsx")
