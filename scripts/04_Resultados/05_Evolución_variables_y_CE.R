# EVOLUCIÓN DE LAS VARIABLES METEOROLÓGICAS VS CONDUCTIVIDAD ELÉCTRICA
# Se procedió a realizar gráficos de línea por el cruce entre variable con CE
# Se utilizó el codigo enseñado en clases 
# Se usó chatgpt para depurar, ordenar, modificar y optimizar los códigos propuestos inicialmente

#Cargar librerías
library(ggplot2)
library(patchwork)
library(scales)

# ============================================================================
# NOMBRES DE COLUMNAS
# ============================================================================
labels_vars <- list(
  mm_precipitation  = c(nombre = "Precipitación", unidad = "mm", nombre_completo = "Precipitación (mm)"),
  m_s_wind_speed    = c(nombre = "Velocidad del Viento", unidad = "m/s", nombre_completo = "Velocidad del Viento (m/s)"),
  c_air_temperature = c(nombre = "Temperatura del Aire", unidad = "°C", nombre_completo = "Temperatura del Aire (°C)"),
  m_s_cm_ec         = c(nombre = "Conductividad Eléctrica", unidad = "mS/cm", nombre_completo = "Conductividad Eléctrica (mS/cm)"))

get_label <- function(name) {
  lbl <- labels_vars[[name]]
  if (is.null(lbl)) return(c(nombre = name, unidad = "", nombre_completo = name))
  return(lbl)
}

# Mapa de colores fijo solicitado
color_map <- c(
  mm_precipitation  = "#7fc9ff",  # celeste
  m_s_wind_speed    = "#6bbf59",  # verde
  c_air_temperature = "#ff9f40",  # naranja
  m_s_cm_ec         = "#000000"   # negro
)

# ============================================================================
# FUNCIÓN GRÁFICO DE LÍNEAS
# ============================================================================
crear_series_temporales <- function(df, var_meteo, var_ce) {
  
  lbl_m <- get_label(var_meteo)
  lbl_c <- get_label(var_ce)
  
  nombre_m <- as.character(lbl_m["nombre"])
  nombre_c <- as.character(lbl_c["nombre"])
  
  dfp <- df %>%
    select(fecha, !!sym(var_meteo), !!sym(var_ce)) %>%
    arrange(fecha) %>%
    filter(!is.na(!!sym(var_meteo)) | !is.na(!!sym(var_ce)))
  if (nrow(dfp) == 0) return(NULL)
  
  # calcular período para subtítulo
  fecha_min <- format(min(dfp$fecha, na.rm = TRUE), "%Y-%m-%d")
  fecha_max <- format(max(dfp$fecha, na.rm = TRUE), "%Y-%m-%d")
  subtitulo  <- paste0("Período: ", fecha_min, " a ", fecha_max)
  
  # escala
  rango_m <- range(dfp[[var_meteo]], na.rm = TRUE)
  rango_c <- range(dfp[[var_ce]], na.rm = TRUE)
  factor_escala <- ifelse(diff(rango_c) == 0, 1, diff(rango_m) / diff(rango_c))
  offset_ce <- rango_m[1] - rango_c[1] * factor_escala
  
  dfp <- dfp %>%
    mutate(
      var_m_original = !!sym(var_meteo),
      var_ce_escalada = (!!sym(var_ce)) * factor_escala + offset_ce
    )
  
  # Mapa de colores universal para TODAS las figuras
  color_universal <- c(
    "Precipitación"          = "#7fc9ff",
    "Velocidad del Viento"   = "#6bbf59",
    "Temperatura del Aire"   = "#ff9f40",
    "Conductividad Eléctrica"= "#000000"
  )
  
  # Selección de colores para este gráfico
  color_map_local <- color_universal[c(nombre_m, nombre_c)]
  
  # plot
  p <- ggplot(dfp, aes(x = fecha)) +
    
    # meteorológica
    geom_line(aes(y = var_m_original, color = nombre_m), linewidth = 1.3, na.rm = TRUE) +
    
    # CE escalada
    geom_line(aes(y = var_ce_escalada, color = nombre_c), linewidth = 1.1, na.rm = TRUE) +
    
    scale_y_continuous(
      name = lbl_m["nombre_completo"],
      sec.axis = sec_axis(~ (. - offset_ce) / factor_escala,
                          name = lbl_c["nombre_completo"])
    ) +
    
    # Usamos el mismo mapa para TODAS las figuras:
    scale_color_manual(values = color_map_local, drop = TRUE) +
    
    labs(
      title = paste("Comparación:", nombre_m, "vs", nombre_c),
      subtitle = subtitulo,
      x = NULL,
      color = NULL
    ) +
    
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 14, color = "gray40", hjust = 0.5, face = "italic"),
      legend.position = "bottom",
      legend.text = element_text(size = 12),
      axis.text.x = element_text(size = 14),
      panel.grid.minor = element_blank()
    ) +
    
    scale_x_date(
      date_breaks = "1 month",
      date_labels = "%B",
      expand = expansion(add = c(0, 0.5))
    )
  return(p)
}

# ============================================================================
# GENERAR GRÁFICOS
# ============================================================================
graficos_series <- list()
for (v in columnas_meteo) {
  if (!v %in% names(datos_diarios)) {
    warning("Variable faltante: ", v); next
  }
  try({
    p <- crear_series_temporales(datos_diarios, v, columnas_ce)
    if (!is.null(p)) {
      # forzamos una leyenda en una fila (por si el label es largo)
      p <- p + guides(color = guide_legend(nrow = 1, byrow = TRUE))
      # asegurar que la leyenda esté abajo en este panel
      p <- p + theme(legend.position = "bottom", legend.title = element_blank())
      graficos_series[[v]] <- p
    }
  }, silent = TRUE)
}

# ============================================================================
# COMBINAR Y GUARDAR FIGURA
# ============================================================================
plots_ordered <- graficos_series[intersect(columnas_meteo, names(graficos_series))]

panel_completo <- wrap_plots(plots_ordered, ncol = 1)

ggsave("04_Series_Temporales_Superpuestas.png", panel_completo, width = 14, height = 5 * length(plots_ordered), dpi = 300)
