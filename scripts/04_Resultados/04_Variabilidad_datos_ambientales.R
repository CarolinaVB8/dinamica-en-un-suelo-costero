# ANALISIS DE LA VARIABILIDAD DE LAS VARIABLES METEOROLOGICAS
# Se procedió a realizar gráficos boxplot por cada variable meteorologica
# Se utilizó el codigo enseñado en clases 
# Se usó chatgpt para depurar, ordenar, modificar y optimizar los códigos propuestos inicialmente

# Librerías
library(dplyr)
library(ggplot2)
library(lubridate)
library(gridExtra)
library(rlang)
library(patchwork)

# ============================================================================
# NOMBRES DE COLUMNAS
# ============================================================================
# Títulos por cada gráfico
labels_info <- list(
  mm_precipitation = list(title = "Variabilidad de la precipitación dentro de cada mes", y = "Precipitación (mm)"),
  m_s_wind_speed   = list(title = "Variabilidad de la velocidad del viento dentro de cada mes", y = "Velocidad del viento (m/s)"),
  c_air_temperature = list(title = "Variabilidad de la temperatura del aire dentro de cada mes", y = "Temperatura del aire (°C)"))

# colores: celeste (precip), naranjo (temp), verde (viento)
col_map <- c(mm_precipitation = "#7fc9ff",  # celeste
             c_air_temperature = "#ff9f40", # naranjo
             m_s_wind_speed = "#6bbf59")    # verde

# Parámetros del quiebre, con el fin de ver un gráfico más compacto (en este caso, precipitacion presenta un outlier de 92.359 que distorsiona todo el gráfico)
quiebre_limite <- 25 # número donde comienza el quiebre
factor_compresion <- 0.05 
max_precipitacion <- 95  # valor máximo del eje Y para precipitación

# ============================================================================
# PREPARAR DATOS: asegurar columnas temporales necesarias
# ============================================================================
datos_limpios <- datos_limpios %>%
  mutate(
    Año = year(fecha),
    Mes_num = month(fecha),
    Mes_Año = paste0(Año, "-", sprintf("%02d", Mes_num)),
    Mes = month(fecha, label = TRUE, abbr = FALSE))

# ============================================================================
# FUNCIÓN BOXPLOT MENSUAL (usa quiebre solo si quiebre = TRUE)
# ============================================================================
crear_boxplot_mensual <- function(datos, variable, titulo, etiqueta_y,
                                  quiebre = FALSE, quiebre_limite = 25, factor_compresion = 0.05, max_y = NULL, fill_color = "steelblue") {
  var_sym <- sym(variable)
  
  datos_filtrados <- datos %>%
    filter(!is.na(!!var_sym), !is.na(Mes_Año))
  
  if (nrow(datos_filtrados) == 0) return(NULL)
  
  # Meses ordenados y etiquetas (solo nombre del mes, sin año)
  meses <- datos_filtrados %>%
    select(Mes_Año, Mes_num, Año, Mes) %>%
    distinct() %>%
    arrange(Año, Mes_num) %>%
    mutate(
      Mes_factor = factor(Mes_Año, levels = unique(Mes_Año), ordered = TRUE),
      Etiqueta_mes = as.character(Mes))
  
  datos_filtrados <- datos_filtrados %>%
    left_join(meses, by = c("Mes_Año", "Mes_num", "Año", "Mes")) %>%
    mutate(Mes_factor = factor(Mes_Año, levels = levels(meses$Mes_factor), ordered = TRUE))
  
  # Conteo por mes para etiquetas X
  n_obs <- datos_filtrados %>%
    group_by(Mes_Año, Etiqueta_mes) %>%
    summarise(n = n(), .groups = "drop") %>%
    arrange(match(Mes_Año, levels(meses$Mes_factor)))
  etiquetas_x <- paste0(n_obs$Etiqueta_mes, "\n(n=", n_obs$n, ")")
  
  # Si hay quiebre: transformar valores
  if (quiebre) {
    datos_filtrados <- datos_filtrados %>%
      mutate(valor_original = !!var_sym,
             valor_transformado = ifelse(valor_original <= quiebre_limite,
                                         valor_original,
                                         quiebre_limite + 1 + (valor_original - quiebre_limite) * factor_compresion))
    y_var <- "valor_transformado"
    
    # Si se especifica max_y, usar ese valor transformado; si no, usar el máximo real
    if (!is.null(max_y)) {
      y_top <- ifelse(max_y <= quiebre_limite,
                      max_y,
                      quiebre_limite + 1 + (max_y - quiebre_limite) * factor_compresion) * 1.05
    } else {
      y_top <- max(datos_filtrados$valor_transformado, na.rm = TRUE) * 1.05
    }
  } else {
    datos_filtrados <- datos_filtrados %>% mutate(valor_original = !!var_sym)
    y_var <- "valor_original"
    y_top <- max(datos_filtrados$valor_original, na.rm = TRUE) * 1.05}
  
  # Precipitación acumulada mensual (si corresponde)
  anot_precip <- NULL
  if (variable == "mm_precipitation" && "mm_precipitation" %in% names(datos)) {
    anot_precip <- datos_filtrados %>%
      group_by(Mes_Año, Etiqueta_mes) %>%
      summarise(Precip_acum = sum(mm_precipitation, na.rm = TRUE), .groups = "drop") %>%
      arrange(match(Mes_Año, levels(meses$Mes_factor)))}
  
  # Medianas por mes (sobre valores originales)
  medianas <- datos_filtrados %>%
    group_by(Mes_Año) %>%
    summarise(med = median(valor_original, na.rm = TRUE), .groups = "drop")
  
  # Construir ggplot
  p <- ggplot(datos_filtrados, aes(x = Mes_factor, y = .data[[y_var]])) +
    geom_boxplot(fill = fill_color, alpha = 0.7,
                 outlier.size = 1.5, outlier.alpha = 0.6,
                 outlier.color = "red", outlier.stroke = 0.5) +
    labs(title = titulo,
         subtitle = paste0("Período: ", min(datos_filtrados$fecha), " a ", max(datos_filtrados$fecha)),
         x = "Mes", y = etiqueta_y) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
          plot.subtitle = element_text(hjust = 0.5, size = 14, color = "gray40", face = "italic"),
          axis.text.x = element_text(angle = 0, hjust = 0.5, size = 14, face = "bold"),
          axis.text.y = element_text(size = 13),
          axis.title = element_text(size = 14, face = "bold"),
          panel.grid.major.x = element_blank(),
          panel.grid.minor = element_blank(),
          plot.margin = margin(t = 5, r = 5, b = 25, l = 5)) +
    scale_x_discrete(labels = etiquetas_x, drop = FALSE) +
    ylim(0, y_top)
  
  # Mediana como texto
  p <- p + geom_text(data = medianas, aes(x = Mes_Año, y = y_top * 0.99, label = paste0("Med=", round(med,2))),
                     inherit.aes = FALSE, size = 4, color = "darkblue", vjust = 1)
  
  # Líneas y marcas de quiebre (si aplica)
  if (quiebre) {
    # Usar max_y si está especificado, si no usar el máximo real
    max_real <- ifelse(!is.null(max_y), max_y, max(datos_filtrados$valor_original, na.rm = TRUE))
    
    # Crear breaks hasta max_real
    breaks_inferior <- seq(0, quiebre_limite, by = 5)
    breaks_superior <- seq(95, max_real, by = 5)
    
    # Transformar breaks superiores
    breaks_superior_transformados <- quiebre_limite + 1 + (breaks_superior - quiebre_limite) * factor_compresion
    
    # Labels correspondientes
    labels_inferior <- as.character(breaks_inferior)
    labels_superior <- as.character(breaks_superior)
    
    p <- p +
      scale_y_continuous(
        limits = c(0, y_top),
        breaks = c(breaks_inferior, breaks_superior_transformados),
        labels = c(labels_inferior, labels_superior)
      ) +
      # Líneas de quiebre
      annotate("segment",
               x = -Inf, xend = Inf,
               y = quiebre_limite + 1, yend = quiebre_limite + 1,
               linetype = "dashed", color = "grey", size = 1.2) +
      annotate("segment",
               x = -Inf, xend = Inf,
               y = quiebre_limite, yend = quiebre_limite,
               linetype = "dashed", color = "grey", size = 1.2)
    }
  
  # Anotación de precip acumulada por mes (arriba)
  if (!is.null(anot_precip) && nrow(anot_precip) > 0) {
    pos_y <- y_top * 1
    for (i in seq_len(nrow(anot_precip))) {
      x_idx <- which(levels(datos_filtrados$Mes_factor) == anot_precip$Mes_Año[i])
      if (length(x_idx)) {
        p <- p + annotate("text", x = x_idx, y = pos_y,
                          label = paste0("\u03A3=", round(anot_precip$Precip_acum[i],1), " mm"),
                          size = 4, color = "darkgreen", fontface = "bold", vjust = 0)
      }
    }
  }
  
  return(p)
}

# ============================================================================
# GENERAR PLOTS SEGUN COLUMNAS DISPONIBLES
# ============================================================================
plots_mensuales <- list()
for (var in columnas_meteo) {
  if (!var %in% names(datos_limpios)) next
  info <- labels_info[[var]]
  if (is.null(info)) info <- list(title = var, y = var)
  color <- col_map[[var]] %||% "steelblue"
  
  if (var == "mm_precipitation") {
    p <- crear_boxplot_mensual(datos_limpios, var, info$title, info$y,
                               quiebre = TRUE, quiebre_limite = quiebre_limite, 
                               factor_compresion = factor_compresion, max_y = max_precipitacion, fill_color = color)
  } else {
    p <- crear_boxplot_mensual(datos_limpios, var, info$title, info$y, quiebre = FALSE, fill_color = color)
  }
  
  if (!is.null(p)) plots_mensuales[[var]] <- p
}

# ============================================================================
# COMBINAR Y GUARDAR FIGURA
# ============================================================================
if (length(plots_mensuales) > 0) {
  plots_ordered <- plots_mensuales[intersect(columnas_meteo, names(plots_mensuales))]
  figura_final <- do.call(grid.arrange, c(plots_ordered, ncol = 1))
  ggsave("Figura1_Boxplots_Mensuales.png", figura_final, width = 14, height = 6 * length(plots_ordered), dpi = 300)
} else {
  message("No se generaron plots: revisa que datos_limpios tenga las columnas y datos.")
}
