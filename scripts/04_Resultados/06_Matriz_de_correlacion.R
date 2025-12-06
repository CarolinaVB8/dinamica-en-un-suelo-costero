# MATRIZ DE CORRELACIÓN
# Se usó stackoverflow y chatgpt para desarrollar el código
# Se utilizó chatgpt para depurar, ordenar, modificar y optimizar los códigos propuestos inicialmente

library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)

# ============================================================================
# PREPARAR DATOS
# ============================================================================
vars_interes <- c("mm_precipitation", "m_s_wind_speed", "c_air_temperature", "m_s_cm_ec")

# Extraer datos (ASUMIMOS que no hay NA en datos_diarios) ----
df_corr <- datos_diarios %>% select(any_of(vars_interes)) %>% mutate(across(everything(), ~ as.numeric(.)))

# Chequeo (si hay NA abortamos porque pediste no usar pairwise) 
if (any(is.na(df_corr))) stop("Se detectaron NA en 'df_corr'. Este script usa 'complete.obs' (no pairwise). Revisa tus datos.")

# Imprimir info básica
cat("Filas en datos_diarios:", nrow(datos_diarios), "\n")
cat("Filas usadas para correlación (sin NA):", nrow(df_corr), "\n\n")

# ============================================================================
# CORRELACION USANDO complete.obs 
# ============================================================================
metodo <- "pearson"
corr_mat <- cor(df_corr, use = "complete.obs", method = metodo)

# p-values: cor.test sobre los vectores completos (sin NA)
vars <- colnames(df_corr)
n <- length(vars)
p_mat <- matrix(NA, nrow = n, ncol = n, dimnames = list(vars, vars))
for (i in seq_len(n)) {
  for (j in seq_len(n)) {
    if (i == j) {
      p_mat[i, j] <- 0
    } else {
      xi <- df_corr[[vars[i]]]
      xj <- df_corr[[vars[j]]]
      # cor.test en vectores completos (sin NA por la verificación anterior)
      test <- tryCatch(cor.test(xi, xj, method = metodo), error = function(e) NULL)
      p_mat[i, j] <- if (!is.null(test)) test$p.value else NA
    }
  }
}

# Mostrar correlaciones y p-values respecto a CE
cat("Correlaciones (", metodo, ") con m_s_cm_ec:\n", sep = "")
print(round(corr_mat["m_s_cm_ec", ], 3))
cat("\nP-values (m_s_cm_ec vs):\n")
print(round(p_mat["m_s_cm_ec", ], 4))
cat("\n")

# Cambio de nombres
nice_names <- c(
  mm_precipitation  = "Precipitación (mm)",
  m_s_wind_speed    = "Velocidad del Viento (m/s)",
  c_air_temperature = "Temperatura del Aire (°C)",
  m_s_cm_ec         = "Conductividad (mS/cm)"
)

common_vars <- intersect(colnames(corr_mat), names(nice_names))
corr_mat_named <- corr_mat[common_vars, common_vars, drop = FALSE]
p_mat_named    <- p_mat[common_vars, common_vars, drop = FALSE]
rownames(corr_mat_named) <- colnames(corr_mat_named) <- nice_names[common_vars]
rownames(p_mat_named)    <- colnames(p_mat_named)    <- nice_names[common_vars]

# Clustering jerárquico para orden (basado en |r|)
temp <- corr_mat_named
dist_mat <- as.dist(1 - abs(temp))   # distancia = 1 - |r|
hc <- hclust(dist_mat)
vars_ordered <- hc$labels[hc$order]

# Construir dataframe para ggplot (triángulo inferior)
corr_df <- as.data.frame(corr_mat_named) %>%
  tibble::rownames_to_column(var = "Var1") %>%
  pivot_longer(-Var1, names_to = "Var2", values_to = "r") %>%
  mutate(
    Var1 = factor(Var1, levels = vars_ordered),
    Var2 = factor(Var2, levels = vars_ordered),
    p_value = as.vector(p_mat_named[cbind(as.character(Var1), as.character(Var2))])
  ) %>%
  filter(as.integer(Var1) > as.integer(Var2))   # mantener sólo triángulo inferior

# Etiquetas y asteriscos de significancia
corr_df <- corr_df %>%
  mutate(
    r_label = ifelse(is.na(r), "", sprintf("%.2f", r)),
    signif = case_when(
      is.na(p_value) ~ "",
      p_value < 0.001 ~ "***",
      p_value < 0.01  ~ "**",
      p_value < 0.05  ~ "*",
      TRUE            ~ ""
    )
  )

# Paleta y plot
pal <- c("#b2182b", "white", "#2166ac")  # rojo-blanco-azul

p_heat <- ggplot(corr_df, aes(x = Var2, y = Var1)) +
  geom_tile(aes(fill = r), color = "grey80", width = 0.95, height = 0.95) +
  scale_fill_gradient2(low = pal[1], mid = pal[2], high = pal[3],
                       midpoint = 0, limits = c(-1, 1), oob = scales::squish, name = "") + # name="" elimina "Corr"
  geom_text(aes(label = r_label), size = 4, color = "black") +
  geom_text(aes(label = signif), size = 5, color = "black", vjust = -1.2) + # asteriscos arriba del número
  coord_fixed() +
  labs(title = paste0("Matriz de correlación (", toupper(metodo), ")")) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.title = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
    axis.text.y = element_text(face = "bold"),
    panel.grid = element_blank(),
    legend.position = "right"
  )

# Guardar
ggsave("04_Corr_Heatmap_CompleteObs.png", p_heat, width = 8, height = 6, dpi = 300)
