# ==============================================================================
# ANÁLISIS EXPLORATORIO DE DATOS (EDA) Y PREPARACIÓN
# ==============================================================================

# 1. CARGA DE BIBLIOTECAS ------------------------------------------------------
library(dplyr)
library(stringr)
library(readxl)
library(ggplot2)
library(skimr)
library(DataExplorer)

# (Opcional) Usar skimr o DataExplorer para un reporte automático rápido
# skim(Fuente)
# create_report(Fuente)

# 2. DIMENSIONES Y ESTRUCTURA --------------------------------------------------
cat("\n=== 2.1 DIMENSIONES ===\n")
dim(Fuente)

cat("\n=== 2.2 TIPOS DE DATOS ===\n")
df_tipos <- data.frame(
  Variable = names(Fuente),
  Tipo     = sapply(Fuente, class)
)
print(df_tipos)

# 3. ANÁLISIS DE CALIDAD DE DATOS ----------------------------------------------

cat("\n=== 3.1 VALORES NULOS ===\n")
resumen_nulos <- data.frame(
  Variable     = names(Fuente),
  N_completos  = sapply(Fuente, function(x) sum(!is.na(x))),
  N_nulos      = sapply(Fuente, function(x) sum(is.na(x))),
  Pct_completo = sapply(Fuente, function(x) round(mean(!is.na(x)) * 100, 2)),
  Pct_nulo     = sapply(Fuente, function(x) round(mean(is.na(x))  * 100, 2))
) %>%
  arrange(desc(Pct_nulo))
print(resumen_nulos)

cat("\n=== 3.2 DUPLICADOS ===\n")
cat("Filas completamente duplicadas:", sum(duplicated(Fuente)), "\n")
cat("ID_aux duplicados (postulaciones múltiples):", sum(duplicated(Fuente$ID_aux)), "\n")
cat("ID_aux únicos:", n_distinct(Fuente$ID_aux), "\n")
cat("Combinaciones únicas (ID_aux + COD_CARRERA_PREF):", n_distinct(Fuente$ID_aux, Fuente$COD_CARRERA_PREF), "\n")

# Investigar tuplas duplicadas específicas
tuplas_duplicadas <- Fuente %>%
  group_by(ID_aux, COD_CARRERA_PREF) %>%
  filter(n() > 1) %>%
  arrange(ID_aux, COD_CARRERA_PREF) %>%
  head(20)

# 4. TABLAS RESUMEN (NUMÉRICAS Y CATEGÓRICAS) ----------------------------------

cat("\n=== 4.1 RESUMEN VARIABLES NUMÉRICAS ===\n")
tabla_numerica <- Fuente %>%
  select(where(is.numeric)) %>%
  lapply(function(x) {
    Q1  <- quantile(x, 0.25, na.rm = TRUE)
    Q3  <- quantile(x, 0.75, na.rm = TRUE)
    IQR <- Q3 - Q1
    
    data.frame(
      N_completos  = sum(!is.na(x)),
      Pct_nulo     = round(mean(is.na(x)) * 100, 1),
      Media        = round(mean(x, na.rm = TRUE), 1),
      Mediana      = round(median(x, na.rm = TRUE), 1),
      SD           = round(sd(x, na.rm = TRUE), 1),
      Min          = min(x, na.rm = TRUE),
      Max          = max(x, na.rm = TRUE),
      N_outliers   = sum(x < (Q1 - 1.5*IQR) | x > (Q3 + 1.5*IQR), na.rm = TRUE)
    )
  }) %>%
  bind_rows(.id = "Variable")
print(tabla_numerica)

cat("\n=== 4.2 RESUMEN VARIABLES CATEGÓRICAS ===\n")
tabla_categorica <- Fuente %>%
  select(where(is.character)) %>%
  lapply(function(x) {
    frec  <- sort(table(x, useNA = "no"), decreasing = TRUE)
    pct   <- round(prop.table(frec) * 100, 1)
    top_n <- min(5, length(frec))
    
    data.frame(
      N_completos  = sum(!is.na(x)),
      Pct_nulo     = round(mean(is.na(x)) * 100, 1),
      N_categorias = length(frec),
      Top_valores  = paste(names(frec)[1:top_n], paste0("(", pct[1:top_n], "%)"), collapse = " | ")
    )
  }) %>%
  bind_rows(.id = "Variable")
print(tabla_categorica)

# 5. VISUALIZACIONES PRINCIPALES -----------------------------------------------

# 5.1 Distribución Puntaje M2
hist_M2 <- ggplot(Fuente, aes(x = MEJOR_M2)) +
  geom_histogram(bins = 40, fill = "steelblue", color = "white") +
  labs(title = "Distribución puntaje PAES M2", x = "Puntaje", y = "Frecuencia") +
  theme_minimal()
print(hist_M2)

# 5.2 Boxplot por Región
boxplot_region <- ggplot(Fuente, aes(x = reorder(CODIGO_REGION_D, MEJOR_M2, median), y = MEJOR_M2)) +
  geom_boxplot(fill = "steelblue") +
  coord_flip() +
  labs(title = "Puntaje PAES M2 por Región", x = "Región", y = "Puntaje") +
  theme_minimal()
print(boxplot_region)

# ============================================================
# TRATAMIENTO DE NULOS Y PREPARACIÓN FINAL
# ============================================================

Fuente_Final <- Fuente %>%
  # 1. Eliminar filas completamente duplicadas
  distinct() %>%
  
  # 2. Manejo de Nulos en Categóricas: Crear categoría "Sin Información"
  mutate(
    across(where(is.character), ~ coalesce(., "Sin Información"))
  ) %>%
  
  # 3. Limpiar Módulo de Ciencias
  # (Si tu variable se llama distinto, cambia "MODULO_CIENCIAS" por el nombre real)
  mutate(
    MODULO_MEJOR_CIENCIA = if_else(MODULO_MEJOR_CIENCIA %in% c("FIS", "BIO", "QUI", "TEC"), 
                              MODULO_MEJOR_CIENCIA, 
                              "Sin Información")
  ) %>%
  
  # 4. Convertir caracteres a factores
  mutate(across(where(is.character), as.factor)) %>%
  
  # 5. Crear variable de distancia a la RM
  mutate(
    Distancia_RM = case_when(
      CODIGO_REGION_D == "Región Metropolitana de Santiago" ~ 0,
      CODIGO_REGION_D == "Valparaíso" ~ 116,
      CODIGO_REGION_D == "Región del Libertador Gral. Bernardo O'Higgins" ~ 87,
      CODIGO_REGION_D == "Región del Maule" ~ 258,
      CODIGO_REGION_D == "Región del Biobío" ~ 500,
      CODIGO_REGION_D == "Región de la Araucanía" ~ 680,
      CODIGO_REGION_D == "Región de Los Ríos" ~ 840,
      CODIGO_REGION_D == "Región de Los Lagos" ~ 1000,
      CODIGO_REGION_D == "Región Aisén del Gral. Carlos Ibáñez del Campo" ~ 1600,
      CODIGO_REGION_D == "Región de Magallanes y de la Antártica Chilena" ~ 2200,
      CODIGO_REGION_D == "Coquimbo" ~ 460,
      CODIGO_REGION_D == "Atacama" ~ 800,
      CODIGO_REGION_D == "Antofagasta" ~ 1330,
      CODIGO_REGION_D == "Tarapacá" ~ 1800,
      CODIGO_REGION_D == "Arica y Parinacota" ~ 2000,
      TRUE ~ NA_real_
    )
  ) %>%
  
  # 6. Eliminar los nulos generados en la variable Distancia_RM
  filter(!is.na(Distancia_RM))

# Verificar la base final para el modelamiento
cat("\n=== BASE FINAL LISTA ===\n")
dim(Fuente_Final)
sum(is.na(Fuente_Final))