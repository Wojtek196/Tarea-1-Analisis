library(ggplot2)
library(dplyr)

# 1. Crear variable de distancia a la RM
Fuente_graficos <- Base_Regresion %>%
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
  )

# 2. Calcular el modelo general
modelo_distancia <- lm(MEJOR_M2 ~ Distancia_RM, data = Fuente_graficos)
pendiente <- round(coef(modelo_distancia)["Distancia_RM"], 3)

# 3. Gráfico 1: Dispersión General (Línea Recta)
grafico_principal <- ggplot(Fuente_graficos, aes(x = Distancia_RM, y = MEJOR_M2)) +
  geom_point(alpha = 0.05, color = "darkgray") + 
  geom_smooth(method = "lm", color = "firebrick", se = TRUE, linewidth = 1) +
  labs(
    title = "Relación entre Distancia a la RM y Rendimiento en PAES M2",
    subtitle = paste0("Pendiente estimada: ", pendiente, " puntos por kilómetro"),
    x = "Distancia a la Región Metropolitana (km)",
    y = "Puntaje M2"
  ) +
  theme_classic() + 
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.text = element_text(color = "black")
  )

print(grafico_principal)

# 4. Gráfico 2: Particionado por Rama Educacional
datos_grafico2 <- Fuente_graficos %>% filter(RAMA_EDUCACIONAL != "Sin Información")

# Calcular la pendiente para cada panel automáticamente
pendientes_rama <- datos_grafico2 %>%
  group_by(RAMA_EDUCACIONAL) %>%
  summarise(
    pendiente = round(coef(lm(MEJOR_M2 ~ Distancia_RM))["Distancia_RM"], 3),
    .groups = 'drop'
  ) %>%
  mutate(texto_pendiente = paste0("Pendiente: ", pendiente))

grafico_particionado <- ggplot(datos_grafico2, aes(x = Distancia_RM, y = MEJOR_M2)) +
  geom_point(alpha = 0.05, color = "darkgray") +
  geom_smooth(method = "lm", color = "navy", se = TRUE, linewidth = 1) +
  
  # Inyectar el texto de la pendiente en cada panel
  geom_text(
    data = pendientes_rama, 
    aes(x = Inf, y = Inf, label = texto_pendiente), 
    hjust = 1.1,  
    vjust = 1.5,  
    color = "navy", 
    fontface = "bold", 
    inherit.aes = FALSE 
  ) +
  
  facet_wrap(~ RAMA_EDUCACIONAL) + 
  labs(
    title = "Efecto de la Distancia en PAES M2 según Rama Educacional",
    subtitle = "Interacción entre territorio escolar y tipo de formación",
    x = "Distancia a la Región Metropolitana (km)",
    y = "Puntaje M2"
  ) +
  theme_bw() + 
  theme(
    panel.grid.minor = element_blank(),
    strip.background = element_rect(fill = "white", color = "black"),
    strip.text = element_text(face = "bold")
  )

print(grafico_particionado)

# 5. Exportación en Máxima Calidad

# Opción A: PNG a 600 DPI (Súper Alta Resolución para imágenes)
ggsave("Grafico_Principal_600dpi.png", plot = grafico_principal, width = 10, height = 6, dpi = 600)
ggsave("Grafico_Particionado_600dpi.png", plot = grafico_particionado, width = 12, height = 8, dpi = 600)


print("Listo")