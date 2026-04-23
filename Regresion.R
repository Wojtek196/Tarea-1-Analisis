# ==============================================================================
# SCRIPT DE REGRESIÓN E INFERENCIA ESTADÍSTICA
# ==============================================================================

# 0. Cargar librerías necesarias
library(car)      # Para multicolinealidad (VIF)
library(lmtest)   # Para homocedasticidad (Breusch-Pagan)
library(sandwich) # Para errores estándar robustos
library(stargazer)# Para exportar tablas de resultados

# ==============================================================================
# FASE 1: ESTIMACIÓN PROGRESIVA DE MODELOS
# ==============================================================================

# M1: Efecto Bruto (Solo Distancia)
modelo_1 <- lm(MEJOR_M2 ~ Distancia_RM, data = Fuente_Final)

# M2: Control Escolar (Se agrega Rama Educacional)
modelo_2 <- lm(MEJOR_M2 ~ Distancia_RM + RAMA_EDUCACIONAL, data = Fuente_Final)

# M3: Control Socioeconómico y Género escolar (Se agrega BEA y Régimen)
modelo_3 <- lm(MEJOR_M2 ~ Distancia_RM + RAMA_EDUCACIONAL + BEA + REGIMEN, data = Fuente_Final)

# M4: Control de Afinidad Académica y Experiencia
modelo_4 <- lm(MEJOR_M2 ~ Distancia_RM + RAMA_EDUCACIONAL + BEA + REGIMEN + 
                 MODULO_MEJOR_CIENCIA + RINDIO_PROCESO_ANTERIOR, data = Fuente_Final)

# M5: Modelo con Interacción Socio-Territorial (El más completo)
# ¿Pega más fuerte la distancia si además tienes vulnerabilidad (BEA)?
modelo_5 <- lm(MEJOR_M2 ~ Distancia_RM * BEA + RAMA_EDUCACIONAL + REGIMEN + 
                 MODULO_MEJOR_CIENCIA + RINDIO_PROCESO_ANTERIOR, data = Fuente_Final)

# M6: Interacción Geográfico-Educacional (ALINEADO A GRÁFICOS PARTE 4)
# Aquí está el efecto que se conecta con tus gráficos de dispersión particionados
modelo_6 <- lm(MEJOR_M2 ~ Distancia_RM * RAMA_EDUCACIONAL + BEA + REGIMEN + 
                 MODULO_MEJOR_CIENCIA + RINDIO_PROCESO_ANTERIOR, data = Fuente_Final)

# Tabla Comparativa (Evolución de los modelos)
cat("\n=== TABLA 1: EVOLUCIÓN DE LA BRECHA TERRITORIAL ===\n")
stargazer(modelo_1, modelo_2, modelo_3, modelo_4, modelo_5, modelo_6,
          type = "text",
          title = "Modelos de Regresión: Factores asociados al rendimiento M2",
          column.labels = c("Bruto", "Escolar", "Socio-Esc", "Completo", "Interacción", "Interaccion DisRM"),
          omit.stat = c("f", "ser"), # Limpia la tabla visualmente
          digits = 3)

# ==============================================================================
# FASE 2: PRUEBAS DE DIAGNÓSTICO (SUPUESTOS)
# ==============================================================================

# 1. Prueba de Multicolinealidad (VIF)
# Se hace sobre el M4 (sin interacción) porque las interacciones inflan el VIF matemáticamente
cat("\n=== PRUEBA DE MULTICOLINEALIDAD (VIF) SOBRE MODELO COMPLETO ===\n")
vif_resultado <- vif(modelo_4)
print(vif_resultado)

# 2. Prueba de Homocedasticidad (Test de Breusch-Pagan) sobre el modelo final (M5)
cat("\n=== PRUEBA DE HOMOCEDASTICIDAD (Breusch-Pagan) SOBRE MODELO FINAL ===\n")
bp_resultado <- bptest(modelo_5)
print(bp_resultado)

# ==============================================================================
# FASE 3: CORRECCIÓN Y REPORTE ROBUSTO
# ==============================================================================

# Como el Test BP seguramente arrojará p < 0.05, aplicamos Errores Estándar Robustos (HC1)
errores_robustos <- sqrt(diag(vcovHC(modelo_5, type = "HC1")))

# Generamos la tabla final del Modelo 5 usando los errores corregidos
cat("\n=== TABLA 2: MODELO FINAL CON ERRORES ESTÁNDAR ROBUSTOS (CORREGIDO) ===\n")
stargazer(modelo_6, 
          type = "text", 
          se = list(errores_robustos), # Aquí inyectamos la corrección
          title = "Modelo Final (M6) con Corrección de Heterocedasticidad (Robust SE)",
          omit.stat = c("f", "ser"),
          digits = 3)