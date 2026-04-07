# Librerias
library(dplyr)
library(stringr)

# Limpieza
rm(list = ls())
graphics.off() 

# Lectura de csv's
inscripcion <- read.csv("DB/Inscripcion/ArchivoB_Adm2025.csv", sep = ";")
dInscripcion <- read_xlsx("DB/Inscripcion/Libro_CódigosADM2025_ArchivoB.xlsx")

postulacion <- read.csv("DB/Postulacion/ArchivoD_Adm2025.csv", sep = ";")
dpostulacion <- read_xlsx("DB/Postulacion/Libro_CódigosADM2025_ArchivoD.xlsx")

rendicion <- read.csv("DB/Rendicion/ArchivoC_Adm2025.csv", sep = ";")
drendicion <- read_xlsx("DB/Rendicion/Libro_CódigosADM2025_ArchivoC.xlsx")

# Depuracion Inscripcion
inscripcion <- inscripcion %>%
  filter(RINDIO_PROCESO_ACTUAL != 0) %>%
  select(-ANYO_PROCESO, -FECHA_NACIMIENTO, -RBD, -COD_ENS, -GRUPO_DEPENDENCIA,
         -ANYO_EGRESO, -CODIGO_REGION, -CODIGO_PROVINCIA, -CODIGO_COMUNA,
         -CODIGO_COMUNA_D, -SITUACION_EGRESO, -PACE, -PAIS_NACIMIENTO,
         -INGRESO_PERCAPITA_GRUPO_FA, -RINDIO_PROCESO_ACTUAL, -SEXO) %>%
  mutate(
    REGIMEN = case_when(
      REGIMEN == 1 ~ "Masculino",
      REGIMEN == 2 ~ "Femenino",
      REGIMEN == 3 ~ "Coeducacional"
    ),
    RAMA_EDUCACIONAL = case_when(
      str_starts(RAMA_EDUCACIONAL, "T") ~ "Técnico profesional",
      str_starts(RAMA_EDUCACIONAL, "H") ~ "Humanista cientifico"
    ),
    CODIGO_REGION_D = case_when(
      as.numeric(CODIGO_REGION_D) == 15 ~ "Arica y Parinacota", 
      as.numeric(CODIGO_REGION_D) == 1  ~ "Tarapacá", 
      as.numeric(CODIGO_REGION_D) == 2  ~ "Antofagasta", 
      as.numeric(CODIGO_REGION_D) == 3  ~ "Atacama", 
      as.numeric(CODIGO_REGION_D) == 4  ~ "Coquimbo", 
      as.numeric(CODIGO_REGION_D) == 5  ~ "Valparaíso", 
      as.numeric(CODIGO_REGION_D) == 6  ~ "Región del Libertador Gral. Bernardo O'Higgins",
      as.numeric(CODIGO_REGION_D) == 7  ~ "Región del Maule", 
      as.numeric(CODIGO_REGION_D) == 8  ~ "Región del Biobío", 
      as.numeric(CODIGO_REGION_D) == 9  ~ "Región de la Araucanía", 
      as.numeric(CODIGO_REGION_D) == 14 ~ "Región de Los Ríos", 
      as.numeric(CODIGO_REGION_D) == 10 ~ "Región de Los Lagos", 
      as.numeric(CODIGO_REGION_D) == 11 ~ "Región Aisén del Gral. Carlos Ibáñez del Campo", 
      as.numeric(CODIGO_REGION_D) == 12 ~ "Región de Magallanes y de la Antártica Chilena", 
      as.numeric(CODIGO_REGION_D) == 13 ~ "Región Metropolitana de Santiago"
    ),
    BEA = if_else(BEA == "BEA", 1, 0, missing = 0)
  )
  
# Depuracion Postulacion
postulacion <- postulacion %>%
  select(-ORDEN_PREF, -ESTADO_PREF, -TIPO_PREF, -PTJE_PREF)

# Depuracion Rendicion
rendicion <- rendicion %>%
  filter(MATE2_REG_ACTUAL > 0 | MATE2_INV_ACTUAL > 0 | MATE2_REG_ANTERIOR > 0 | MATE2_INV_ANTERIOR >0) %>%
  select(-RBD, -COD_ENS, -GRUPO_DEPENDENCIA, -RAMA_EDUCACIONAL, -SITUACION_EGRESO,
         -CODIGO_REGION, -CODIGO_COMUNA, -PROMEDIO_NOTAS, -PORC_SUP_NOTAS, -PTJE_NEM,
         -PTJE_RANKING, -CLEC_REG_ACTUAL, -MATE1_REG_ACTUAL, -HCSOC_REG_ACTUAL,
         -CLEC_INV_ACTUAL, -MATE1_INV_ACTUAL, -HCSOC_INV_ACTUAL, -CLEC_REG_ANTERIOR,
         -MATE1_REG_ANTERIOR, -HCSOC_REG_ANTERIOR, -CLEC_INV_ANTERIOR, -MATE1_INV_ANTERIOR,
         -HCSOC_INV_ANTERIOR) %>%
  mutate(
    MEJOR_M2 = pmax(MATE2_REG_ACTUAL, MATE2_INV_ACTUAL, MATE2_REG_ANTERIOR, MATE2_INV_ANTERIOR, na.rm = TRUE),
    MEJOR_CIENCIA = pmax(CIEN_REG_ACTUAL, CIEN_INV_ACTUAL, CIEN_REG_ANTERIOR, CIEN_INV_ANTERIOR, na.rm = TRUE),
    MODULO_MEJOR_CIENCIA = case_when(
      MEJOR_CIENCIA == CIEN_REG_ACTUAL ~ MODULO_REG_ACTUAL,
      MEJOR_CIENCIA == CIEN_INV_ACTUAL ~ MODULO_INV_ACTUAL,
      MEJOR_CIENCIA == CIEN_REG_ANTERIOR ~ MODULO_REG_ANTERIOR,
      MEJOR_CIENCIA == CIEN_INV_ANTERIOR ~ MODULO_INV_ANTERIOR,
      TRUE ~ NA_character_
    )
  ) %>%
  select(ID_aux, MEJOR_M2, MEJOR_CIENCIA, MODULO_MEJOR_CIENCIA)


Fuente <- postulacion %>%
  left_join(inscripcion, by = "ID_aux") %>%
  left_join(rendicion, by = "ID_aux") %>%
  filter(MEJOR_M2 > 0) %>%
  select(-MEJOR_CIENCIA)
