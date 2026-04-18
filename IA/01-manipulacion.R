# Instalo los paquetes necesarios (si aún no los tengo instalados)
# install.packages("tidyverse")

# Cargo los paquetes que voy a usar
library(tidyverse)

# Fijo el dataset
attach(datos)

datos_limpios <- datos %>%
  select(   # Eliminar las columnas que no quiero conservar
    -ISO3, -Country, -tipo_academia_en, -tipo_privado_en
  )

# Renombrar columnas
colnames(datos_limpios) <- c("Ranking","Pais","GIRAI_region","NU_region",
                             "NU_subregion","GIRAI", 
                             "Marcos_normativos_gubernamentales",
                             "Acciones_gubernamentales","Actores_no_estatales",
                             "Derechos_humanos","Gobernanza_IA",
                             "Capacidades_IA",
                             "Marcos_normativos_fuentes_secundarias",
                             "Acciones_gubernamentales_fuentes_secundarias",
                             "Actores_no_estatales_fuentes_secundarias",
                             "Dimesion_mejor_puntuada",
                             "Sesgo_y_discriminacion", "Derechos_infancia",
                             "Diversidad_culturtal_y_linguistica",
                             "Proteccion_de_datos_y_privacidad",
                             "Igualdad_de_genero", "Supervision_humana",
                             "Proteccion_laboral_y_derecho_al_trabajo",
                             "Seguridad_precision_y_fiabilidad",
                             "Transparencia_y_explicabilidad",
                             "Cantidad_de_areas_reglas_estrategias_IA",
                             "Cantidad_de_areas_acciones_gubernamientales_IA",
                             "Cantidad_de_areas_discusiones_parlamentarias_IA",
                             "Cantidad_de_areas_concientizacion_publica_IA",
                             "Cantidad_de_areas_trabajo_actores_no_estatales_IA",
                             "Hay_academias_trabajando",
                             "Tipo_iniciativa_academia",
                             "Hay_sector_privado_trabajando",
                             "Tipo_iniciativa_sector_privado")

# HASTA ACA LABURE LURATI GAY--------------------------------------------------
#bueno

