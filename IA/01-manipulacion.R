# Instalo los paquetes necesarios (si aún no los tengo instalados)
# install.packages("tidyverse")

# Cargo los paquetes que voy a usar
library(tidyverse)

# Fijo el dataset
attach(datos)

datos_limpios <- datos %>%
  select(   # Eliminar las columnas que no quiero conservar
    -Ranking, -ISO3, -Country, -GIRAI_region,
    -areas_parlam, -areas_concient,
    -academia, -tipo_academia_en, -tipo_academia_es,
    -privado, -tipo_privado_en, -tipo_privado_es
  )

colnames(datos_limpios) <- c(
  "Pais", "NU_region","NU_subregion", "GIRAI", "Marcos_normativos_gob",
  "Acciones_gob", "Actores_no_estatales", "Derechos_humanos", "Gobernanza_IA",
  "Capacidades_IA", "Marcos_fuentes_secundarias",
  "Acciones_fuentes_secundarias", "Actores_no_estatales_secundarias",
  "Dimension_mejor_puntuada", "Sesgo_y_discriminacion",
  "Derechos_infancia", "Diversidad_cultural_y_linguistica",
  "Proteccion_datos_y_privacidad", "Igualdad_de_genero",
  "Supervision_humana", "Proteccion_laboral_y_trabajo",
  "Seguridad_precision_y_fiabilidad", "Transparencia_y_explicabilidad",
  "Cant_areas_reglas_IA", "Cant_areas_acciones_gob_IA",
  "Cant_areas_trabajo_nsa_IA"
)

# Creación de variable de respuesta múltiple y limpieza de p70
datos_limpios <- datos_limpios %>%
  rowwise() %>%
  mutate(
    Areas_p70_multiple = {
      v <- c(
        if (Sesgo_y_discriminacion == 1) "Sesgo",
        if (Derechos_infancia == 1) "Infancia",
        if (Diversidad_cultural_y_linguistica == 1) "Diversidad",
        if (Proteccion_datos_y_privacidad == 1) "Datos",
        if (Igualdad_de_genero == 1) "Género",
        if (Supervision_humana == 1) "Supervisión",
        if (Proteccion_laboral_y_trabajo == 1) "Laboral",
        if (Seguridad_precision_y_fiabilidad == 1) "Seguridad",
        if (Transparencia_y_explicabilidad == 1) "Transparencia"
      )
      if (length(v) == 0) "Ninguna" else paste(v, collapse = ", ")
    },
    .after = Dimension_mejor_puntuada # Acomoda la columna acá
  ) %>%
  ungroup() %>%
  # Elimino las columnas de indicadores p70
  select(
    -Sesgo_y_discriminacion, -Derechos_infancia,
    -Diversidad_cultural_y_linguistica, -Proteccion_datos_y_privacidad,
    -Igualdad_de_genero, -Supervision_humana,
    -Proteccion_laboral_y_trabajo, -Seguridad_precision_y_fiabilidad,
    -Transparencia_y_explicabilidad
  )