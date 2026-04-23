# Instalo los paquetes necesarios (si aún no los tengo instalados)
# install.packages("tidyverse")
# install.packages("janitor")

# Cargo los paquetes que voy a usar
library(tidyverse)
library(janitor)


# Fijo el dataset
attach(datos_limpios)

# Frecuencias
datos_limpios %>% group_by(Pais) %>%
  summarize(cant = n())

## medidas resumen por regiones de la ONU
datos_limpios %>% group_by(NU_region) %>%
  summarize(GIRAI_media = mean(GIRAI),
            GIRAI_maximo = max(GIRAI),
            GIRAI_minimo = min(GIRAI),
            GIRAI_ds = sd(GIRAI),
            mng_media = mean(Marcos_normativos_gob),
            ag_media = mean(Acciones_gob),
            ane_media = mean(Actores_no_estatales)
            )



## medidas resumen por sub_regiones de la ONU
datos_limpios %>% group_by(NU_subregion) %>%
  summarize(GIRAI_media = mean(GIRAI),
            GIRAI_maximo = max(GIRAI),
            GIRAI_minimo = min(GIRAI),
            GIRAI_ds = sd(GIRAI),
            mng_media = mean(Marcos_normativos_gob),
            ag_media = mean(Acciones_gob),
            ane_media = mean(Actores_no_estatales))


# Variable de respuesta múltiple
tabla_resumen_multiple <- datos_limpios %>%
  separate_rows(Areas_p70_multiple, sep = ", ") %>%
  filter(Areas_p70_multiple != "Ninguna") %>%
  
  group_by(Areas_p70_multiple) %>%
  summarize(
    Cantidad_Paises = n(),
    Promedio_GIRAI = mean(GIRAI, na.rm = TRUE) 
  ) %>%
  
  arrange(desc(Cantidad_Paises))
  print(tabla_resumen_multiple)

  #  Resumen por región para las tres variables de fuentes secundarias
  
  niveles_ordenados <- c("Muy bajo", "Bajo", "Medio", "Alto", "Muy alto")
  tabla_resumen <- datos_limpios %>%
    group_by(NU_region) %>%
    summarise(
      # Mediana de Marcos
      Mediana_Marcos = niveles_ordenados[round(median(match(Marcos_fuentes_secundarias, niveles_ordenados), na.rm = TRUE))],
      
      # Mediana de Acciones
      Mediana_Acciones = niveles_ordenados[round(median(match(Acciones_fuentes_secundarias, niveles_ordenados), na.rm = TRUE))],
      
      # Mediana de Actores No Estatales
      Mediana_Actores = niveles_ordenados[round(median(match(Actores_no_estatales_secundarias, niveles_ordenados), na.rm = TRUE))]
    )
  
  print(tabla_resumen)


  #########################
  # Tablas usando janitor #
  #########################
  
  # Tabla de distribución de frecuencias
  tabla <- tabyl(NU_region)
  
  # Adorns
  tabla %>% 
    rename(   # Renombro columnas
      "Region" = NU_region,
      "Cant de países" = n,
      " % países" = percent
    ) %>% 
    adorn_totals() %>%  # Agrego fila de totales
    adorn_pct_formatting(digits = 1) # Cant. de decimales en %

  # Tabla de contingencia
  datos_limpios %>%
  tabyl(NU_region, Dimension_mejor_puntuada) %>%
    adorn_totals(where = c("row", "col")) %>%
    adorn_percentages(denominator = "row") %>% # Distribuciones condicionales
    adorn_pct_formatting(digits = 1) %>%
    adorn_title(placement = "combined", row_name = "Región", col_name = "Dimensión mejor puntuada")
      
  
  

    
