# Instalo los paquetes necesarios (si aún no los tengo instalados)
# install.packages("tidyverse")
# install.packages("janitor")

# Cargo los paquetes que voy a usar
library(tidyverse)


# Fijo el dataset
attach(datos_limpios)




datos_limpios %>% group_by(NU_region) %>%
  summarize(GIRAI_media = mean(GIRAI),
            GIRAI_maximo = max(GIRAI),
            GIRAI_minimo = min(GIRAI),
            GIRAI_ds = sd(GIRAI))



datos_limpios %>% group_by(NU_subregion) %>%
  summarize(GIRAI_media = mean(GIRAI),
            GIRAI_maximo = max(GIRAI),
            GIRAI_minimo = min(GIRAI),
            GIRAI_ds = sd(GIRAI))

datos_limpios %>% group_by(NU_region) %>%
  summarize(mng_media = mean(Marcos_normativos_gubernamentales),
            ag_media = mean(Acciones_gubernamentales),
            ane_media = mean(Actores_no_estatales))


datos_limpios %>% group_by(NU_subregion) %>%
  summarize(mng_media = mean(Marcos_normativos_gubernamentales),
            ag_media = mean(Acciones_gubernamentales),
            ane_media = mean(Actores_no_estatales))      


