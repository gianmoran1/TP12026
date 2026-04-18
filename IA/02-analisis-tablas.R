# Instalo los paquetes necesarios (si aún no los tengo instalados)
# install.packages("tidyverse")
# install.packages("janitor")

# Cargo los paquetes que voy a usar
library(tidyverse)


# Fijo el dataset
attach(datos)




datos_limpios %>% group_by(NU_region) %>%
  summarize(GIRAI_media = mean(GIRAI),
  region_ds = sd(GIRAI))



datos_limpios %>% group_by(NU_subregion) %>%
  summarize(GIRAI_media = mean(GIRAI),
            region_ds = sd(GIRAI))