# Instalo los paquetes necesarios (si aún no los tengo instalados)
# install.packages("tidyverse")
# install.packages("ggplot2")
# install.packages("janitor")
# install.packages("gridExtra")

# Cargo los paquetes que voy a usar
library(tidyverse)
library(ggplot2)
library(janitor)
library(gridExtra)

# Fijo el dataset
attach(datos_limpios)




# Grafico de sectores circulares con las dimensiones mejor puntuadas-----------

datos_limpios %>% 
  tabyl(Dimension_mejor_puntuada) %>% # Primero armo la tabla
  adorn_pct_formatting(digits = 1) %>%
  ggplot() + 
  aes(x = "", fill = Dimension_mejor_puntuada, y = percent) + # Porcentajes
  geom_bar(stat="identity", color="white") + 
  coord_polar("y", start=0) + # Se transforman las frecuencias a coordenadas polares
  labs(fill = "") +
  ggtitle("Dimensión mejor puntuada") +
  theme_void() + # Quito elementos gráficos no deseados
  geom_text(aes(label = percent), # Agrego etiquetas
            position = position_stack(vjust = 0.5),
            cex = 3) + 
  scale_fill_brewer(palette="Set1") # Extra: puedo elegir paleta de colores

# Grafico areas con mas de 70 puntos

datos_limpios %>%
  separate_rows(Areas_p70_multiple, sep = ", ") %>%
  filter(Areas_p70_multiple != "Ninguna") %>%
  count(Areas_p70_multiple) %>%
  ggplot() +
  aes(x = reorder(Areas_p70_multiple, n), y = n) +
  geom_bar(stat = "identity", fill = "steelblue", col = "black") +
  coord_flip() +
  labs(
    title = "Áreas con desempeño destacado (>70 puntos)",
    x = "Área Temática",
    y = "Cantidad de Países"
  ) +
  theme_minimal()

#-----------------------------------------------------------------------------

# Definir la ordinalidad de la variable de fuentes secundarias [cite: 161]
datos_limpios$Marcos_fuentes_secundarias <- factor(
  datos_limpios$Marcos_fuentes_secundarias,
  levels = c("Muy bajo", "Bajo", "Medio", "Alto", "Muy alto")
)



##############
# Histograma #
############## 

# histograma de Frecuencias absolutas del GIRAI
ggplot(datos_limpios) +
  aes(x = GIRAI) +
  geom_histogram(fill = "lightgray", col = "black", 
                 
                 breaks = seq(0, 100, 5)) + # Límites de intervalos
  
  scale_x_continuous(breaks = seq(0, 250, 20)) + #Marcas del eje
  
  labs(x = "Puntos GIRAI", y = "Cantidad de países")