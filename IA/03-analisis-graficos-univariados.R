# Instalo los paquetes necesarios (si aún no los tengo instalados)
# install.packages("tidyverse")
# install.packages("ggplot2")

# Cargo los paquetes que voy a usar
library(tidyverse)
library(ggplot2)

# Fijo el dataset
attach(datos_limpios)

# Gráfico de barras top 10 GIRAI

top_10 <- datos_limpios[order(datos$Ranking), ][1:10, ]

top_10 %>%
  ggplot() + 
  aes(x = reorder(Pais, -GIRAI), y = GIRAI) + 
  geom_bar(width = 0.75,   # Ancho de barras 
           fill = '#7ed021',  # Color 
           col = 'black',     # Borde 
           stat = "identity") + # Agregado necesario porque ya tenemos el valor calculado
  labs(x = "País", y = "Valor GIRAI") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Grafico de sectores circulares con las dimensiones mejor puntuadas

datos_limpios %>% 
  tabyl(Dimesion_mejor_puntuada) %>% # Primero armo la tabla
  adorn_pct_formatting(digits = 1) %>%
  ggplot() + 
  aes(x = "", fill = Dimesion_mejor_puntuada, y = percent) + # Porcentajes
  geom_bar(stat="identity", color="white") + 
  coord_polar("y", start=0) + # Se transforman las frecuencias a coordenadas polares
  labs(fill = "") +
  ggtitle("Dimensión mejor puntuada") +
  theme_void() + # Quito elementos gráficos no deseados
  geom_text(aes(label = percent), # Agrego etiquetas
            position = position_stack(vjust = 0.5),
            cex = 3) + 
  scale_fill_brewer(palette="Set1") # Extra: puedo elegir paleta de colores






