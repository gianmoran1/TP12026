# Instalo los paquetes necesarios (si aún no los tengo instalados)
# install.packages("tidyverse")
# install.packages("ggplot2")
# install.packages("janitor")

# Cargo los paquetes que voy a usar
library(tidyverse)
library(ggplot2)
library(janitor)

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

#boxplot girai en regiones:
# Creamos los gráficos de a uno y los guardamos en objetos (g1, ..., g5)
g1 <- datos_limpios %>% filter(NU_region == "América") %>%
  ggplot() +
  aes(x = NU_region, y = GIRAI) +
  geom_boxplot(show.legend = F, fill = "orange") +
  scale_y_continuous(limits = c(0,90)) + # Fijo límites para el eje continuo
  theme_minimal() +
  labs(x = "", y = "GIRAI") # Le dejo el nombre del eje solo al primero
g2 <- datos_limpios %>% filter(NU_region == "Asia") %>%
  ggplot() +
  aes(x = NU_region, y = GIRAI) +
  geom_boxplot(show.legend = F, fill = "orange") +
  scale_y_continuous(limits = c(0,90)) + # Fijo límites para el eje continuo
  theme_minimal() +
  theme(axis.text.y = element_text(size = 0)) +
  labs(x = "", y = "") 


g3 <- datos_limpios %>% filter(NU_region == "Europa") %>%
  ggplot() +
  aes(x = NU_region, y = GIRAI) +
  geom_boxplot(show.legend = F, fill = "orange") +
  scale_y_continuous(limits = c(0,90)) + # Fijo límites para el eje continuo
  theme_minimal() +
  theme(axis.text.y = element_text(size = 0)) +
  labs(x = "", y = "") 

g4 <- datos_limpios %>% filter(NU_region == "Oceanía") %>%
  ggplot() +
  aes(x = NU_region, y = GIRAI) +
  geom_boxplot(show.legend = F, fill = "orange") +
  scale_y_continuous(limits = c(0,90)) + # Fijo límites para el eje continuo
  theme_minimal() +
  theme(axis.text.y = element_text(size = 0)) +
  labs(x = "", y = "") 

# Con la función grid arrange armo la grilla para "imprimir"
grid.arrange(g1, g2, g3, g4, g5, # Qué objetos vamos a mostrar
             ncol=5, nrow =1) # Cant de columnas y filas de la grilla

g5 <- datos_limpios %>% filter(NU_region == "África") %>%
  ggplot() +
  aes(x = NU_region, y = GIRAI) +
  geom_boxplot(show.legend = F, fill = "orange") +
  scale_y_continuous(limits = c(0,90)) + # Fijo límites para el eje continuo
  theme_minimal() +
  theme(axis.text.y = element_text(size = 0)) +
  labs(x = "", y = "") 


# Con la función grid arrange armo la grilla para "imprimir"
grid.arrange(g1, g2, g3, g4, g5, # Qué objetos vamos a mostrar
             ncol=5, nrow =1) # Cant de columnas y filas de la grilla



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

