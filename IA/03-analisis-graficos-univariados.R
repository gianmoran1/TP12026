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

# Gráfico de barras top 10 GIRAI-----------------------------------------------

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

# Boxplot GIRAI en regiones----------------------------------------------------

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

# Grafico variable categorica de respuesta multiple

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

# Gráfico de relación Categórica vs Cuantitativa (Sin puntos)
datos_limpios %>%
  ggplot() +
  aes(x = Marcos_fuentes_secundarias, y = Cant_areas_reglas_IA) +
  # El boxplot muestra la distribución de áreas reguladas [cite: 164]
  geom_boxplot(fill = "lightblue", outlier.color = "red") +
  labs(
    title = "Relación entre desarrollo percibido y regulación efectiva",
    x = "Nivel de desarrollo (Fuentes secundarias)",
    y = "Cantidad de áreas con marcos normativos"
  ) +
  theme_minimal()