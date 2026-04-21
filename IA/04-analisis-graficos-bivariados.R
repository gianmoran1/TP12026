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

# Gráfico de barras top 10 GIRAI----------POSIBLE VUELE A LA MIERDA COMO EL TERO-------------------------------------

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


#grafico de barras apiladas entre regiones y dimension mejor puntuada
datos_limpios$Dimension_mejor_puntuada <- factor(datos_limpios$Dimension_mejor_puntuada, 
                                                 levels = c("gob", "ddhh", "cap"))

datos_limpios %>% 
  ggplot() + 
  aes(x = NU_region, fill = Dimension_mejor_puntuada) +
  labs(x = "Regiones", 
       y = "", 
       fill = "Dimension mejor puntuada") +
        scale_fill_discrete(labels = c("Capacidad", "Derechos Humanos", "Gobernanza"))+
  geom_bar(position = "fill") # position = fill, dodge, stack 


# Boxplot GIRAI en regiones----------------------------------------------------

# Creamos los gráficos de a uno y los guardamos en objetos (g1, ..., g5)
g2 <- datos_limpios %>% filter(NU_region == "América") %>%
  ggplot() +
  aes(x = NU_region, y = GIRAI) +
  geom_boxplot(show.legend = F, fill = "orange") +
  scale_y_continuous(limits = c(0,90)) + # Fijo límites para el eje continuo
  theme_minimal() +
  theme(axis.text.y = element_text(size = 0)) +
  labs(x = "", y = "") 

g3 <- datos_limpios %>% filter(NU_region == "Asia") %>%
  ggplot() +
  aes(x = NU_region, y = GIRAI) +
  geom_boxplot(show.legend = F, fill = "orange") +
  scale_y_continuous(limits = c(0,90)) + # Fijo límites para el eje continuo
  theme_minimal() +
  theme(axis.text.y = element_text(size = 0)) +
  labs(x = "", y = "") 


g4 <- datos_limpios %>% filter(NU_region == "Europa") %>%
  ggplot() +
  aes(x = NU_region, y = GIRAI) +
  geom_boxplot(show.legend = F, fill = "orange") +
  scale_y_continuous(limits = c(0,90)) + # Fijo límites para el eje continuo
  theme_minimal() +
  theme(axis.text.y = element_text(size = 0)) +
  labs(x = "", y = "") 

g5 <- datos_limpios %>% filter(NU_region == "Oceanía") %>%
  ggplot() +
  aes(x = NU_region, y = GIRAI) +
  geom_boxplot(show.legend = F, fill = "orange") +
  scale_y_continuous(limits = c(0,90)) + # Fijo límites para el eje continuo
  theme_minimal() +
  theme(axis.text.y = element_text(size = 0)) +
  labs(x = "", y = "") 

g1 <- datos_limpios %>% filter(NU_region == "África") %>%
  ggplot() +
  aes(x = NU_region, y = GIRAI) +
  geom_boxplot(show.legend = F, fill = "orange") +
  scale_y_continuous(limits = c(0,90)) + # Fijo límites para el eje continuo
  theme_minimal() +
  labs(x = "", y = "GIRAI") # Le dejo el nombre del eje solo al primero

# Con la función grid arrange armo la grilla para "imprimir"
grid.arrange(g1, g2, g3, g4, g5, # Qué objetos vamos a mostrar
             ncol=5, nrow =1) # Cant de columnas y filas de la grilla




# boxplot entre la cantidad de areas con marcos normativos y el nivel de desarrollo.
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


##########################
# Diagrama de dispersión #
##########################

ggplot(datos_limpios) +
  aes(x = Marcos_normativos_gob , y = GIRAI) +
  geom_point() +
  labs(x = "Marcos normativos gubernamentales", y = "Puntos GIRAI")+
  ggtitle("Relacion entre los marcos normativos gubernamentales y los puntos GIRAI") +
  theme_classic()