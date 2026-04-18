# Instalo los paquetes necesarios (si aún no los tengo instalados)
# install.packages("tidyverse")
# install.packages("ggplot2")

# Cargo los paquetes que voy a usar
library(tidyverse)
library(ggplot2)

# Fijo el dataset

# Fijo el dataset
attach(datos)

#####################
# Gráfico de barras top 10 girai #
#####################

top_10 <- datos[order(datos$Ranking), ][1:10, ]


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


##HACER ESTO POR REGION

# datos_limpios %>% group_by(especie) %>%
#summarize(altura_media = mean(altura),
          #altura_ds = sd(altura))

