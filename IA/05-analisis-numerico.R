# Instalo los paquetes necesarios (si aún no los tengo instalados)
# install.packages("tidyverse")
# install.packages("janitor")

# Cargo los paquetes que voy a usar
library(tidyselect)
library(janitor)

# Estructura del conjunto de datos
str(datos_limpios)

# Algunas medidas resumen 
summary(datos_limpios)
summary(datos_limpios[,c(1,2,3,4)])

# Otras funciones para obtener medidas

# Posición: tendencia central
mean(datos_limpios$GIRAI) # Media aritmética
median(datos_limpios$GIRAI) # Mediana

attach(datos_limpios) # Puedo fijar los datos por comodidad

# Posición: otras
min(GIRAI) 
max(GIRAI)
quantile(GIRAI) # 5 medidas resumen
quantile(GIRAI, 0.9) # Otros percentiles
sort(table(NU_region), decreasing = TRUE)[1] # Moda

# Dispersión
range(GIRAI) # Valores mín y max
max(GIRAI) - min(GIRAI) # Rango
sd(GIRAI) # Desvío estándar
var(GIRAI) # Variancia
IQR(GIRAI) # Rango intercuartílico
round(sd(GIRAI)/mean(GIRAI)*100,1) # Coeficiente de variación

# Otras medidas
var(Derechos_humanos,Gobernanza_IA)# Covariancia
var(Derechos_humanos, Capacidades_IA)
var(Capacidades_IA, Gobernanza_IA)
cor(Derechos_humanos,Gobernanza_IA)# Correlacion lineal
cor(Derechos_humanos, Capacidades_IA)
cor(Capacidades_IA, Gobernanza_IA)

# Medidas por grupos (boxplot naranja)
datos_limpios %>% group_by(NU_region) %>%
  summarise(Promedio = median(GIRAI),
            Desv.Est. = IQR(GIRAI),
            Mínimo = min(GIRAI),
            Máximo = max(GIRAI))
