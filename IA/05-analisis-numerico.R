library(tidyselect)
library(janitor)

# Estructura del conjunto de datos
str(datos_limpios)

# Algunas medidas resumen 
summary(datos_limpios)
summary(datos_limpios[,c(2,3,4)])

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
# sort(table(NU_region), decreasing = TRUE)[1] # Moda

# Dispersión
range(GIRAI) # Valores mín y max
max(GIRAI) - min(GIRAI) # Rango
sd(GIRAI) # Desvío estándar
var(GIRAI) # Variancia
IQR(GIRAI) # Rango intercuartílico
round(sd(GIRAI)/mean(GIRAI)*100,1) # Coeficiente de variación

# Otras medidas
#var(altura,diametro) # Covariancia
#cor(altura,diametro) # Correlación lineal

# Medidas por grupos (boxplot naranja)
datos_limpios %>% group_by(NU_region) %>%
  summarise(Promedio = median(GIRAI),
            Desv.Est. = IQR(GIRAI),
            Mínimo = min(GIRAI),
            Máximo = max(GIRAI))
           

niveles_ordenados <- c("Muy bajo", "Bajo", "Medio", "Alto", "Muy alto")

# 2. Resumen por región para las tres variables de fuentes secundarias
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
            
#tero

# Distribuciones condicionales
#tabyl(datos_limpios, tiempo, follaje) %>%
  #adorn_totals(where = c("row", "col")) %>%
  #adorn_percentages(denominator = "row") %>%
  #adorn_pct_formatting(digits = 1) %>%
  #adorn_title(placement = "top", "Origen", "Tipo de follaje")