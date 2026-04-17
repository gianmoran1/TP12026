# Hay que instalar el paquete de google sheets con:
# install.packages("googlesheets4")

# Se realiza la ingesta de datos desde la hoja de datos.

library(googlesheets4)
url = "https://docs.google.com/spreadsheets/d/1Kwl4KByOv8q2kXMsgaO3d5QI3vUQ40RCZJgJHhg5bmE/edit?pli=1&gid=580479207#gid=580479207"
gs4_deauth()
datos <- read_sheet(url, sheet = 2, skip = 1)
str(datos)
