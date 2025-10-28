################################################################################
#
#          Titulo de la tesis: Memoria social y rituales funerarios 
#                      en la comunidad local de Usme
#
#                                Rafael Robles
#                             Estudiante doctoral
#
#
#                        Universidad Nacional de Colombia
#                          Facultad de Ciencias Humanas
#                            Doctorado en Antropologia
#
#
################################################################################
#
#    Script para la comparación de las tumbas superpuestas y no superpuestas
#
#
#
#   Paquetes necesarios: sf, spatstat, vcd, tidyverse
#
library(tidyverse) # dplyr ggplot2
library(vcd) # assocstats()
library(sf) # read_sf() st_transform()
library(aplpack) #stem.leaf.backback()
#
#
# Cargar datos de los centroides de los tumbas desde github
#
enlace <- "https://raw.githubusercontent.com/rafaelroblesc/Usme/refs/heads/main/Tumbas_coordenadas.txt"
tumbas_usme <- read.table(enlace, header = TRUE, dec = ".", row.names = 1)
#
str(tumbas_usme) # Revisar la tabla de datos
#
# Agregar el objeto espacial del area total excavada
#
enlace <- "https://raw.githubusercontent.com/rafaelroblesc/Usme/main/area_excavada.kml"
ventana <- read_sf(enlace)
#
# Cambiar el sistema de coordenadas de WGS 84 a 102232
#
texto_crs <- "+proj=tmerc 
                          +lat_0=4.68333333333333 
                          +lon_0=-74.15 
                          +k=1.00039978753252 
                          +x_0=92334.879 
                          +y_0=109320.965 
                          +ellps=intl 
                          +towgs84=221.899,274.136,-397.554,-2.80844591036278,0.44850858891268,2.81017234679107,-2.199943 
                          +units=m 
                          +no_defs"

ventana <- st_transform(ventana, crs = texto_crs)
#
str(ventana) # Revisar la estructura de la ventana
#
# Hacer el plano del area excavada y de los tumbas cercanas
# 
plot(ventana$geometry, reset = TRUE)
points(tumbas_usme$X[tumbas_usme$a20cm == "Si"], tumbas_usme$Y[tumbas_usme$a20cm == "Si"], pch = 20, col = "gold")
#
#
####### Análisis de tumbas a menos de 20 cm
#
# Tabla del número de tumbas por grupo
#
super <- table(tumbas_usme$a20cm) 
#
super # Imprimir
#
# De las 61 tumbas, 32 están fuera de los grupos y 29 al interior
#
41/61 # 67.2% están superpuestas
#
# Análisis de las tumbas
#
# 1) Sexo
#
# Tabla de frecuencias
#
resumen <- tumbas_usme %>% select(a20cm, Sexo) %>% na.omit() %>% table() 
resumen # imprimir
#
# Tabla de porcentajes
#
resumen_p <- resumen
resumen_p[1,] <- resumen_p[1,] %>% proportions()
resumen_p[2,] <- resumen_p[2,] %>% proportions()
round(resumen_p*100, 2)  # imprimir
#
# Chi cuadrado
#
chichi <- chisq.test(resumen) # X-squared = 0.26576, df = 2, p-value = 0.8756
chichi
round(chichi$expected, 2)
#
assocstats(resumen) # V de Cramer = 0.066 es cercana a 0
#
# Las diferencias entre los dos grupos con respecto al sexo no es nada significativa
#
# 2) Grupos de edad
#
# Tabla de frecuencias
#
resumen <- tumbas_usme %>% select(a20cm, Edad_grup) %>% na.omit() %>% table()
resumen # imprimir
#
# Tabla de porcentajes
#
resumen_p <- resumen
resumen_p[1,] <- resumen_p[1,] %>% proportions()
resumen_p[2,] <- resumen_p[2,] %>% proportions()
round(resumen_p*100, 2)  # imprimir
#
# Chi cuadrado
#
chichi <- chisq.test(resumen) # X-squared = 2.4108, df = 3, p-value = 0.4916
chichi
round(chichi$expected, 2)
#
assocstats(resumen) # V de Cramer = 0.199 
#
# Las diferencias entre los dos grupos con respecto al sexo es poco significativa
# Las diferencias no son muy fuertes
#
# 3) Orientación
#
# Tabla de frecuencias
#
resumen <- tumbas_usme %>% select(a20cm, Orienta) %>% na.omit() %>% table()
resumen # imprimir
#
# Tabla de porcentajes
#
resumen_p <- resumen
resumen_p[1,] <- resumen_p[1,] %>% proportions()
resumen_p[2,] <- resumen_p[2,] %>% proportions()
round(resumen_p*100, 2)  # imprimir
#
# Chi cuadrado
#
chichi <- chisq.test(resumen) # X-squared = 0.91952, df = 5, p-value = 0.9688
chichi
round(chichi$expected, 2)
#
assocstats(resumen) # V de Cramer = 0.066 es cercana a 0
# 
#
# 4) Construcciones internas
# Tabla de frecuencias
#
resumen <- tumbas_usme %>% select(a20cm, Construcciones) %>% na.omit() %>% table()
resumen # imprimir
#
# Tabla de porcentajes
#
resumen_p <- resumen
resumen_p[1,] <- resumen_p[1,] %>% proportions()
resumen_p[2,] <- resumen_p[2,] %>% proportions()
round(resumen_p*100, 2)  # imprimir
#
# Chi cuadrado
#
chichi <- chisq.test(resumen) # X-squared = 4.523, df = 3, p-value = 0.2102
chichi
round(chichi$expected, 2)
#
assocstats(resumen) # V de Cramer = 0.066 es cercana a 0
#
# 5) Forma
# Tabla de frecuencias
#
resumen <- tumbas_usme %>% select(a20cm, Forma) %>% na.omit() %>% table()
resumen # imprimir
#
# Tabla de porcentajes
#
resumen_p <- resumen
resumen_p[1,] <- resumen_p[1,] %>% proportions()
resumen_p[2,] <- resumen_p[2,] %>% proportions()
round(resumen_p*100, 2)  # imprimir
#
# Chi cuadrado
#
chichi <- chisq.test(resumen) # X-squared = 4.6501, df = 1, p-value = 0.03105
chichi
round(chichi$expected, 2)
#
assocstats(resumen) # V de Cramer = 0.066 es cercana a 0
#
# revisar variables numéricas
#
#
# 6) MArcas
# Tabla de frecuencias
#
marcas <- tumbas_usme %>% select(a20cm, Marcas) %>% na.omit() %>% table()
marcas 
# imprimir
#
# Tabla de porcentajes
#
resumen_p <- marcas 
resumen_p[1,] <- resumen_p[1,] %>% proportions()
resumen_p[2,] <- resumen_p[2,] %>% proportions()
round(resumen_p*100, 2)  # imprimir
#
# Chi cuadrado
#
chichi <- chisq.test(marcas) # X-squared = 4.6501, df = 1, p-value = 0.03105
chichi
round(chichi$expected, 2)
#
assocstats(resumen) # V de Cramer = 0.066 es cercana a 0
# 
# 
# 7) Volumen
#
volumen_tumbas <- tumbas_usme %>% select(a20cm, Volumen) 
volumen_tumbas <- na.omit(volumen_tumbas)
#
# Tallo y hojas
#
tem.leaf.backback(volumen_tumbas$Volumen[volumen_tumbas$a20cm == "Si"],
                  volumen_tumbas$Volumen[volumen_tumbas$a20cm == "No"],
                  unit = 0.1)
#
# Prueba t
#
t.test(Volumen ~ a20cm, data = volumen_tumbas)
#
#
