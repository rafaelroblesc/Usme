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
ventana <- sf::read_sf(enlace)

# Cambiar el sistema de coordenadas de WGS 84 a 102232

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
# Hacer el plano del area excavada y de los tumbas
# 
plot(ventana$geometry, reset = TRUE)
points(tumbas_usme$X[tumbas_usme$Superposicion == "Si"], tumbas_usme$Y[tumbas_usme$Superposicion == "Si"], pch = 20, col = "gold")
#
#
# Comparación de variables según si las tumbas están superpuestas o no
#
# 1) Sexo
#
# Tabla de frecuencias
#
resumen <- tumbas_usme %>% select(Superposicion, Sexo) %>% na.omit() %>% table() 
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
chichi <- chisq.test(resumen) # X-squared = 0.61227, df = 2, p-value = 0.7363
chichi
round(chichi$expected, 2)
fishert <- fisher.test(resumen)
fishert$method
#
assocstats(resumen) # V de Cramer = 0.092 es cercana a 0
#
# 2) Grupos de edad
#
# Tabla de frecuencias
#
resumen <- tumbas_usme %>% select(Superposicion, Edad_grup) %>% na.omit() %>% table()
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
chisq.test(resumen) # X-squared = 0.82822, df = 3, p-value = 0.8427
#
# Valroes esperados
#
round(chisq.test(resumen)$expected, 2)
#
assocstats(resumen) # V de Cramer = 0.117 
#
# 3) Orientación
#
# Tabla de frecuencias
#
resumen <- tumbas_usme %>% select(Superposicion, Orienta) %>% na.omit() %>% table()
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
chisq.test(resumen) # X-squared = 3.7237, df = 5, p-value = 0.5898
#
# Valores esperados
#
round(chisq.test(resumen)$expected, 2)
#
assocstats(resumen) # V de Cramer = 0.322 
#
#
# 4) Volumen
#
volumen_tumbas <- tumbas_usme %>% select(Superposicion, Volumen) 
volumen_tumbas <- na.omit(volumen_tumbas)
#
# Tallo y hojas
#
stem.leaf.backback(volumen_tumbas$Volumen[volumen_tumbas$Superposicion == "Si"],
                   volumen_tumbas$Volumen[volumen_tumbas$Superposicion == "No"],
                   unit = 0.1)
# Prueba t
#
t.test(Volumen ~ Superposicion, data = tumbas_usme)
#
#
# 5) Forma
# Tabla de frecuencias
#
resumen <- tumbas_usme %>% select(Superposicion, Forma) %>% na.omit() %>% table()
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
chichi <- chisq.test(resumen) # X-squared = 2.4005, df = 1, p-value = 0.1213
chichi
round(chichi$expected, 2)
#
assocstats(resumen) # V de Cramer = 0.24 
#
# 6) Construcciones internas
# Tabla de frecuencias
#
resumen <- tumbas_usme %>% select(Superposicion, Construcciones) %>% na.omit() %>% table()
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
chisq.test(resumen) # X-squared = 5.5612, df = 3, p-value = 0.135
#
assocstats(resumen) # V de Cramer = 0.302 
#
