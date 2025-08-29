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
#           Script para la comapración de los dos grupos de casas
#
#
#
#   Paquetes necesarios: sf, spatstat, vcd, tidyverse y agricolae
#
library(tidyverse) # dplyr ggplot2 pipelines
library(spatstat) # area.owin()
library(vcd) # assocstats()
library(sf) # read_sf()
library(aplpack) # stem.leaf.backback()
#
#
#
# Parte 1: comparación de las huellas de postes de los dos grupos
#
# Cargar datos de huellas de postes
#
postes_usme <- read.table("https://raw.githubusercontent.com/rafaelroblesc/Usme/refs/heads/main/Postes_coordenadas.txt",
                          header = TRUE, 
                          dec = ".", 
                          row.names = 1)
#
# Agisnar la pertenencia grupal
#
postes_usme$grupo <- c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 
  2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 
  2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 
  2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 
  2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 1)
#
#
str(postes_usme) # Hay área pero no hay diámetro
#
# Crear la variables diametro
#
# Media del diámetro
#
postes_usme$diametro <- 2*sqrt(postes_usme$Area/pi)*100 # Calcular diámetro
#
# Calcular la media del diámetro
#
mean(postes_usme$diametro) # Media del diámetro en cm
#
# Rangos de error al 95% 
#
n <- length(postes_usme$diametro) 
gl <- n-1 
EE.obs <- sd(na.omit(postes_usme$diametro)) / sqrt(length(na.omit(postes_usme$diametro)))
t95.obs <- abs(qt(0.025, gl))
RE.obs <- EE.obs*t95.obs
#
RE.obs # Imprimir el rango
#
# 
# Comparar diámetros de ambos grupos
#
#
stem.leaf.backback(postes_usme$diametro[postes_usme$grupo == 1], 
                   postes_usme$diametro[postes_usme$grupo == 2])
#
# Media grupo 1
#
mean(postes_usme$diametro[postes_usme$grupo == 1]) # Promedio grupo 1: 24.76 cm
#
nx <- length(postes_usme$diametro[postes_usme$grupo == 1]) 
gl <- nx-1 
EE.o <- sd(postes_usme$diametro[postes_usme$grupo == 1]) / sqrt(length(postes_usme$diametro[postes_usme$grupo == 1]))
t95.o <- abs(qt(0.025, gl))
RE.o <- EE.o*t95.o
#
RE.o # 3.66
# 
# Media del grupo 1: 24.76 +- 3.66 cm
#
# Media grupo 2
#
mean(postes_usme$diametro[postes_usme$grupo == 2]) # Promedio grupo 2: 14.14 cm
#
nx <- length(postes_usme$diametro[postes_usme$grupo == 2]) 
gl <- nx-1 
EE.o <- sd(postes_usme$diametro[postes_usme$grupo == 2]) / sqrt(length(postes_usme$diametro[postes_usme$grupo == 1]))
t95.o <- abs(qt(0.025, gl))
RE.o <- EE.o*t95.o
#
RE.o # 2.21
# 
# Media del grupo 1: 14.14 +- 2.21 cm
#
#
# Prueba t
#
t.test(postes_usme$diametro ~ postes_usme$grupo)
#
# t = 5.6796, df = 30.347, p-value = 3.292e-06
# Hay diferencias significativas entre el diámetro de las huellas de los dos grupos
#
#
#
# Parte 2: comparación de las tumbas de los dos grupos
#
# Cargar datos de los centroides de los tumbas desde github
#
enlace <- "https://raw.githubusercontent.com/rafaelroblesc/Usme/refs/heads/main/Tumbas_coordenadas.txt"
tumbas_usme <- read.table(enlace, header = TRUE, dec = ".", row.names = 1)
# View(tumbas_usme)
#
str(tumbas_usme) # Revisar la tabla de datos
#
# Agisnar la pertenencia grupal
#
tumbas_usme$Grupo <- c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 
  2, 2, 2, 2, 2, 1, 1, 2, 2, 2, 1, 1, 1, 2, 2, 2, 1, 1, 1, 2, 2, 2, 1, 1, 1, 2, 
  2, 2, 2, 2, 2, 2, 2, 2, 1)
#
#
# Comparación de las variables según los grupos
#
# Revisar variables disponibles en la tabla
#
head(tumbas_usme, 0)
#
#
# A) Revisar las variables de categorías
#
# 1) Sexo
#
# Tabla de frecuencias
#
resumen <- tumbas_usme %>% select(Grupo, Sexo) %>% na.omit() %>% table() 
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
chisq.test(resumen) # X-squared = 2.219, df = 2, p-value = 0.3297
#
# Valores esperados
chisq.test(resumen)$expected
#
assocstats(resumen) # V de Cramer = 0.191 es cercana a 0
#
# Las diferencias entre los dos grupos con respecto al sexo no es nada significativa
#
# 2) Grupos de edad
#
# Tabla de frecuencias
#
resumen <- tumbas_usme %>% select(Grupo, Edad_grup) %>% na.omit() %>% table()
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
chisq.test(resumen) # X-squared = 1.2792, df = 3, p-value = 0.7341
#
# Valores esperados
#
chisq.test(resumen)$expected
#
assocstats(resumen) # V de Cramer = 0.145
#
# Las diferencias entre los dos grupos con respecto al sexo es poco significativa
# Las diferencias no son muy fuertes
#
# 3) Orientación
#
# Tabla de frecuencias
#
resumen <- tumbas_usme %>% select(Grupo, Orienta) %>% na.omit() %>% table()
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
chisq.test(resumen) # X-squared = 5.94, df = 5, p-value = 0.3121
#
# Valores esperados
#
round(chisq.test(resumen)$expected, 2)
#
assocstats(resumen) # V de Cramer = 0.322 
#
# Las diferencias entre los dos grupos con respecto al sexo es poco significativa
# Las diferencias no son muy fuertes
#
# 4) Forma
# Tabla de frecuencias
#
resumen <- tumbas_usme %>% select(Grupo, Forma) %>% na.omit() %>% table()
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
chisq.test(resumen) # X-squared = 0.55528, df = 1, p-value = 0.4562
#
# Valores esperados
#
round(chisq.test(resumen)$expected, 2)
#
assocstats(resumen) # V de Cramer = 0.151 
#
# 5) Construcciones internas
# Tabla de frecuencias
#
resumen <- tumbas_usme %>% select(Grupo, Construcciones) %>% na.omit() %>% table()
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
chisq.test(resumen) # Chi cuadrado = 1.276, df = 2, p = 0.5283
#
# Valores esperados
#
round(chisq.test(resumen)$expected, 2)
#
assocstats(resumen) # V de Cramer = 0.151 
# revisar variables numéricas
#
# 6) Marcas
# Tabla de frecuencias
#
marcas <- tumbas_usme %>% select(Grupo, Marcas) %>% na.omit() %>% table()
marcas 
# imprimir
#
# Tabla de porcentajes
#
resumen_p <- marcas 
resumen_p[1,] <- Superposicion[1,] %>% proportions()
resumen_p[2,] <- Superposicion[2,] %>% proportions()
round(resumen_p*100, 2)  # imprimir
#
# Chi cuadrado
#
chisq.test(marcas) # Chi cuadrado = 1.276, df = 2, p = 0.5283
#
# Valores esperados
#
round(chisq.test(marcas)$expected, 2)
#
assocstats(marcas2) # V de Cramer = 0.151 
#
# 7) Superposicion
# Tabla de frecuencias
#
Superposicion <- tumbas_usme %>% select(Grupo, Superposicion) %>% na.omit() %>% table()
Superposicion
# imprimir
#
# Tabla de porcentajes
#
resumen_p <- Superposicion
resumen_p[1,] <- Superposicion[1,] %>% proportions()
resumen_p[2,] <- Superposicion[2,] %>% proportions()
round(resumen_p*100, 2)  # imprimir
#
# Chi cuadrado
#
chisq.test(Superposicion) # X-squared = 4.4482, df = 1, p-value = 0.03494
#
# Valores esperados
#
round(chisq.test(Superposicion)$expected, 2)
#
assocstats(Superposicion) # V de Cramer = 0.304 
# 
# Únicamente hay diferencias en el número de tumbas superpuestas en los dos grupos
#
#
# B) revisar variables numéricas
#
head(tumbas_usme, 0)
#
# Media de todas las variables numéricas
#
media_grupos <- tumbas_usme %>% group_by(Grupo) %>% 
  summarize_at(vars(Edad_err, Edad, Ollas, Jarras, Copas,  Mocasines, ceramicas, 
                    Venado, Cuy, Cricetidae, Boa, Laja, Monton_de_piedras, Volante, Gallinula, Saino, Conchas,
                    Felidae, humanos), ~ mean(.,na.rm=TRUE))
print(media_grupos)
#
# Suma de todas las variables numéricas
#
sum_grupos <- tumbas_usme %>% group_by(Grupo) %>% 
  summarize_at(vars(Ollas, Jarras, Copas,  Mocasines, ceramicas, 
                    Venado, Cuy, Cricetidae, Boa, Laja, Monton_de_piedras, Volante, Gallinula, Saino, Conchas,
                    Felidae, humanos), ~ sum(.,na.rm=TRUE))
print(sum_grupos)
#
# Proporciones de la suma de las variables numéricas por grupo
#
resumen_p <- as.data.frame(sum_grupos)
resumen_p[1,] <- sum_grupos[1,] %>% proportions() 
resumen_p[2,] <- sum_grupos[2,] %>% proportions()
round(resumen_p*100, 2) 
#
# Máximo de todas las variables numéricas
#
max_grupos <- tumbas_usme %>% group_by(Grupo) %>% 
  summarize_at(vars(Ollas, Jarras, Copas,  Mocasines, ceramicas, 
                    Venado, Cuy, Cricetidae, Boa, Laja, Monton_de_piedras, Volante, Gallinula, Saino, Conchas,
                    Felidae, humanos), ~ max(.,na.rm=TRUE))
print(max_grupos)
#
#
# 
# Comparar proporciones de cerámicas
# 
ceramica_grupos <- sum_grupos %>% select(Jarras, Copas, Ollas, Mocasines)
ceramica_grupos # imprimir
#
# Chi cuadrado
#
chi_ceramic <- chisq.test(ceramica_grupos)
#
chi_ceramic # X-squared = 10.667, df = 3, p-value = 0.01367
#
round(chi_ceramic$expected, 2)
#
# Las diferencias entre los dos grupos con respecto a la ceramica son significativas
#
#
# Comparar proporciones de fauna
# 
fauna_grupos <- sum_grupos %>% select(Venado, Cuy, Boa, Gallinula, Saino, 
                                      Conchas, Felidae)
fauna_grupos # imprimir
#
# Chi cuadrado
#
chisq.test(fauna_grupos) # X-squared = 26.706, df = 6, p-value = 0.0001644
#
# Valores esperados
#
round(chisq.test(fauna_grupos)$expected, 2)
#
# V de Cramer
#
sqrt(26.706/(64*(2-1))) # V de Cramer = 0.46
#
#
fauna_grupos_nmi <- data.frame(Venado = c(4,8), cuy = c(0,3), Boa = c(0,1), Gallinula = c(1,0), Saino = c(0,1), Concha = c(3,1), Felino = c(0,1))
#
fauna_grupos_nmi # imprimir
#
n <- sum(fauna_grupos_nmi) 
n # imprimir
#
# Chi cuadrado
#
chisq.test(fauna_grupos_nmi) # X-squared = 7.9382, df = 6, p-value = 0.2427
#
# Valores esperados
#
round(chisq.test(fauna_grupos_nmi)$expected, 2)
#
# V de Cramer
#
sqrt(7.93/(n*(2-1))) # V de Cramer = 0.5871819
#
#
#
# Comparar las edades de los individuos de ambos grupos
#
# Ver los datos
#
tumbas_usme %>% select(Grupo, Edad)
#
# Prueba t
#
t.test(Edad ~ Grupo, data = tumbas_usme)
#
# Media de la edad del grupo 1 = 26.42 y del grupo 2 = 30.62 
# t = -0.39408, df = 6.6823, p = 0.7058
# La diferencia de edad no es muy significativa
#
# Volumen
#
volumen_tumbas <- tumbas_usme %>% select(Grupo, Volumen) 
volumen_tumbas <- na.omit(volumen_tumbas)
#
# Tallo y hojas

# Prueba t
#
t.test(Volumen ~ Grupo, data = tumbas_usme)
#
stem.leaf.backback(volumen_tumbas$Volumen[volumen_tumbas$Grupo == 1],
                            volumen_tumbas$Volumen[volumen_tumbas$Grupo == 2],
                            unit = 0.1)
#
# Area
#
tumbas_usme %>% select(Grupo, Area) %>% na.omit()
#
# Prueba t
#
t.test(Area ~ Grupo, data = tumbas_usme)
#
stem.leaf.backback(na.omit(tumbas_usme$Area[tumbas_usme$Grupo == 1]), 
                   na.omit(tumbas_usme$Area[tumbas_usme$Grupo == 2]))
#
# Profundidad
#
tumbas_usme %>% select(Grupo, Profundidad) %>% na.omit()
#
# Prueba t
#
t.test(Profundidad ~ Grupo, data = tumbas_usme)
#
#