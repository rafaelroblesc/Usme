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
#         Script para la descripción de los datos de huellas de poste,
#                             tumbas y marcas
#
#
#
#   Paquetes necesarios: sf, spatstat, vcd, tidyverse y stats
#
library(tidyverse) # dplyr ggplot2
library(RcmdrMisc) # prop.table()
library(sf) # read_sf()
library(factoextra) # fviz_pca_biplot()
#
#


# Parte 1: Huellas de poste

# Cargar datos de los centroides de los postes desde github

postes_usme <- read.table("https://raw.githubusercontent.com/rafaelroblesc/Usme/refs/heads/main/Postes_coordenadas.txt",
                          header = TRUE, 
                          dec = ".", 
                          row.names = 1)

postes_usme # Imprimir la tabla de datos

# Media del área

mean(postes_usme$Area)

# Media del diámetro

mean(2*sqrt(postes_usme$Area/pi)*100) # Media del diámetro en cm

# Tangos de error al 95% 

n <- length(postes_usme$Area) 
gl <- n-1 
EE.obs <- sd(na.omit(2*sqrt(postes_usme$Area/pi)*100)) / sqrt(length(na.omit(2*sqrt(postes_usme$Area/pi)*100)))
t95.obs <- abs(qt(0.025, gl))
RE.obs <- EE.obs*t95.obs

RE.obs # Imprimir el rango

#
# Parte 2: Tumbas

# Parte 2.1 cargar datos espaciales, hacer mapa con las tumbas
#
# Cargar datos de los centroides de los tumbas desde github
#
enlace <- "https://raw.githubusercontent.com/rafaelroblesc/Usme/refs/heads/main/Tumbas_coordenadas.txt"
tumbas_usme <- read.table(enlace, header = TRUE, dec = ".", row.names = 1)
view(tumbas_usme)
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
points(tumbas_usme$X, tumbas_usme$Y, pch = 20, col = "gold")
#

# Parte 2.3 DEscripción general de la muestra

# Total de tumbas
#
n <- length(tumbas_usme[,1])
n

# Sexo

table(tumbas_usme$Sexo)
round(prop.table(table(tumbas_usme$Sexo)), 4)

# Edad

mean(na.omit(tumbas_usme$Edad))

hist(na.omit(tumbas_usme$Edad), probability = TRUE, ylim = c(0, 0.04),
     main = "Histograma de las edades", xlab = "Edad", ylab = "Densidad") # histograma
abline(v = mean(na.omit(tumbas_usme$Edad)), col = "red", lty = 6) # Linea de la media
lines(density(na.omit(tumbas_usme$Edad)), col = "blue", lty = 2) # curva de densidad

# Rangos de edad

table(tumbas_usme$Edad_grup)
round(prop.table(table(tumbas_usme$Edad_grup)), 4)

# Orientación

table(tumbas_usme$Orienta)
round(prop.table(table(tumbas_usme$Orienta)), 4)

# forma

table(tumbas_usme$Forma)
round(prop.table(table(tumbas_usme$Forma)), 4)

# cosntrucciones

table(tumbas_usme$Construcciones)
round(prop.table(table(tumbas_usme$Construcciones)), 4)

# volumen

mean(na.omit(tumbas_usme$Volumen))

# Rangos de erros de volúmen a 95%

n <- length(na.omit(tumbas_usme$Volumen)) 
gl <- n-1 
EE.obs <- sd(na.omit(tumbas_usme$Volumen)) / sqrt(length(na.omit(tumbas_usme$Volumen)))
t95.obs <- abs(qt(0.025, gl))
RE.obs <- EE.obs*t95.obs

RE.obs # Imprimir el rango

# Diversidad de objetos en la muestra

# Crear objeto para índice de diversidad de simpson

# Datos

diversidad_obj <- tumbas_usme %>% 
  summarize_at(vars(Ollas, Jarras, Copas,  Mocasines, Venado, Cuy, Cricetidae, Boa, Saino, Conchas, Felidae, humanos, Volante, Propulsor), ~sum(.,na.rm=TRUE))
print(round(diversidad_obj , 2))

# Índice de Simpson

1 - sum(proportions(diversidad_obj)^2)

# Cerámicas

# Datos

ceramicas_total <- tumbas_usme %>% 
  summarize_at(vars(Ollas, Jarras, Copas,  Mocasines), ~sum(.,na.rm=TRUE))

ceramicas_total # Imprimir

round(proportions(ceramicas_total),4) # Tabla de proporciones

# Fauna

# Datos
fauna_total <- tumbas_usme %>% 
  summarize_at(vars(Venado, Cuy, Cricetidae, Boa,  Gallinula, Saino, Conchas, Felidae), ~sum(.,na.rm=TRUE))

fauna_total # Imprimir

round(proportions(fauna_total),4) # Tabla de proporciones

# Marcas sobre las tumbas
# Ingresar datos



table(marcas$Tipo) # Imprimir
round(prop.table(table(marcas$Tipo)), 4)  # Tabla de proporciones



# Parte 2.3 Análisis general de la muestra: Componentes Principales

library(cluster)

tumbas_u_numerico <- data.frame(Fauna = tumbas_usme$Venado + tumbas_usme$Cuy + tumbas_usme$Boa + tumbas_usme$Gallinula + tumbas_usme$Saino + tumbas_usme$Conchas + tumbas_usme$Felidae,
                                Ceramica = tumbas_usme$Ollas + tumbas_usme$Jarras + tumbas_usme$Copas + tumbas_usme$Mocasines,
                                montones = tumbas_usme$Monton_de_piedras,
                                area = tumbas_usme$Area,
                                lajas = tumbas_usme$Laja,
                                row.names = row.names(tumbas_usme))

tumbas_u_numerico

tumbas_escalado <- scale(tumbas_u_numerico)


# # Análisis general de la muestra: ejes de variación otras variables

# Crear tabla de datos numéricos

tumbas_u_numerico <- data.frame(venado = tumbas_usme$Venado,
                                jarras = tumbas_usme$Jarras,
                                ollas = tumbas_usme$Ollas,
                                fauna = tumbas_usme$Boa + tumbas_usme$Saino + tumbas_usme$Conchas + tumbas_usme$Felidae,
                                montones = tumbas_usme$Monton_de_piedras,
                                area = tumbas_usme$Area,
                                lajas = tumbas_usme$Laja,
                                row.names = row.names(tumbas_usme))

tumbas_u_numerico # imprimir

# Escalar

tumbas_escalado <- scale(tumbas_u_numerico)

# Análisis de componentes principales

#	Correlación

corrr <- cor(tumbas_escalado, method = "spearman")

corrr # imprimir

# 	Adelantar el Análisis de Componentes Principales: 

PC <- prcomp(tumbas_escalado, scale=TRUE)

# Ver las distintas partes del objeto

str(PC)
PC$center
PC$scale
PC$rotation
write.csv(PC$rotation, "D:/01DOCTORADO/escritura/PCA_componentes.csv" )
PC$x
PC$sdev

# 	Lo siguiente muestra un gráfico de codo:

plot(PC, type="lines")

# Porcetaje de varianza explicada

fviz_screeplot(PC, addlabels = TRUE, ylim = c(0, 50))

# 	El gráfico "biplot" relaciona variables y componentes: 

fviz_pca_biplot(PC,
                axes = c(1, 2),
                geom.ind = "point",
                geom.var = c("arrow", "text"),
                col.ind = "black",
                fill.ind = "white",
                col.var = "steelblue",
                fill.var = "white",
                title = NULL,
                habillage = tumbas_usme$Edad_grup)



# Parte 3: marcas

# Cargar datos de los centroides de las marcas desde github
#
enlace <- "https://raw.githubusercontent.com/rafaelroblesc/Usme/refs/heads/main/marcas.txt"
marcas <- read.table(enlace, header = TRUE, dec = ".", row.names = 1)
print(marcas)

# Resumen de cantidad de tipos de marcas

table(marcas$Tipo)

round(prop.table(table(marcas$Tipo)), 4)
