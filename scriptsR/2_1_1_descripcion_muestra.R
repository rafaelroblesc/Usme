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
library(cluster) # daisy()
library(corrplot) # corrplot()
#
#
# Parte 1: Huellas de poste
#
# Cargar datos de los centroides de los postes desde github
#
postes_usme <- read.table("https://raw.githubusercontent.com/rafaelroblesc/Usme/refs/heads/main/Postes_coordenadas.txt",
                          header = TRUE, 
                          dec = ".", 
                          row.names = 1)
#
postes_usme # Imprimir la tabla de datos
#
# Media del área
#
mean(postes_usme$Area)
#
# Media del diámetro
#
postes_usme$diametro <- 2*sqrt(postes_usme$Area/pi)*100 # Calcular diámetro
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
# Parte 2: Tumbas
#
# Parte 2.1 cargar datos espaciales, hacer mapa con las tumbas
#
# Cargar datos de los centroides de los tumbas desde github
#
enlace <- "https://raw.githubusercontent.com/rafaelroblesc/Usme/refs/heads/main/Tumbas_coordenadas.txt"
tumbas_usme <- read.table(enlace, header = TRUE, dec = ".", row.names = 1)
# view(tumbas_usme)
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
#
ventana <- st_transform(ventana, crs = texto_crs)
#
str(ventana) # Revisar la estructura de la ventana
#
# Hacer el plano del area excavada y de los tumbas
# 
plot(ventana$geometry, reset = TRUE)
points(tumbas_usme$X, tumbas_usme$Y, pch = 20, col = "gold")
#
#
# Parte 2.2 DEscripción general de la muestra de tumbas
#
# Total de tumbas
#
n <- length(tumbas_usme[,1])
n
#
# Sexo
#
table(tumbas_usme$Sexo)
round(prop.table(table(tumbas_usme$Sexo)), 4)
#
# Edad
#
mean(na.omit(tumbas_usme$Edad))
#
hist(na.omit(tumbas_usme$Edad), probability = TRUE, ylim = c(0, 0.04),
     main = "Histograma de las edades", xlab = "Edad", ylab = "Densidad") # histograma
abline(v = mean(na.omit(tumbas_usme$Edad)), col = "red", lty = 6) # Linea de la media
lines(density(na.omit(tumbas_usme$Edad)), col = "blue", lty = 2) # curva de densidad
#
# Rangos de edad
#
table(tumbas_usme$Edad_grup)
round(prop.table(table(tumbas_usme$Edad_grup)), 4)
#
# Orientación
#
table(tumbas_usme$Orienta)
round(prop.table(table(tumbas_usme$Orienta)), 4)
#
# forma
#
table(tumbas_usme$Forma)
round(prop.table(table(tumbas_usme$Forma)), 4)
#
# construcciones
#
table(tumbas_usme$Construcciones)
round(prop.table(table(tumbas_usme$Construcciones)), 4)
#
# volumen
#
mean(na.omit(tumbas_usme$Volumen))
#
# Rangos de error de volúmen a 95%
#
n <- length(na.omit(tumbas_usme$Volumen)) 
gl <- n-1 
EE.obs <- sd(na.omit(tumbas_usme$Volumen)) / sqrt(length(na.omit(tumbas_usme$Volumen)))
t95.obs <- abs(qt(0.025, gl))
RE.obs <- EE.obs*t95.obs
#
RE.obs # Imprimir el rango
#
# Diversidad de objetos en la muestra
#
# Crear objeto para índice de diversidad de simpson
#
# Datos
#
diversidad_obj <- tumbas_usme %>% 
  summarize_at(vars(Ollas, Jarras, Copas,  Mocasines, Venado, Cuy, Boa, Saino, Conchas, humanos, Volante, Propulsor), ~sum(.,na.rm=TRUE))
print(round(diversidad_obj , 2))
#
# Índice de Simpson
#
sum(((diversidad_obj-1)/(sum(diversidad_obj)-1))) # Edward H. Simpson (1949) Measurement of diversity. Nature 163
#
# Cerámicas
#
# Datos
#
ceramicas_total <- tumbas_usme %>% 
  summarize_at(vars(Ollas, Jarras, Copas,  Mocasines), ~sum(.,na.rm=TRUE))
#
ceramicas_total # Imprimir
#
round(proportions(ceramicas_total),4) # Tabla de proporciones
#
# Fauna
#
# Datos
fauna_total <- tumbas_usme %>% 
  summarize_at(vars(Venado, Cuy, Cricetidae, Boa,  Gallinula, Saino, Conchas, Felidae), ~sum(.,na.rm=TRUE))
#
fauna_total # Imprimir
#
round(proportions(fauna_total),4) # Tabla de proporciones
#
#
# Parte 3: Marcas sobre las tumbas
#
# Ingresar datos
enlace <- "https://raw.githubusercontent.com/rafaelroblesc/Usme/refs/heads/main/marcas.txt"
marcas <- read.table(enlace, header = TRUE, dec = ".", row.names = 1)
#
# Resumen tipos de marca
#
table(marcas$Tipo) # Imprimir
round(prop.table(table(marcas$Tipo)), 4)  # Tabla de proporciones
#
#
#
# Parte 4 Análisis general de la muestra: Componentes Principales
# Análisis general de la muestra: ejes de variación
#
# Crear tabla de datos numéricos
#
tumbas_u_numerico <- data.frame(Edad = tumbas_usme$Edad_grup,
                                Área = tumbas_usme$Area,
                                Construcciones = tumbas_usme$Construcciones,
                                Copas = tumbas_usme$Copas,
                               "Huesos humanos" = tumbas_usme$humanos,
                                Venado = tumbas_usme$Venado,
                                Montones = tumbas_usme$Monton_de_piedras,
                                "Fauna exógena" = recode(tumbas_usme$Boa, "'22' = 1") + tumbas_usme$Saino + tumbas_usme$Conchas + tumbas_usme$Felidae,
                                Lajas = tumbas_usme$Laja,
                                Mocasines = tumbas_usme$Mocasines,
                                Ollas = tumbas_usme$Ollas, 
                                Jarras = tumbas_usme$Jarras,
                                Volantes = tumbas_usme$Volante,
                                Superposicion = tumbas_usme$Superposicion, 
                                row.names = row.names(tumbas_usme))
#
# Recodificar variables
#
tumbas_u_numerico$Construcciones <- recode(tumbas_u_numerico$Construcciones, "'Ninguna' = 0; 'Nicho' = 1; 'Escalón' = 2; 'Asán' = 3")
tumbas_u_numerico$Superposicion <- recode(tumbas_u_numerico$Superposicion, "'No' = 0; 'Si' = 1")
tumbas_u_numerico$Área <- recode(tumbas_u_numerico$Área, "0:median(tumbas_u_numerico$Área)-(IQR(tumbas_u_numerico$Área)/2) = 1;
                                 median(tumbas_u_numerico$Área)-(IQR(tumbas_u_numerico$Área)/2):median(tumbas_u_numerico$Área) = 2;
                                 median(tumbas_u_numerico$Área):median(tumbas_u_numerico$Área)+(IQR(tumbas_u_numerico$Área)/2) = 3;
                                 median(tumbas_u_numerico$Área)+(IQR(tumbas_u_numerico$Área)/2):max(tumbas_u_numerico$Área) = 4")
tumbas_u_numerico$Edad <- recode(tumbas_u_numerico$Edad, "'Indeterminado' = NA; 'Infantil' = 1; 'Adulto' = 2; 'Adulto_m' = 3")
#
tumbas_u_numerico # imprimir
#
# Escalar
#
tumbas_escalado <- scale(na.omit(tumbas_u_numerico))
#
# Análisis de componentes principales
#
#	Correlación
#
corrr <- cor(na.omit(tumbas_escalado, method = "pearson"))
par(mar = c(9,3,2,2))
#
# Gráfico de caja
#
boxplot(corrr,  las = 2)
#
# Gráfico de la correlación
#
par(mar = c(2,2,2,2))
corrplot(corrr, diag = FALSE, tl.col = 1, order = "original", addCoef.col = 'darkgrey')
#
# Gráfico con niveles de confianza
# 
res1 = cor.mtest(na.omit(tumbas_escalado), method = "pearson", conf.level = 0.95)
res2 = cor.mtest(na.omit(tumbas_escalado), method = "pearson",conf.level = 0.99)
#
corrplot(corrr, p.mat = res2$p, low = res2$lowCI, upp = res2$uppCI,
         order = 'original', tl.col = 1, pch.col = 'grey', sig.level = 0.1, addrect = 3,
         rect.col = 'navy', plotCI = 'rect', cl.pos = 'n')
#
# Graficar correlación y dendrograma
#
col <- colorRampPalette(c("darkred", "white","darkblue"))(100)
heatmap(x = corrr, col = col, Rowv = FALSE, symm = FALSE, scale = "none")
#
corrr # imprimir
#
# Análisis de Componentes Principales: 
#
PC <- prcomp(tumbas_escalado, scale=TRUE)
#
# Ver las distintas partes del objeto
#
str(PC)
PC$center
PC$scale
PC$rotation
#write.csv(PC$rotation, "PCA_componentes.csv" )
PC$x
PC$sdev
#
# Gráfico de codo:
#
plot(PC, type="lines")
#
# Porcetaje de varianza explicada
#
fviz_screeplot(PC, addlabels = TRUE, ncp = 14, barfill = "Grey", barcolor = "grey", ylim = c(0, 22), main = "", ylab = "Porcentaje de varianza explicada", xlab = "Dimensiones")
#
# 	El gráfico "biplot" relaciona variables y componentes: 
# help(fviz_pca_biplot)
fviz_pca_biplot(PC,
                axes = c(1, 2),
                geom.ind = "point",
                geom.var = c("arrow", "text"),
                col.ind = "black",
                fill.ind = "white",
                col.var = "steelblue",
                fill.var = "white",
                title = NULL,
                habillage = tumbas_usme$Sexo[tumbas_usme$Edad_grup != "Indeterminado"],
                palette = c("black", "blue", "red"))
#
#
# Parte 5: análisis de agrupamientos
#
# Matrices de distancia
#
(DISTMAT <- daisy(tumbas_escalado, metric = "gower")) # Calcula matriz de distancia
#
# Evaluar si hay mucha o poca distancia entre casos
#
hist(DISTMAT) 
#
# Calcular coeficiente de aglomeración
#
aglomeracion <- agnes(tumbas_escalado, method = "complete")
cat("el coeficiente de aglomeración es ", aglomeracion$ac, "\n")
#
# Hacer el dendrograma
#
summary(DISTMAT) # mediana, media y cuartiles de la matriz de distancia
#
hc <- hclust(DISTMAT, method = "average") # crea datos para construir dendrograma
dendrohc <- as.dendrogram(hc)
#
# Ver el dendrograma
#
par(mfrow = c(1,1))
par(mar = c(2,2,2,5))
plot(dendrohc, edgePar = list(col = 1:2, lty = 2:3),
     edge.root = TRUE, horiz = TRUE) # Plano de dendrograma
abline(v = c(seq(0,0.6, by=0.1)), lty = 3, col = "lightgrey") # lineas
#
#	Agrupamiento por variables:
par(mar = c(9,2,2,2))
dv <- dist(1-cor(tumbas_escalado, method = "pearson")) # Correlaciones
#
hc <- hclust(dv, method="complete")
plot(as.dendrogram(hc, hang=-1))
rect.hclust(hc,k = 6, cluster = cutree(hc, 6), border = "darkgrey")
abline(v = c(seq(0.5,4, by=0.5)), lty = 3, col = "lightgrey") # lineas
#
par(mar = c(2,2,2,2))
#
#
#
# Parte 6: Análisis de correspondencia
#
# Paquetes
#
library("FactoMineR")
library("ca")
#
# Análisis de correspondencia
#
ca_ht <- CA(na.omit(tumbas_u_numerico), graph=TRUE, ncp = 10)
#
summary(ca_ht, nb.dec = 2, ncp = 2) # Resumen
#
filas <- get_ca_row(ca_ht) # Extraer filas
str(ca_ht) # Revisar
ca_ht # Imprimir
#
# Coordenadas
#
filas$coord
#
# Porcentaje de cosntribución
#
filas$contrib
#
# Graficar
#
fviz_ca_row(ca_ht, col.row = "cos2",
            gradient.cols = c("orange", "yellow","salmon"), 
            repel = TRUE)
#
# Contribución de los casos a las dimensiones
#
corrplot(filas$cos2, is.corr=FALSE)
#
# Extraer columnas
#
cols <- get_ca_col(ca_ht)
#
# Coordenadas
#
cols$coord
#
# Porcentaje de cosntribución
#
cols$contrib
#
cols$cos2
#
# Graficar Dimensiones 1 y 2
#
fviz_ca_col(ca_ht, col.col = "cos2", 
            gradient.cols = c("grey","salmon","steelblue"),
            repel = TRUE)
#
# Graficar con casos
#
fviz_ca_biplot(ca_ht, repel = TRUE)
