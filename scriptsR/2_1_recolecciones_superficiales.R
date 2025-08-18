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
#      Script para la exploracion de los datos de los muestreos espaciales
#                      en las vías y en los superlotes
#
# Paquetes necesarios
install.packages("rgdal")
#
library(automap)
library(rgdal)
library(sp)
library(raster)
#
#################################################################
#        
# DATOS SUPER LOTES
#
enlace <- "https://raw.githubusercontent.com/rafaelroblesc/Usme/refs/heads/main/superlotes.txt"
super_lotes <- read.table(enlace, header = TRUE, row.names = 1)
#
#
plot(super_lotes$X, super_lotes$Y, asp = 1)
#
hist(super_lotes$Cerámica)
boxplot(super_lotes$Cerámica)
#
data.frame(Lote = row.names(super_lotes),
           Z_cerámica = super_lotes$Cerámica)
#
# el super lote 4 tiene un valor de 3.48, es atípico
# sin el valor 4:
hist(c(super_lotes$Cerámica[1:3],super_lotes$Cerámica[5:16]))
boxplot(c(super_lotes$Cerámica[1:3],super_lotes$Cerámica[5:16]))
Z_cerámica = as.numeric((c(super_lotes$Cerámica[1:3],super_lotes$Cerámica[5:16])-mean(c(super_lotes$Cerámica[1:3],super_lotes$Cerámica[5:16])))/sd(c(super_lotes$Cerámica[1:3],super_lotes$Cerámica[5:16])))
Z_cerámica
hist(Z_cerámica)
boxplot(Z_cerámica)
#
# Crear data frame con datos espaciales y de cerámica
#
xy_cerámica <- data.frame(x=super_lotes$X,
                          y=super_lotes$Y,
                          z=super_lotes$Cerámica)
#
#	Mapa de "burbuja"
#
with(xy_cerámica,plot(x,y,pch=20,cex=z/50, asp = 1))
#
#	Convertir al tipo "sp" espacial
#
coordinates(xy_cerámica) <- ~x+y
#
spplot(xy_cerámica,colorkey=TRUE, scales=list(TRUE))
#
#	Interpolar con Krige
#
help(autoKrige) # Pedir ayuda
superf_cerámica <- autoKrige(z~1, xy_cerámica, block = c(0.5, 0.5), remove_duplicates =  TRUE)
#
str(superf_cerámica)
#
#	Graficar el resultado
#
plot(superf_cerámica)
par(mfrow = c(1,1))
#

#	Extraer el resultado con las coordenadas la predicción, la varianza y la sd
#
ks <- data.frame(superf_cerámica$krige_output)
#
str(ks)
# Las coordenadas están en ks[,1:2], la predicción en ks[,3],
# La varianza en ks[,4] y la sd en ks[,5]
#
# Mapear el resultado
#
# Mapa de calor
#
image(superf_cerámica$krige_output)
# 
# Mapa de contornos
#
contour(superf_cerámica$krige_output)
#
# Mapa de calor y de contornos
#
image(superf_cerámica$krige_output)
contour(superf_cerámica$krige_output, add = TRUE)
#
#	Crear el ráster
#
r <- raster(superf_cerámica$krige_output)
plot(r, asp = 1)
points(super_lotes)
#
# Exportar imagen ráster
#
proj4string(r) <- CRS("+init=epsg:3116") # Asigna sistema de coordenadas
#
writeRaster(r, filename="Recoleccion_superficial_ceramica_escalado.tif", format="GTiff", overwrite=TRUE)
#
# Crear contornos a partir del ráster
#
x <- rasterToContour(r, maxpixels = 100000, nlevels= 6)
plot(x, add = TRUE)
#
proj4string(x) <- CRS("+init=epsg:3116") # Asigna sistema de coordenadas
#
CRSGEO <-  CRS("+init=epsg:4326")		# Asigna a CRSGEO el sistema WGS 84
geox <- spTransform(x, CRSGEO)	# Transforma a el sistema CRSGEO
#
#	Exportar .kml 
writeOGR(geox,dsn="Recoleccion_superficial_ceramica.kml",layer="Sitios",driver="KML",dataset_options=c("NameField=name"))
#

#################################################################
#
# Recolecciones superficiales en las vías
#
enlace <- "https://raw.githubusercontent.com/rafaelroblesc/Usme/refs/heads/main/vias.txt"
total <- read.table(enlace, header = TRUE, row.names = 1)

# Crear data frame con datos espaciales y de cerámica
length(total[,1])
#
xy_total <- data.frame(x=total$X,
                       y=total$Y,
                       z=total$Cerámica)
#
#	Mapa de "burbuja"
#
with(xy_total,plot(x,y,pch=20,cex=z/20, asp = 1))
#
#	Convertir al tipo "sp" espacial
#
coordinates(xy_total) <- ~x+y
#
spplot(xy_total,colorkey=TRUE, scales=list(TRUE))
#
#	Interpolar con Krige
#
help(autoKrige) # Pedir ayuda
superf_total <- autoKrige(z~1, xy_total, block = 0, remove_duplicates =  TRUE)
#
#	Graficar el resultado
#
plot(superf_total)
par(mfrow = c(1,1))
#
#	Extraer el resultado con las coordenadas la predicción, la varianza y la sd
#
ks <- data.frame(superf_total$krige_output)
#
# Las coordenadas están en ks[,1:2], la predicción en ks[,3],
# La varianza en ks[,4] y la sd en ks[,5]
#
# Mapear el resultado
#
# Mapa de calor
#
image(superf_total$krige_output)
# 
# Mapa de contornos
#
contour(superf_total$krige_output)
#
# Mapa de calor y de contornos
#
image(superf_total$krige_output)
contour(superf_total$krige_output, add = TRUE)
#
#	Crear el ráster
#
r <- raster(superf_total$krige_output)
plot(r, asp = 1)
points(xy_total)
#
# Exportar imagen ráster
#
#
proj4string(r) <- CRS("+init=epsg:3116") # Asigna sistema de coordenadas
#
writeRaster(r, filename="Vias.tif", format="GTiff", overwrite=TRUE)
#
# Crear contornos a partir del ráster
#
x <- rasterToContour(r, maxpixels = 100000, nlevels= 9)
plot(x, add = TRUE)
#
proj4string(x) <- CRS("+init=epsg:3116") # Asigna sistema de coordenadas
#
CRSGEO <-  CRS("+init=epsg:4326")		# Asigna a CRSGEO el sistema WGS 84
geox <- spTransform(x, CRSGEO)	# Transforma a el sistema CRSGEO
#
#	Exportar .kml 
writeOGR(geox,dsn="Cotas_Carmen_c.kml",layer="Sitios",driver="KML",dataset_options=c("NameField=name"))
shell.exec("Cotas_Carmen_c.kml") # Ver en google Earth
#
