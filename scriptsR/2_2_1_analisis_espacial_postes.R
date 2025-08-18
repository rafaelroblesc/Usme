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
#       Script para el análisis espacial de las huellas de poste de la 
#     comunidad local de Usme. Análisis del vecino más cercano y k medias
#
#
# Paquetes necesarios: sf, spatstat y ggplot2
#
library(sf) # read_sf() ; st_transform() ; st_area()
library(spatstat) # ripras() ; nndist()
library(ggplot2) # ggplot()
#
#
#
# Parte 1: Vecino mas cercano. Script basado en González (2019)
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
# Agregar el objeto espacial del area total excavada
#
ventana <- read_sf("https://raw.githubusercontent.com/rafaelroblesc/Usme/main/area_excavada.kml")
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
# Hacer el plano del area excavada y de los postes
#
plot(ventana$geometry, reset = TRUE)
points(postes_usme$X, postes_usme$Y, pch = 20, col = "grey")
#
# Calcular distancias al vecino mas cercano y la media
#
distNN <- nndist(postes_usme)
nombres <- row.names(postes_usme)
media.obs <- mean(distNN)
#
cat("La media observada es ", round(media.obs, digits = 2), "m")
#
# Histograma de distancias
#
hist(distNN, 
     probability = TRUE,
     xlab = "Distancia",
     ylab = " ",
     main = "Histograma de las distancias al vecino m?s cercano")
abline(v = media.obs,
       col = "red", lty = 5)
lines(density(distNN))
#
# Calcular area de la excavacion
#
area.ventana <- st_area(ventana)
#
cat("El area de la ventana es", area.ventana, "m2")
#
# Calcular la media esperada
#
Media.esp <- 0.5 / sqrt(length(postes_usme[,1]) / area.ventana)
#
cat("La media esperada es ", round(Media.esp, digits = 2), "m")
#
# Calcular R
#
R <- media.obs / Media.esp 
#
cat("El indice de vecino mas cercano es ", round(R, digits = 2))
cat(R, "<1, y por lo mismo hay un patron agrupado")
#
# Rango de error de distancia observada
#
n <- length(distNN) 
gl <- n-1 
EE.obs <- sd(distNN) / sqrt(length(distNN))
t95.obs <- abs(qt(0.025, gl))
RE.obs <- EE.obs*t95.obs
#
cat("El rango de error de la media observada es ", round(RE.obs, digits = 2), "m al 95% de confianza")
cat("La media observada es", round(media.obs, digits = 2), "m con un rango de error de ", round(RE.obs, digits = 2),"al 95% de confianza" )
#
# Rango de error de distancia esperada
#
distNN.esp <- rnorm(n = n, mean = Media.esp, sd = sd(distNN))
EE.esp <- sd(distNN.esp) / sqrt(length(distNN.esp))
t95.esp <- abs(qt(0.025, gl))
RE.esp <- EE.esp*t95.esp
#
cat("El rango de error de la media esperada es ", round(RE.esp, digits = 2), "m al 95% de confianza")
cat("La media esperada es ", round(Media.esp, digits = 2), "m con un rango de error de ", round(RE.esp, digits = 2),"al 95% de confianza" )
#
#	Significancia
#
SE <- (0.26136 * as.numeric(sqrt(area.ventana))) / n
t  <- (as.numeric(Media.esp) - media.obs)/SE
P <- dt(t, df = n - 1)
P
#
cat("Hay una confianza del", round((1-P), digits = 5)*100, "% de que el patrón agrupado no se deba a un error del muestreo")
#
# lista de resultados
#
resultado <-list(ventana = ventana,
                 distancias = data.frame( ID = nombres,
                                          distancia = distNN ,
                                          row.names = TRUE),
                 area_ventana = area.ventana,
                 media_observada = data.frame(Media = media.obs,
                                              Error95 = RE.obs),
                 media_esperada = data.frame(Media = as.numeric(Media.esp),
                                             Error95 = RE.esp),
                 R = as.numeric(R),
                 p = P)
#
resultado # Imprimir la lista
#
# Hacer un grafico de rangos de error
#
medias <- rbind(media_observada = resultado$media_observada,
                media_esperada = resultado$media_esperada)
#
categorias <- factor(rownames(medias)) # Extraer nombres de categorías
catn <- length(levels(categorias))
#
plot(1:catn,
     medias$Media,
     pch = 3,xaxt = "n",
     ylab = "metros",
     xlab = "",
     main = "Grafico de rangos de error al 95% de confianza",
     xlim = c(0.5,catn + 0.5),
     ylim = c(min(.4),max(1)), 
     cex  = 2.5, 
     col = 1)
arrows(1:catn,
       medias$Media - medias$Error95,
       1:catn,
       medias$Media + medias$Error95,
       length = .0,
       angle = 90,
       code = 3,
       lend = 1,
       lwd = 6, 
       col = 1)
axis(side = 1,
     at = 1:catn,
     labels = row.names(medias))
#
#
#
# Parte 2: K medias, basado en González (2019)
#
# Plano de densidad de las huellas de poste
#
ggplot(postes_usme, aes(X,Y, asp=1))+
  geom_density_2d_filled()+
  geom_point()+
  xlim(min(postes_usme$X), max(postes_usme$X))+
  ylim(min(postes_usme$Y), max(postes_usme$Y))+
  theme_bw()
#
# Grafico de codo para saber el numero optimo de grupos
#
wss <- sum(kmeans(postes_usme, centers = 1)$withinss, 
           iter.max = 1000)
#
wss # Imprimir el resultado
#
for (i in 1:10) wss[i] <- sum(kmeans(postes_usme,centers=i)$withinss, 
                              iter.max = 1000)
#
wss # imprimir la suma de los cuadrados de 1 a 10 grupos
#
#	Grafico de codo con 5 grupos
#
plot(1:5,
     wss[1:5],
     type="b",
     xlab="Numero de Grupos", 
     ylab="Suma de cuadrados",
     main="Gr?fico de suma de cuadrados para k+1 grupos", 
     pch=16, 
     cex=1, 
     col = "darkgrey", 
     lty = 2)
abline(v=2, 
       col=2, 
       lty=2) # agrega la linea del numero de grupos
#
#	K medias para 2 grupos
#
(k=2) # asigna 2 grupos
#
kc <- kmeans(postes_usme, k, iter.max = 1000) # crear el kmeans
print(kc) # Imprimir kc
#
# Asignar la columna de pertencia grupal a la tabla de datos en bruto
#
postes_usme$Grupo <- kc$cluster
#
# Extraer los centroides
#
centroides_postes <- kc$centers
#
# Plano de los dos grupos
#
V1 <- ripras(subset(postes_usme, subset = Grupo == 1), shape="convex", f = 1.1)
V2 <- ripras(subset(postes_usme, subset = Grupo == 2), shape="convex", f = 1.1)
#
plot(postes_usme$X, postes_usme$Y, type = "n", xlim = c(95183.79,95225.53), ylim =  c(86920.14,86956.25), ylab = "" , xlab = "", asp = 1, main = "Grupos de huellas de poste")
plot(ventana$geometry, add = TRUE)
plot(V1, add = TRUE, col = "lightblue")
plot(V2, add = TRUE, col = "lightblue")
points(postes_usme$X, postes_usme$Y, col = "darkblue", pch = 20,  cex = 1)
points(kc$centers[,1], kc$centers[,2], pch = 20, col = "black")
#
#