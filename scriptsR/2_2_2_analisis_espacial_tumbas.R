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
library(ggplot2) # ggplot() ; aes()
#
#
#
# Parte 1: Análisis del vecino más cercano, Script basado en González (2019)                 
#
# Cargar datos de los centroides de los tumbas desde github
#
enlace <- "https://raw.githubusercontent.com/rafaelroblesc/Usme/refs/heads/main/Tumbas_coordenadas.txt"
tumbas_usme <- read.table(enlace, header = TRUE, dec = ".", row.names = 1)
# View(tumbas_usme)
#
str(tumbas_usme) # Revisar la tabla de datos
#
# Agregar el objeto espacial del area total excavada
#
enlace <- "https://raw.githubusercontent.com/rafaelroblesc/Usme/main/area_excavada.kml"
ventana <- read_sf(enlace)

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
# Encontrar n
#
n <- length(tumbas_usme[,1])
n # imprimir n = 61
#
# Cacular vector de distancias al vecino más cercano
#
distNN <- nndist(tumbas_usme) # Distancias al vecino mas cercano
nombres <- row.names(tumbas_usme) # Extraer los nombres de filas
media_obs <- mean(distNN) # Calcular la media de distancias
#  
cat("La media observada es ", round(media_obs, digits = 2), "m")
#  
# Histograma de distancias
# 
hist(distNN, probability = TRUE, ylim = c(0,1), main = "Histograma",
     xlab = "Distancia", ylab = "Frecuencia") # historigrama
abline(v = media_obs, col = "red", lty = 5) # Linea de la media observada
lines(density(distNN), col = "blue", lty = 2) # Agregar la curva de densidad
#
#
# Calcular area de la excavacion
#
area_ventana <- st_area(ventana)
#
cat("El area de la ventana es", area_ventana, "m2")
#  
# Calcular la media esperada
# 
media_esp <- 0.5 / sqrt(length(tumbas_usme[,1]) / area_ventana)
#  
cat("La media esperada es ", round(media_esp, digits = 2), "m")
#  
# Calcular R
# 
R <- media_obs / media_esp 
#  
cat("El indice de vecino mas cercano es ", round(R, digits = 2))
cat(round(R, digits = 2), "< 1, y por lo mismo hay un patron agrupado")
#  
# Rango de error de distancia observada
#
gl <- n-1 # Grados de libertad
EE_obs <- sd(distNN) / sqrt(length(distNN)) # Error estandar observado
t95_obs <- abs(qt(0.025, gl)) # t para 95% de confianza
RE_obs <- EE_obs*t95_obs # Rango de error
#  
cat("El rango de error de la media observada es ", round(RE_obs, digits = 2), 
    "m al 95% de confianza")
cat("La media observada es", round(media_obs, digits = 2), "m con un rango de error de ", 
    round(RE_obs, digits = 2),"al 95% de confianza" )
#  
# Rango de error de distancia esperada
#
distNN_esp <- rnorm(n = n, mean = Media_esp, sd = sd(distNN)) # distribucion normal
EE_esp <- sd(distNN_esp) / sqrt(length(distNN_esp)) # Error estandar esperado
t95_esp <- abs(qt(0.025, gl)) # t para 95% de confianza
RE_esp <- EE_esp*t95_esp # Rango de error
#
cat("El rango de error de la media esperada es ", round(RE_esp, digits = 2), 
    "m al 95% de confianza")
cat("La media esperada es ", round(Media_esp, digits = 2), "m con un rango de error de ", 
    round(RE_esp, digits = 2),"al 95% de confianza" )
#
#	Significancia
#
SE <- (0.26136 * sqrt(area_ventana)) / n # Error estandar
t  <- (as.numeric(media_esp) - media_obs)/SE # Valor t
P <- as.numeric(dt(t, df = n - 1)) # Valor p
P# imprimir
#
cat("Hay una confianza del", round((1-P), digits = 4)*100, "% de que el patrón aleatorio no se deba a un error del muestreo")
#
# lista de resultados
# 
resultado <-list(ventana = ventana,
                 distancias = data.frame( ID = nombres, distancia = distNN, row.names = TRUE),
                 area_ventana = area_ventana,
                 media_observada = data.frame(Media = media_obs, Error95 = RE_obs),
                 media_esperada = data.frame(Media = as.numeric(media_esp), Error95 = RE_esp),
                 R = as.numeric(R),
                 p = P)
# 
resultado # Imprimir la lista
#
# Hacer un grafico de rangos de error
#
medias <- rbind(media_observada = resultado$media_observada,
                media_esperada = resultado$media_esperada) # Extraer datos
#
categorias <- factor(rownames(medias)) # Extraer categorias
catn <- length(levels(categorias)) # Extraer el numero de categorias
#
plot(1:catn, medias$Media, pch = 3,xaxt = "n", ylab = "metros", xlab = "",
     main = "Grafico de rangos de error al 95% de confianza", xlim = c(0.5,catn + 0.5),
     ylim = c(min(1),max(1.45)),cex  = 2.5, col =1)
arrows(1:catn, medias$Media - medias$Error95, 1:catn, medias$Media + medias$Error95,
       length = .0, angle = 90, code = 3, lend = 1, lwd = 6, col = 1)
axis(side = 1, at = 1:catn, labels = row.names(medias))
#
#
#
# Parte 2: K medias, basado en los scripts de González (2019)                        
#
#
# Plano de densidad de las tumbas
#
ggplot(tumbas_usme, aes(X,Y))+
  geom_density_2d_filled()+
  geom_point()+
  xlim(min(tumbas_usme$X), max(tumbas_usme$X))+
  ylim(min(tumbas_usme$Y), max(tumbas_usme$Y))+
  theme_bw()
#
#
# Grafico de codo para saber el numero optimo de grupos
#
wss <- sum(kmeans(tumbas_usme[,1:2], centers = 1)$withinss, iter.max = 1000)
wss
#
for (i in 1:10) wss[i] <- sum(kmeans(tumbas_usme[,1:2],centers=i)$withinss, iter.max = 1000)
#
wss # imprimir la suma de los cuadrados de 1 a 10 grupos
#
#	Plano con 5 grupos
#
plot(1:5,wss[1:5],type="b",xlab="Numero de Grupos", ylab="Suma de cuadrados",
     main="Gráfico de suma de cuadrados para k+1 grupos", pch=16, cex=1, col = "darkgrey", lty = 2)
abline(v=2, col=2, lty=2) # agrega la linea del numero de grupos
#
#	K medias para 2 grupos
#
(k=2) # asigna 2 grupos
#
kc <- kmeans(tumbas_usme[,1:2], k, iter.max = 1000) # crear el kmeans
print(kc) # Imprimir kc
#
# Asignar la columna de pertencia grupal a la tabla de datos en bruto
#
tumbas_usme$Grupo <- kc$cluster
#
centroides_tumbas <- kc$centers # Extraer los centroides
#
# Plano de los dos grupos
#
V1 <- ripras(subset(tumbas_usme, subset = Grupo == 1), 
                            shape="convex", f = 1) # Crear la ventana del grupo 1
V2 <- ripras(subset(tumbas_usme, subset = Grupo == 2), 
                            shape="convex", f = 1) # Crear la ventana del grupo 2
#
plot(tumbas_usme$X, tumbas_usme$Y, type = "n", xlim = c(95183.79,95225.53), 
     ylim =  c(86920.14,86956.25), ylab = "" , xlab = "", asp = 1, main = "Grupos de tumbas") # Base
plot(ventana$geometry, add = TRUE) # Ventana de la excavacion
plot(V1, add = TRUE, col = "lightgrey") # Ventana del grupo 1
plot(V2, add = TRUE, col = "lightgrey") # Ventana del grupo 2
points(tumbas_usme$X, tumbas_usme$Y, col = "darkgrey", pch = 16) # Tumbas
points(kc$centers[,1], kc$centers[,2], pch = 20) # Centroides
text(x = kc$centers[,1]+3.5, y= kc$centers[,2]+3.5, 
     labels(row.names(kc$centers))) # Nombres de los grupos
#
#
#