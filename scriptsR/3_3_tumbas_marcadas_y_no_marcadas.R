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
#       Script para la comparación de las tumbas marcadas y no marcadas
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
# Cargar datos de las tumbas desde github
#
enlace <- "https://raw.githubusercontent.com/rafaelroblesc/Usme/refs/heads/main/Tumbas_coordenadas.txt"
tumbas_usme <- read.table(enlace, header = TRUE, dec = ".", row.names = 1)
# View(tumbas_usme)
#
str(tumbas_usme) # Revisar la tabla de datos
#
# Parte 1: Comparar las tumbas marcadas y no marcadas
#
# marcadas vs no marcadas, forma
#
table(tumbas_usme$Marcada, tumbas_usme$Forma)
#
chisq.test(table(tumbas_usme$Marcada, tumbas_usme$Forma)) # X-squared = 2.6881e-31, df = 1, p-value = 1
#
# Valores esperados
#
round(chisq.test(table(tumbas_usme$Marcada, tumbas_usme$Forma))$expected, 2) 
#
# marcadas vs no marcadas, construcciones internas
#
table(tumbas_usme$Marcada, tumbas_usme$Construcciones)
#
chisq.test(table(tumbas_usme$Marcada, tumbas_usme$Construcciones)) # X-squared = 4.9664, df = 3, p-value = 0.1743
#
# Valores esperados
#
round(chisq.test(table(tumbas_usme$Marcada, tumbas_usme$Construcciones))$expected, 2)
#
# marcadas vs no marcadas, orientación
#
table(tumbas_usme$Marcada, tumbas_usme$Orienta)
#
chisq.test(table(tumbas_usme$Marcada, tumbas_usme$Orienta)) # X-squared = 3.8828, df = 5, p-value = 0.5664
#
# Valores esperados
#
round(chisq.test(table(tumbas_usme$Marcada, tumbas_usme$Orienta))$expected, 2)
#
# marcadas vs no marcadas, volumen
#
mean(na.omit(tumbas_usme$Volumen[tumbas_usme$Marcada == "No_Marcada"]))
#
mean(na.omit(tumbas_usme$Volumen[tumbas_usme$Marcada == "Marcada"]))
#
t.test(Volumen ~ Marcada, data = tumbas_usme) # t = 0.10862, df = 51.001, p-value = 0.9139
#
#
# marcadas vs no marcadas, rangos de edad
#
table(tumbas_usme$Marcada, tumbas_usme$Edad_grup)
#
chisq.test(table(tumbas_usme$Marcada, tumbas_usme$Edad_grup)) # X-squared = 3.8828, df = 5, p-value = 0.5664
#
# Valores esperados
#
round(chisq.test(table(tumbas_usme$Marcada, tumbas_usme$Edad_grup))$expected, 2)
#
# marcadas vs no marcadas, sexo
#
table(tumbas_usme$Marcada, tumbas_usme$Sexo)
#
chisq.test(table(tumbas_usme$Marcada, tumbas_usme$Sexo)) # X-squared = 2.9612, df = 2, p-value = 0.2275
#
# Valores esperados
#
round(chisq.test(table(tumbas_usme$Marcada, tumbas_usme$Sexo))$expected, 2)
#
#
#
# Parte 2: Análisis del vecino más cercano de las marcas
#
#
# Cargar datos de los centroides de las marcas desde github
#
enlace <- "https://raw.githubusercontent.com/rafaelroblesc/Usme/refs/heads/main/marcas.txt"
marcas <- read.table(enlace, header = TRUE, dec = ".", row.names = 1)
print(marcas)
#
str(marcas) # Revisar la tabla de datos
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
points(marcas$X, marcas$Y, pch = 20, col = marcas$Grupo)
#
# Vecino más cercano
# Calcular distancias al vecino mas cercano y la media
#
distNN <- nndist(marcas)
nombres <- row.names(marcas)
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
     main = "Histograma de las distancias al vecino más cercano")
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
Media.esp <- 0.5 / sqrt(length(marcas[,1]) / area.ventana)
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
cat("Hay una confianza del", round((1-P), digits = 5)*100, "% de que el patrÃ³n agrupado no se deba a un error del muestreo")
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

# Hacer un grafico de rangos de error

medias <- rbind(media_observada = resultado$media_observada,
                media_esperada = resultado$media_esperada)

categorias <- factor(rownames(medias))
catn <- length(levels(categorias))

plot(1:catn,
     medias$Media,
     pch = 3,xaxt = "n",
     ylab = "metros",
     xlab = "",
     main = "Grafico de rangos de error al 95% de confianza",
     xlim = c(0.5,catn + 0.5),
     ylim = c(1.2,2.8), 
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