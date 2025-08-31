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
#  Script para la comparación de la cercanía de las tumbas de Usme y Portalegre
#
#
# Paquetes necesarios
#
library(tidyverse) # dplyr ggplot2
library(vcd) # assocstats()
library(sf) # read_sf() st_transform()
library(aplpack) #stem.leaf.backback()
library(ggplot2) # ggplot
#
#
#
# Comparación con otras comundiades del sur de Bogotá
#
# Cargar datos
#
cercania_sur <- data.frame(Sitio = c(rep("Usme", 61), 
                                     rep("Portalegre", 101), 
                                     rep("Nueva Esperanza", 1225)),
                           menos_de_50cm = c(rep("No",8), 
                                             rep("Si", 53), 
                                             rep("No", 42), 
                                             rep("Si", 59), 
                                             rep("No",796), 
                                             rep("Si", 429)))
#
# Función balasprop, tomada de Víctor González (2019)
# 
balasprop <- function(cat1,cat2,catn, labelposition) {
  T = data.frame(cat1,cat2) 
  Table = xtabs(~T[,1]+T[,2])
  TableR = RcmdrMisc::rowPercents(Table)
  TR = data.frame(TableR)
  TR$n = as.factor(rownames(TR))
  TR$p = TR[,catn]/100
  TR$ser = sqrt(TR$p*(1-TR$p))/sqrt(TR$Total)
  TR$p80 = qt(0.90,  df=TR$Count-1)
  TR$p95 = qt(0.975, df=TR$Count-1)
  TR$p99 = qt(0.995, df=TR$Count-1)
  print(cbind(Categorías = TR$n,
              porcentaje = TR$p*100, 
              ochenta = TR$ser*TR$p80*100, 
              noventaycinco = TR$ser*TR$p95*100, 
              noventaynueve = TR$ser*TR$p99*100))
  ggplot(TR, aes(x=n, y=p)) +
    theme_bw()+
    geom_errorbar(aes(ymin=p-ser*p80, ymax=p+ser*p80),  width=0.0, linewidth=6)+
    geom_errorbar(aes(ymin=p-ser*p95, ymax=p+ser*p95), width=0.0, linewidth=4)+
    geom_errorbar(aes(ymin=p-ser*p99, ymax=p+ser*p99), width=0.0, linewidth=2)+
    geom_point(shape=3, size=7)+
    ylab("Proporción")+ 
    ggtitle(names(TR[(catn)])) + 
    xlab(deparse(substitute(cat1)))+
    annotate("text", 
             x = labelposition, 
             y = max(TR$p+TR$ser*TR$p95), 
             label = "Confianza:  \n  \u2590\u2588\ 80%  \u2588 95%  \u2590 99%")
  
}
#
#
balasprop(cercania_sur$Sitio, cercania_sur$menos_de_50cm, 2, 1)
#
#
# Explroación de la cercanía de las paredes de las tumbas de Usme
#
# Datos Usme
#
cercania<- read.table("https://raw.githubusercontent.com/rafaelroblesc/Usme/refs/heads/main/cercania_usme.txt",
                      header = TRUE, row.names = 1)
#
cercania # Imprimir
#
# Proporción de tumbas cercanas
#
par(mfrow=c(1,2))
# 
plot(0:15,cercania[1:16 , 3], type = "l", ylab = "Proporción de tumbas", xlab = "Distancia (cm)", lty = 2, xaxt = "n")
axis(side = 1, labels = c(as.character(row.names(cercania[1:16,]))), at = 0:15)
points( 0:15, cercania[1:16 , 1], pch = 20, cex = 0.5)
#
plot(0:15,cercania[1:16 , 4], type = "l", ylab = "Aumento", xlab = "Distancia (cm)", lty = 1, , xaxt = "n")
axis(side = 1, labels = c(as.character(row.names(cercania[1:16,]))), at = 0:15)
#
cor.test(cercania[1:16 , 1], 0:15, method = "spearman")
#
modelo <- lm( cercania[, 2] ~ as.numeric(row.names(cercania)))
modelo
#
# Número de grupos
#
cercania$Grupos
#
plot(0:45,cercania[1:46 , 2], type = "l", ylab = "Número de grupos", xlab = "Distancia (cm)", lty = 2, xaxt = "n")
axis(side = 1, labels = c(as.character(row.names(cercania[1:46,]))), at = 0:45)
points( 0:45, cercania[1:46 , 2], pch = 20, cex = 0.5)
#
plot(0:45,cercania[1:46 , 5], type = "l", ylab = "Aumento", xlab = "Distancia (cm)", lty = 2, xaxt = "n")
axis(side = 1, labels = c(as.character(row.names(cercania[1:46,]))), at = 0:45)
abline(h = 0, col = "red", lty = 3)
#
# Tumbas por grupo
#
cercania$Tumb_por_Grup
#
plot(0:45,cercania[1:46 , 6], type = "l", ylab = "Tumbas por grupo", xlab = "Distancia (cm)", lty = 2, xaxt = "n")
axis(side = 1, labels = c(as.character(row.names(cercania[1:46,]))), at = 0:45)
points( 0:45, cercania[1:46 , 6], pch = 20, cex = 0.5)
#
plot(0:45,cercania[1:46 , 7], type = "l", ylab = "Aumento", xlab = "Distancia (cm)", lty = 2, , xaxt = "n")
axis(side = 1, labels = c(as.character(row.names(cercania[1:46,]))), at = 0:45)
abline(h = 0, col = "red", lty = 3)
#
par(mfrow=c(1,1))
#
# Numero de grupos y Numero de tumbas por grupo
#
plot(cercania[ , 2],cercania[, 6], type = "p", ylab = "Tumbas por grupo", xlab = "Cantidad de grupos", pch = 20)
#
plot(cercania[ , 2],cercania[, 7], type = "p", ylab = "Aumento", xlab = "Cantidad de grupos", lty = 2)
#
#
#
# Explroación de la cercanía de las paredes de las tumbas de Portalegre
#
# Datos Portalegre
#
cercania_p <- read.table("https://raw.githubusercontent.com/rafaelroblesc/Usme/refs/heads/main/cercania_portalegre.txt", 
                         header = TRUE, row.names = 1)
#
cercania_p # Imprimir
#
# Número de tumbas cercanas
#
par(mfrow=c(1,2))
#
plot(0:15,cercania_p[1:16 , 3], type = "l", ylab = "Proporción de tumbas", xlab = "Distancia (cm)", lty = 2, xaxt = "n")
axis(side = 1, labels = c(as.character(row.names(cercania[1:16,]))), at = 0:15)
points( 0:15, cercania_p[1:16 , 3], pch = 20, cex = 0.5)
#
plot(0:15,cercania_p[1:16 , 4], type = "l", ylab = "Aumento", xlab = "Distancia (cm)", lty = 1, , xaxt = "n")
axis(side = 1, labels = c(as.character(row.names(cercania_p[1:16,]))), at = 0:15)
#
cor.test(cercania_p[1:16 , 1], as.numeric(row.names(cercania_p)), method = "spearman")
#
modelo <- lm( cercania_p[, 2] ~ as.numeric(row.names(cercania_p)))
modelo
#
#
# Comparar Usme y portalegre
#
cercania_u_p <- read.table(header = TRUE, text = "
Sitio Distancia Tumbas Proporcion
Portalegre 0 29 0.2871
Portalegre 10 36 0.3564
Portalegre 20 41 0.4059
Portalegre 30 46 0.4554
Portalegre 40 48 0.4752
Portalegre 50 59 0.5842
Portalegre 60 60 0.5941
Portalegre 70 66 0.6535
Portalegre 80 71 0.7030
Portalegre 90 72 0.7129
Portalegre 100 75 0.7426
Portalegre 110 80 0.7921
Portalegre 120 83 0.8218
Portalegre 130 83 0.8218
Portalegre 140 83 0.8218
Portalegre 150 84 0.8317
Usme 0 23 0.3770
Usme 10 29 0.4754
Usme 20 41 0.6721
Usme 30 47 0.7705
Usme 40 52 0.8525
Usme 50 53 0.8689
Usme 60 54 0.8852
Usme 70 57 0.9344
Usme 80 58 0.9508
Usme 90 58 0.9508
Usme 100 58 0.9508
Usme 110 58 0.9508
Usme 120 58 0.9508
Usme 130 58 0.9508
Usme 140 59 0.9672
Usme 150 60 0.9836
")
#
# Comparar el aumento en las tumbas cercanas según la distancia
#
par(mfrow = c(1, 1))
#
g <- ggplot(cercania_u_p, aes(Distancia, Proporcion))
#
g+
  geom_point()+
  geom_smooth(col = "grey", lty = 3)+
  facet_grid(. ~ Sitio)+
  theme_bw()
#
#
