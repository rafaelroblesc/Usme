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
#  Script para la exploracion de los datos de las dataciones radiocarbónicas
#
# Paquetes necesarios
# install.packages("rcarbon")
library(rcarbon)
#
# Fechas de Usme
fechas <- read.table(header = TRUE, text = "
ID Fecha Error Sitio
Usme_1 710 40 Usme
Usme_2 410 20 Usme
")
#
# Calibrar
#
xx <- calibrate(x=fechas$Fecha,
                errors= fechas$Error,
                calCurves='intcal20',
                ids= fechas$ID)
#
summary(xx) # Resumen
#
# Graficar las fechas
multiplot(xx, 
          type = 'b',
          label = FALSE,
          credMass = 0.95,
          col.fill = 1,
          lwd = 5,
          decreasing = TRUE,
          rescale = TRUE,
          gapFactor = c(1,2),
          HPD = TRUE, 
          calendar = "BCAD") #revisar "help" para opciones alternativas
#
# Suma de probabilidades
#
spdxx <- spd(xx, c(800,400)) # Suma de probabilidades
#
plot(spdxx, calendar = "BCAD") # Graficar

