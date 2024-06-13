###
# Clase 2 - Curso de Estadística
# author: Rafa Carretero
# date: 13 junio de 2024
###
#
#######################################
#                                     #
# CARGA DATOS Y LIBRERIAS             #
#                                     #
#######################################
library(dplyr)
library(Hmisc)
library(tableone)
pacientes <- read.table("estudio301.txt", header=TRUE, sep="&")

######################################
#                                    #
# AÑADIR ETIQUETAS                   #
#                                    #
######################################

label(pacientes$EDAD) <- "Edad"
label(pacientes$DiseaseLo) <- "Localización de la enfermedad"
label(pacientes$BPI) <- "Escala de dolor BPI"
label(pacientes$C_ECOG) <- "Escala ECOG basal"
label(pacientes$TRATprevio) <- "N. de tratamiento previos"
label(pacientes$Visceral) <- "Enf. viceral"
label(pacientes$race) <- "Raza"
label(pacientes$muerto) <- "Muertes"
label(pacientes$recaida) <- "Recaída"
levels(pacientes$DiseaseLo)=c("Hueso","Ganglio","Hígado")
levels(pacientes$tratamiento)=c("Placebo","Abiraterona")
levels(pacientes$muerto)=c("Vivo","Fallecido")
levels(pacientes$recaida)=c("No recaída","Recaída")

#
#######################################
#                                     #
# TABLA CARACTERISTICAS GENERALES     #
#                                     #
#######################################
#
CreateTableOne(data = pacientes) # Tabla genérica:

######################################
#                                    #
# TABLA con TABLEONE                 #
#                                    #
######################################
# Tabla más elaborada:
myVars <- c("EDAD", "DiseaseLo", "C_ECOG", "BPI", 
            "TRATprevio", "Visceral", "race", 
            "muerto", "recaida")
## Vector of categorical variables that need transformation
catVars <- c("DiseaseLo", "C_ECOG", "TRATprevio", "race", 
             "tratamiento","muerto", "recaida")
## Create a TableOne object
tab2 <- CreateTableOne(vars = myVars, 
                       data = pacientes, 
                       factorVars = catVars,
                       strata = "tratamiento")
print(tab2, varLabels = TRUE) 
