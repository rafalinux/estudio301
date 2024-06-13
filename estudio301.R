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

