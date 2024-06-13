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

######################################
#                                    #
# SUPERVIVENCIA                      #
#                                    #
######################################
library(survival)
library(ggsurvfit)
library(survminer)

#las variables "muerto" y "recaida" tienen que volver a ser
# números, porque si no, el análisis de supervivencia no sale
pacientes$muerto <- as.numeric(pacientes$muerto)
pacientes$recaida <- as.numeric(pacientes$recaida)

#######################################
#                                     #
# CURVA DE KAPLAN-MEIER (I)           #
#                                     #
#######################################
#
km_fit <- survfit(Surv(time_Global, muerto) ~ 1, data=pacientes)

curva_1 <- ggsurvplot(km_fit, 
                      data = pacientes, 
                      risk.table = TRUE, 
                      conf.int = FALSE,
                      #pval=TRUE,
                      ggtheme = theme_bw(),
                      #xlim = c(0, 6400),
                      #xscale = "d_y",
                      legend = c("bottom"),
                      title = "Curva de Kaplan-Meier (supervivencia global)",
                      xlab = "Tiempo (meses)",
                      ylab = "Supervivencia (probabilidad)",
                      risk.table.title="Pacientes en riesgo",
                      censor = FALSE
)
curva_1$plot + scale_linetype_manual(values = c("solid","dashed", "solid", "dotted", "dotdash")) +
  scale_colour_manual(values = c("steelblue","red","red","red")) 

#######################################
#                                     #
# CURVA DE KAPLAN-MEIER (II)          #
#                                     #
#######################################
#
km_fit <- survfit(Surv(time_f, recaida) ~ 1, data=pacientes)

curva_1 <- ggsurvplot(km_fit, 
                      data = pacientes, 
                      risk.table = TRUE, 
                      conf.int = FALSE,
                      #pval=TRUE,
                      ggtheme = theme_bw(),
                      #xlim = c(0, 6400),
                      #xscale = "d_y",
                      legend = c("bottom"),
                      title = "Curva de Kaplan-Meier (supervivencia libre de enfermedad)",
                      xlab = "Tiempo (meses)",
                      ylab = "Supervivencia (probabilidad)",
                      risk.table.title="Pacientes en riesgo",
                      censor = FALSE
)
curva_1$plot + scale_linetype_manual(values = c("solid","dashed", "solid", "dotted", "dotdash")) +
  scale_colour_manual(values = c("red")) 

#######################################
#                                     #
# CURVA DE KAPLAN-MEIER (III)         #
#                                     #
#######################################
#
km_fit <- survfit(Surv(time_Global, muerto) ~ tratamiento, data=pacientes)

curva_2 <- ggsurvplot(km_fit, 
                      data = pacientes, 
                      risk.table = TRUE, 
                      conf.int = FALSE,
                      #pval=TRUE,
                      ggtheme = theme_bw(),
                      #xlim = c(0, 6400),
                      #xscale = "d_y",
                      legend = c("bottom"),
                      title = "Curva de Kaplan-Meier (supervivencia global) \n (Abiterona vs Placebo)",
                      xlab = "Tiempo (meses)",
                      ylab = "Supervivencia (probabilidad)",
                      risk.table.title="Pacientes en riesgo",
                      pval = TRUE,
                      censor = FALSE
)
curva_2
superv_1 <- curva_2$plot + 
  theme_bw() +
  scale_linetype_manual(values = c("solid","dashed", "solid", "dotted", "dotdash")) +
  scale_colour_manual(values = c("#2E9FDF","#E7B800","steelblue","red","red","red")) +
  labs(title = "Curva de Kaplan-Meier",
       subtitle="Supervivencia global")+  
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position="bottom") +
  theme(legend.title = element_blank()) +
  theme(panel.background = element_rect(colour = "black"),
        axis.text=element_text(size=8),  #tamaño de las fechas
        axis.title.x = element_text(vjust=-0.2),
        axis.title.y = element_text(vjust=+0.6),
        axis.title=element_text(size=10,face="bold"), #tamaño de los títulos de los ejes
        plot.title = element_text(size = 11, hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(size = 9, hjust = 0.5, color = "grey40"),
        plot.caption = element_text(size = 7.5, color = "grey40"))
superv_1

#######################################
#                                     #
# CURVA DE KAPLAN-MEIER (IV)          #
#                                     #
#######################################
#
km_fit <- survfit(Surv(time_f, recaida) ~ tratamiento, data=pacientes)

curva_3 <- ggsurvplot(km_fit, 
                      data = pacientes, 
                      risk.table = TRUE, 
                      conf.int = FALSE,
                      #pval=TRUE,
                      ggtheme = theme_bw(),
                      #xlim = c(0, 6400),
                      #xscale = "d_y",
                      legend = c("bottom"),
                      title = "Curva de Kaplan-Meier (libre de enfermedad) \n (Abiterona vs placebo)",
                      xlab = "Tiempo (meses)",
                      ylab = "Supervivencia (probabilidad)",
                      risk.table.title="Pacientes en riesgo",
                      pval = TRUE,
                      censor = FALSE
)
superv_2 <- curva_3$plot + 
  theme_bw() +
  scale_linetype_manual(values = c("solid","dashed", "solid", "dotted", "dotdash")) +
  scale_colour_manual(values = c("#2E9FDF","#E7B800","steelblue","red","red","red")) +
  labs(title = "Curva de Kaplan-Meier",
       subtitle="Supervivencia libre de enfermedad")+  
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position="bottom") +
  theme(legend.title = element_blank()) +
  theme(panel.background = element_rect(colour = "black"),
        axis.text=element_text(size=8),  #tamaño de las fechas
        axis.title.x = element_text(vjust=-0.2),
        axis.title.y = element_text(vjust=+0.6),
        axis.title=element_text(size=10,face="bold"), #tamaño de los títulos de los ejes
        plot.title = element_text(size = 11, hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(size = 9, hjust = 0.5, color = "grey40"),
        plot.caption = element_text(size = 7.5, color = "grey40"))
superv_2
