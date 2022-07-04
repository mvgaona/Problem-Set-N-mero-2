# Valeria Gaona - 202214418
# Andrea Beleño - 200620739

#### PROBLEM SET 2 #####

#Con este script se realizará el scraping de las bases de datos que serán usada 
#para el desarrollo del Problem Set, específicamente puntos 1 y 2.

install.packages("pacman") #Instalar librería si no cuenta con esta 
library(pacman) #Llamar librería
p_load(rio, #Instalar librerías que falten
       tidyverse,
       glmnet,
       dplyr,
       readr,
       gamlr,
       e1071,
       EnvStats,
       tidymodels,
       ggplot2,
       scales,
       ggpubr,
       knitr,
       kableExtra,
       foreign,
       skimr,
       rvest,
       caret,
       stringr,
       boot,
       caret,
       ISLR2,
       stargazer,
       modeest,
       recipes)

rm(list = ls()) #Limpia las variables que existan al momento de correr el código


###Base de datos Problem set 2

library(readr)

#Datos_test_hogares<-read.csv("../Elementos_Guardados/test_hogares.csv") #Guardar las bases de datos
#Datos_test_personas<-read.csv("../Elementos_Guardados/test_personas.csv") #Guardar las bases de datos

#Datos_training_hogares<-read.csv("../Elementos_Guardados/train_hogares.csv") #Guardar las bases de datos
#Datos_training_personas<-read.csv(unzip("../Elementos_Guardados/train_personas.zip","train_personas.csv"))#Se extrae del .zip, teniendo en cuenta que es un archivo muy grande para subir a Github
#file.remove('train_personas.csv')#"Por si se requiere borrar algún archivo

#Se debe poner el directorio de donde está el script:
#Session-> Set Working directory -> To source file location, para lo cual se debe descargar el repositorioDatos_test_hogares<-readRDS("../Elementos_Guardados/test_hogares.rds") #Guardar las bases de datos
setwd("C:/Users/valer/Desktop/Andes/Intersemestral/Big Data/Problem Set 2/Problem-Set-2/Scripts")
DTEST_P<-data.frame(readRDS("../Elementos_Guardados/test_personas.rds"))  #Guardar las bases de datos
DTEST_H <- data.frame(readRDS("../Elementos_Guardados/test_hogares.rds"))
DTRAIN_H<-data.frame(readRDS("../Elementos_Guardados/train_hogares.rds")) #Guardar las bases de datos
DREAIN_P<-data.frame(readRDS("../Elementos_Guardados/train_personas.rds"))
summary(DTRAIN_H$Lp)
summary(DTEST_H$Lp)
plot(hist(DTRAIN_H$Lp),main="Distribución Línea Pobreza, train Hogares",
     xlab="Lp",
     ylab="Frecuencia")
plot(hist(DTEST_H$Lp),main="Distribución Línea Pobreza, test Hogares",
     xlab="Lp",
     ylab="Frecuencia")
#Para formular el modelo, es necesario contar con las variables que expliquen la pobreza de un hogar y con el parámetro definido de la línea de pobreza para analizar si los hogares son pobres o no: $257.000
#Vamos a utilizar: #ingtotugarr, Npersug, lp, dominio de la casa, id
#Vamos crear 2 bases de datos, en donde solo se cuenten las variablers de interés.
DaTRAIN_H <- data.frame()
DaTRAIN_H<- subset(DTRAIN_H, select = c("id", "Ingtotugarr", "Npersug", "Lp", "Dominio", "Pobre", "P5090"))
DaTEST_H <- data.frame()
DaTEST_H<- subset(DTEST_H, select = c("id", "Npersug", "Lp", "Dominio", "P5090"))
#Se procede análisis de las variables:
cantidad_na <- sapply(DaTRAIN_H, function(x) sum(is.na(x)))
cantidad_na <- data.frame(cantidad_na)
porcentaje_na <- cantidad_na/nrow(DaTRAIN_H)
porcentaje_na <-porcentaje_na*100
porcentaje_na #Visualizo el porcentaje de los datos que tienen N
#Se evidencia que la base de datos DaTRAIN_H no cuenta con missing values.
cantidad_na_th <- sapply(DaTEST_H, function(x) sum(is.na(x)))
cantidad_na_th <- data.frame(cantidad_na_th)
porcentaje_na_th <- cantidad_na_th/nrow(DaTEST_H)
porcentaje_na_th <-porcentaje_na_th*100
porcentaje_na_th
#Se evidencia que no existen variables con missing values.
#Análisis de cada variable:
#Descripción Npersug
Npersug<- DaTRAIN_H$Npersug
class(Npersug)
plot(hist(Npersug),col = "blue", main="Histograma Número de personas en la unidad de gasto",
     xlab="Npersug",
     ylab="Frecuencia")
mean(Npersug)
min(Npersug)
max(Npersug)
mean(Npersug)
modeNpersug <- function(Npersug){
  return(as.numeric(names(which.max(table(Npersug)))))
}
modeNpersug(Npersug)
#Descipción Lp
Lp<- DaTRAIN_H$Lp
class(Lp)
plot(hist(Lp),col = "red", main="Histograma Línea de pobreza",
     xlab="Lp",
     ylab="Frecuencia")
min(Lp)
max(Lp)
mean(Lp)
modeLp <- function(Lp){
  return(as.numeric(names(which.max(table(Lp)))))
}
modeLp(Lp)
#Descripción Dominio
DOM <- factor(DaTRAIN_H$Dominio)
class(DOM)
summary(DOM)
plot(DOM)
#Mirar como sacar el porcentaje de cada ciudad/ Rural 
#Descripción P5090 = Ocupación de la vivienda habitada
OcViv <- DaTRAIN_H$P5090
class(OcViv)
skim(OcViv)
OcVivl <- factor(OcViv, labels = c("Propia_Pagada", "Propia_Pagando", "Arriendo",
                                        "Usufructo", "Posesión_Sin_Titulo",
                                        "Otra"))
summary(OcVivl)
#Categorización P5090 en DaTEST_H
OcVivT <- DaTEST_H$P5090
class(OcVivT)
skim(OcVivT)
OcVivTl <- factor(OcVivT, labels = c("Propia_Pagada", "Propia_Pagando", "Arriendo",
                                   "Usufructo", "Posesión_Sin_Titulo",
                                   "Otra"))
summary(OcVivTl)

DaTEST_H<- cbind(DaTEST_H, OcVivTl)
#Falta porcentaje
###Clasificación
table(DaTRAIN_H$Pobre)
#Modelo1
model_log_1 <- glm(Pobre ~ factor(OcVivl),family="binomial", data= DaTRAIN_H)
summary(model_log_1)
tidy(model_log_1)
###Prediccion
PredMod_Log_1 = predict(model_log_1 , newdata= DaTRAIN_H, type="response")
summary(PredMod_Log_1)
tidy(PredMod_Log_1) 





