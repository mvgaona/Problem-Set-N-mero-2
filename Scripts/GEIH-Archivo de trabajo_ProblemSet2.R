# Valeria Gaona - 202214418
# Andrea Beleño - 200620739

#### PROBLEM SET 2 #####

#Con este script se realizará el scraping de las bases de datos que serán usada 
#para el desarrollo del Problem Set, específicamente puntos 1 y 2.

install.packages("pacman") #Instalar librería si no cuenta con esta 
install.packages("caret")
install.packages("Matrix")
install.packages("recipes")
library(pacman) #Llamar librería
#Se sacaran de p_load las siguientes librerias: e1071 ,EnvStats,knitr,kableExtra,foreign,ISLR2,
p_load(rio, #Instalar librerías que falten
       tidyverse,
       glmnet,
       dplyr,
       readr,
       gamlr,
       tidymodels,
       ggplot2,
       scales,
       ggpubr,
       skimr,
       rvest,
       caret,
       stringr,
       boot,
       caret,
       modeest,
       recipes,
       glmnet,
       stargazer)
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
DTRAIN_P<-data.frame(readRDS("../Elementos_Guardados/train_personas.rds"))
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
DaTRAIN_H<- subset(DTRAIN_H, select = c("id", "Ingtotugarr", "Npersug", "Lp", "Dominio", "Pobre", "P5090", "P5000"))
DaTEST_H <- data.frame()
DaTEST_H<- subset(DTEST_H, select = c("id", "Npersug", "Lp", "Dominio", "P5090", "P5000"))
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
summary(Lp)
#Descripción Dominio
DOM <- factor(DaTRAIN_H$Dominio)
class(DOM)
summary(DOM)
plot(DOM)
library(dplyr)
plot(DaTRAIN_H, col = c("red", "blue"), main = "DOM vs id")
#Mirar como sacar el porcentaje de cada ciudad/ Rural 
#Descripción P5090 = Ocupación de la vivienda habitada
library(tidyverse)
OcViv <- DaTRAIN_H$P5090
class(OcViv)
skim(OcViv)
OcVivl <- factor(OcViv, labels = c("Propia_Pagada", "Propia_Pagando", "Arriendo",
                                        "Usufructo", "Posesión_Sin_Titulo",
                                        "Otra"))
summary(OcVivl)
DaTRAIN_H <- cbind(DaTRAIN_H, OcVivl)
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
#Llamamos las librerias
###Clasificación
table(DaTRAIN_H$Pobre)
#Ridge y Lasso.
set.seed(10101)
modeloR <- lm(Pobre ~ Npersug + Lp + factor(Dominio) + P5000 + factor(OcVivl), data = DaTRAIN_H)
xmodlas<- model.matrix(~Npersug+Lp + factor(Dominio)+P5000+factor(OcVivl), DaTRAIN_H)
ymodlas <- DaTRAIN_H$Pobre
lasso.mod1 <- glmnet(xmodlas, ymodlas, alpha = 1 , lambda = 0.03)
lasso.mod1$beta
tidy(modeloR)
modeloR
library(stargazer)
stargazer(modeloR,type = "text")
mr_coeficientes <- modeloR$coefficients %>%
  enframe(name = "predictor", value = "coeficiente")

df_coeficientes %>%
  filter(predictor != "(Intercept)") %>%
  ggplot(aes(x = predictor, y = coeficiente)) +
  geom_col() +
  labs(title = "Coeficientes del modelo OLS") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 5, angle = 45))
#Modelo1
model1 <- as.formula("Pobre ~ OcVivl")
xmod1<- model.matrix(~OcVivl, DaTRAIN_H)
ymod1 <- DaTRAIN_H$Pobre
lasso.mod1 <- glmnet(xmod1, ymod1, alpha = 1 , lambda = 0.02)
lasso.mod1$beta
library(glmnet)
library(gamlr)
model_log_1 <- stats::glm(model1,family=binomial(link="logit"), data= DaTRAIN_H)
summary(model_log_1)
tidy(model_log_1)
###Prediccion
DaTRAIN_H$PredMod_Log_1 <- stats::predict.glm(model_log_1 , newdata= DaTRAIN_H, type="response")
summary(DaTRAIN_H$PredMod_Log_1)
tidy(PredMod_Log_1) 
mean(DaTRAIN_H$PredMod_Log_1) 
rule= mean(DaTRAIN_H$PredMod_Log_1)
ClasPredMod_Log_1 <- ifelse(DaTRAIN_H$PredMod_Log_1>rule,1,0)
summary(ClasPredMod_Log_1)
head(ClasPredMod_Log_1)
#Clasificación Modelo 1
cm_log1 = confusionMatrix(data= factor(ClasPredMod_Log_1) , 
                         reference= factor(DaTRAIN_H$Pobre) , 
                         mode="sens_spec" , positive="1")
cm_log1

#Modelo2
model2 <- as.formula("Pobre ~ Lp")
Mod_log_2 <- stats::glm(model2,family=binomial(link = "logit"), data= DaTRAIN_H)
tidy(Mod_log_2)
#Predicción
###Prediccion
DaTRAIN_H$PredMod_Log_2 <- stats::predict.glm(Mod_log_2 , newdata= DaTRAIN_H, type="response")
summary(DaTRAIN_H$PredMod_Log_2)
rule2 = mean(DaTRAIN_H$PredMod_Log_2) 
ClasPredMod_Log_2 <- ifelse(DaTRAIN_H$PredMod_Log_2>rule2,1,0)
summary(ClasPredMod_Log_2) 
#Clasificación Modelo 2
cm_log2 = confusionMatrix(data= factor(ClasPredMod_Log_2) , 
                          reference= factor(DaTRAIN_H$Pobre) , 
                          mode="sens_spec" , positive="1")
cm_log2

#Modelo3
model3 <- as.formula("Pobre ~ Lp + OcVivl + Npersug")
Mod_log_3 <- stats::glm(model3,family=binomial(link = "logit"), data= DaTRAIN_H)
tidy(Mod_log_2)
#Predicción
###Prediccion
DaTRAIN_H$PredMod_Log_3 <- stats::predict.glm(Mod_log_3 , newdata= DaTRAIN_H, type="response")
summary(DaTRAIN_H$PredMod_Log_3)
rule3 = mean(DaTRAIN_H$PredMod_Log_3) 
ClasPredMod_Log_3 <- ifelse(DaTRAIN_H$PredMod_Log_3>rule3,1,0)
summary(ClasPredMod_Log_3) 
cm_log3 = confusionMatrix(data= factor(ClasPredMod_Log_3) , 
                          reference= factor(DaTRAIN_H$Pobre) , 
                          mode="sens_spec" , positive="1")
cm_log3
#Modelo4
model4 <- as.formula("Pobre ~ Lp + OcVivl + Npersug + Dominio")
Mod_log_4 <- stats::glm(model4,family=binomial(link = "logit"), data= DaTRAIN_H)
tidy(Mod_log_2)
#Predicción
###Prediccion
DaTRAIN_H$PredMod_Log_4 <- stats::predict.glm(Mod_log_4 , newdata= DaTRAIN_H, type="response")
summary(DaTRAIN_H$PredMod_Log_4)
rule4 = mean(DaTRAIN_H$PredMod_Log_4) 
ClasPredMod_Log_4 <- ifelse(DaTRAIN_H$PredMod_Log_4>rule4,1,0)
summary(ClasPredMod_Log_4) 
cm_log4 = confusionMatrix(data= factor(ClasPredMod_Log_4) , 
                          reference= factor(DaTRAIN_H$Pobre) , 
                          mode="sens_spec" , positive="1")
cm_log4
#Modelo5
model5 <- as.formula("Pobre ~ P5000 + OcVivl")
Mod_log_5 <- stats::glm(model5,family=binomial(link = "logit"), data= DaTRAIN_H)
tidy(Mod_log_5)
#Predicción
###Prediccion
DaTRAIN_H$PredMod_Log_5 <- stats::predict.glm(Mod_log_5 , newdata= DaTRAIN_H, type="response")
summary(DaTRAIN_H$PredMod_Log_5)
head(DaTRAIN_H$PredMod_Log_5)
head(DaTRAIN_H$Pobre)
rule5 = mean(DaTRAIN_H$PredMod_Log_5) 
ClasPredMod_Log_5 <- ifelse(DaTRAIN_H$PredMod_Log_5>rule4,1,0)
summary(ClasPredMod_Log_5) 
cm_log5 = confusionMatrix(data= factor(ClasPredMod_Log_5) , 
                          reference= factor(DaTRAIN_H$Pobre) , 
                          mode="sens_spec" , positive="1")
cm_log5
head(DaTRAIN_H$PredMod_Log_5)
tail(DaTRAIN_H$PredMod_Log_5)
head(ClasPredMod_Log_5)
tail(ClasPredMod_Log_5)
tail(DaTRAIN_H$Pobre)
head(DaTRAIN_H$Pobre)
library(scales)

#Ahora reescalamos un variable de la base de datos
rescale(DaTRAIN_H$OcVivl)
