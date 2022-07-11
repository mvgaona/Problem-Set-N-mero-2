# Valeria Gaona - 202214418
# Andrea Beleño - 200620739

#### PROBLEM SET 2 #####

#Con este script se realizará el scraping de las bases de datos que serán usada 
#para el desarrollo del Problem Set, específicamente puntos 1 y 2.

install.packages("pacman") #Instalar librería si no cuenta con esta 
library(pacman) #Llamar librería
#Se sacaran de p_load las siguientes librerias: e1071 ,EnvStats,knitr,kableExtra,foreign,ISLR2,
p_load(caret, 
       Matrix,
       recipes,
       rio, #Instalar librerías que falten
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
#setwd("C:/Users/valer/Desktop/Andes/Intersemestral/Big Data/Problem Set 2/Problem-Set-2/Scripts")
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
#Descripción de P5000
P5000<- DaTRAIN_H$P5000
class(P5000)
plot(hist(P5000),col = "black", main="Histograma No. de cuartos de la vivienda",
     xlab="P5000",
     ylab="Frecuencia")
min(P5000)
max(P5000)
mean(P5000)
modeLp <- function(P5000){
  return(as.numeric(names(which.max(table(P5000)))))
}

modeLp(P5000)
summary(P5000)
#Llamamos las librerias
###Clasificación
table(DaTRAIN_H$Pobre)
#Ridge y Lasso para modelo de Clasificación
#lasso.mod1
#lasso.mod1$beta
#tidy(modeloR)
#modeloR
library(stargazer)

DaTRAIN_H<-DaTRAIN_H %>% mutate(Pobre_dummy=factor(Pobre,levels=c(1,0), labels=c("Si", "No")))
table(DaTRAIN_H$Pobre_dummy)
#Se dividirá la base Training personas para obtener las siguientes bases: otra train, mini test y la evaluation para calcular el ROC
require(caret)
set.seed(10101)
Split_1 <- createDataPartition(DaTRAIN_H$Pobre, p = .7) [[1]]
length(Split_1)

other_ <- DaTRAIN_H[-Split_1,]
DaTRAIN_H_mini<- DaTRAIN_H[ Split_1,] #Base mini train

set.seed(10101)
Split_2<- createDataPartition(other_$Pobre, p = 1/3) [[1]]
Evaluation_H <- other_[ Split_2,] #Base evaluacion para ROC
Testing_H <- other_[-Split_2,] #Base mini test

#Se realiza el K-fold como método de control del modelo

Varios_parametros<-function(...)c(twoClassSummary(...), defaultSummary(...))

ctrl_def_modg <- trainControl(method = "cv",
                         number = 5,
                         summaryFunction = Varios_parametros,
                         classProbs = TRUE,
                         verbose=FALSE,
                         savePredictions = T)
#logit
set.seed(10101)
#Se realiza el modelo de clasificacón con la base de control 
logit_caret_modg <- train(
  Pobre_dummy ~Npersug+Lp +factor(OcVivl)+ factor(Dominio)+P5000,
  data =DaTRAIN_H_mini ,
  method = "glm", #Para logit
  trControl = ctrl_def_modg,
  family = "binomial",
  preProcess = c("center", "scale"))

logit_caret_modg

#Lambdas para Lasso
lambdas <- 10^seq(-4, 0.01, length = 200)

#Ahora, se hará la prueba tomando como métrica la Sensibilidad
set.seed(10101)
logit_lasso_acc <- train(
  Pobre_dummy ~Npersug+Lp +factor(OcVivl)+ factor(Dominio)+P5000,
  data = DaTRAIN_H_mini,
  method = "glmnet",
  trControl = ctrl_def_modg,
  family = "binomial",
  metric = "Sens",
  tuneGrid = expand.grid(alpha = 0,lambda=lambdas),
  preProcess = c("center", "scale"))

logit_lasso_acc

#Ahora, se hará la prueba tomando como métrica el ROC

set.seed(10101)
logit_lasso_roc <- train(
  Pobre_dummy ~Npersug+Lp +factor(OcVivl)+ factor(Dominio)+P5000,
  data = DaTRAIN_H_mini,
  method = "glmnet",
  trControl = ctrl_def_modg,
  family = "binomial",
  metric = "ROC",
  tuneGrid = expand.grid(alpha = 0,lambda=lambdas),
  preProcess = c("center", "scale"))

logit_lasso_roc

#Calcularemos la regla para realizar la clasificación (Cut off)

Eval_Resultados <- data.frame(Pobre = Evaluation_H$Pobre_dummy)
Eval_Resultados$Roc <- predict(logit_lasso_roc,
                           newdata = Evaluation_H,
                           type = "prob")[,1]

install.packages("pROC")
library(pROC)
#Se calcula el ROC para la regresión
rf_ROC <- roc(Eval_Resultados$Pobre, Eval_Resultados$Roc, levels = rev(levels(Eval_Resultados$Pobre)))

rf_ROC

#Se calcula el Cut off
rf_Thresh <- coords(rf_ROC, x = "best", best.method = "closest.topleft")
rf_Thresh

#Se evalúan los resultados
Eval_Resultados<-Eval_Resultados %>% mutate(hat_def_05=ifelse(Eval_Resultados$Roc>0.5,"Si","No"),
                                    hat_def_rf_Thresh=ifelse(Eval_Resultados$Roc>rf_Thresh$threshold,"Si","No"))

#Cuando el threshold es igual a 0.5 (regla de Bayes)
with(Eval_Resultados,table(Pobre,hat_def_05))
#Cuando el threshold es obtenido del ROC
with(Eval_Resultados,table(Pobre,hat_def_rf_Thresh))

#Up-sampling
set.seed(10101)
upSampled_Train_H <- upSample(x = DaTRAIN_H_mini,
                           y = DaTRAIN_H_mini$Pobre_dummy,
                           ## Mantener la variable de clasificación con el mismo nombre:
                           yname = "Pobre_dummy")

dim(upSampled_Train_H)
table(upSampled_Train_H$Pobre_dummy)

set.seed(10101)
logit_lasso_upsample <- train(
  Pobre_dummy ~Npersug+Lp +factor(OcVivl)+ factor(Dominio)+P5000,
  data = upSampled_Train_H,
  method = "glmnet",
  trControl = ctrl_def_modg,
  family = "binomial",
  metric = "ROC",
  tuneGrid = expand.grid(alpha = 0,lambda=lambdas),
  preProcess = c("center", "scale")
)
logit_lasso_upsample

#Down-sampling
set.seed(10101)
downSampled_Train_H <- downSample(x = DaTRAIN_H_mini,
                               y = DaTRAIN_H_mini$Pobre_dummy,
                               ## keep the class variable name the same:
                               yname = "Pobre_dummy")

table(downSampled_Train_H$Pobre_dummy)

set.seed(10101)
logit_lasso_downsample <- train(
  Pobre_dummy ~Npersug+Lp +factor(OcVivl)+ factor(Dominio)+P5000,
  data = downSampled_Train_H,
  method = "glmnet",
  trControl = ctrl_def_modg,
  family = "binomial",
  metric = "ROC",
  tuneGrid = expand.grid(alpha = 0,lambda=lambdas),
  preProcess = c("center", "scale")
)

logit_lasso_downsample

#SMOTE resampling

require("smotefamily")
library(smotefamily)
predictors<-c("Npersug","Lp","OcVivl", "Dominio","P5000") 
            
head(DaTRAIN_H_mini[predictors])

######################################################################################################
salida_smote = SMOTE(X = DaTRAIN_H_mini[predictors], #No está funcionando porque las variables no son numéricas
                     target = DaTRAIN_H_mini$Pobre_dummy)
Oversampled_data = salida_smote$data
table(DaTRAIN_H_mini$Pobre_dummy)
table(Oversampled_data$class)

set.seed(10101)
logit_lasso_smote<- train(
  class ~Npersug+Lp +factor(OcVivl)+ factor(Dominio)+P5000,
  data = Oversampled_data,
  method = "glmnet",
  trControl = ctrl_def_modg,
  family = "binomial",
  metric = "ROC",
  tuneGrid = expand.grid(alpha = 0,lambda=lambdas),
  preProcess = c("center", "scale")
)
#################################################################################################


testResults <- data.frame(Pobre = Testing_H$Pobre_dummy)
testResults$logit<- predict(logit_caret_modg,
                            newdata = Testing_H,
                            type = "prob")[,1]
testResults$lasso<- predict(logit_lasso_roc,
                            newdata = Testing_H,
                            type = "prob")[,1]
testResults$lasso_thresh<- predict(logit_lasso_roc,
                                   newdata = Testing_H,
                                   type = "prob")[,1]
testResults$lasso_upsample<- predict(logit_lasso_upsample,
                                     newdata = Testing_H,
                                     type = "prob")[,1]
testResults$mylogit_lasso_downsample<- predict(logit_lasso_downsample,
                                               newdata = Testing_H,
                                               type = "prob")[,1]
#################################################################################################
testResults$mylogit lasso smote<- predict(mylogit lasso smote,
                                          newdata = testing,
                                          type = "prob")[,1]
##################################################################################################

testResults<-testResults %>%
  mutate(logit=ifelse(logit>0.5,"Si","No"),
         lasso=ifelse(lasso>0.5,"Si","No"),
         lasso_thresh=ifelse(lasso_thresh>rf_Thresh$threshold,"Si","No"),
         lasso_upsample=ifelse(lasso_upsample>0.5,"Si","No"),
         mylogit_lasso_downsample=ifelse(mylogit_lasso_downsample>0.5,"Si","No")#,
         #mylogit lasso smote=ifelse(mylogit lasso smote>0.5,"Si","No"),
  )

with(testResults,table(Pobre,logit))
with(testResults,table(Pobre,lasso))
with(testResults,table(Pobre,lasso_thresh))
with(testResults,table(Pobre,lasso_upsample))
with(testResults,table(Pobre,mylogit_lasso_downsample))


##Termina! por el 07-07-2022



#Modelo1
model1 <- as.formula("Pobre ~ OcVivl")
#Se dividirá la base Training personas para obtener las siguientes bases: otra train, mini test y la evaluation para calcular el ROC
require(caret)
set.seed(10101)
Split_1Mod1<- createDataPartition(DaTRAIN_H$Pobre, p = .7) [[1]]
length(Split_1Mod1)
other_Mod1 <- DaTRAIN_H[-Split_1Mod1,]
DaTRAIN_H_mini_Mod1<- DaTRAIN_H[ Split_1Mod1,] #Base mini train

set.seed(10101)
Split_2Mod1<- createDataPartition(other_Mod1$Pobre, p = 1/3) [[1]]
Evaluation_H_Mod1 <- other_Mod1[ Split_2Mod1,] #Base evaluacion para ROC
Testing_H_Mod1 <- other_Mod1[-Split_2Mod1,] #Base mini test

#Se realiza el K-fold como método de control del modelo
Varios_parametrosMod1<-function(...)c(twoClassSummary(...), defaultSummary(...))

ctrl_def_Mod1 <- trainControl(method = "cv",
                              number = 5,
                              summaryFunction = Varios_parametrosMod1,
                              classProbs = TRUE,
                              verbose=FALSE,
                              savePredictions = T)
#logit
set.seed(10101)
#Se realiza el modelo de clasificacón con la base de control 
logit_caret_Mod1 <- train(
  Pobre_dummy ~factor(OcVivl),
  data =DaTRAIN_H_mini_Mod1 ,
  method = "glm", #Para logit
  trControl = ctrl_def_Mod1,
  family = "binomial",
  preProcess = c("center", "scale"))
logit_caret_Mod1

#Lambdas para Lasso
lambdasMod1<- 10^seq(-4, 0.01, length = 200)

#Ahora, se hará la prueba tomando como métrica la Sensibilidad
set.seed(10101)
logit_lasso_SensMod1 <- train(
  Pobre_dummy ~factor(OcVivl),
  data = DaTRAIN_H_mini_Mod1,
  method = "glmnet",
  trControl = ctrl_def_Mod1,
  family = "binomial",
  metric = "Sens",
  tuneGrid = expand.grid(alpha = 0,lambda=lambdasMod1),
  preProcess = c("center", "scale"))

logit_lasso_SensMod1

#Ahora, se hará la prueba tomando como métrica el ROC

set.seed(10101)
logit_lasso_rocMod1 <- train(
  Pobre_dummy ~factor(OcVivl),
  data = DaTRAIN_H_mini_Mod1,
  method = "glmnet",
  trControl = ctrl_def_Mod1,
  family = "binomial",
  metric = "ROC",
  tuneGrid = expand.grid(alpha = 0,lambda=lambdasMod1),
  preProcess = c("center", "scale"))

logit_lasso_rocMod1

#Calcularemos la regla para realizar la clasificación (Cut off)

Eval_ResultadosMod1 <- data.frame(Pobre = Evaluation_H_Mod1$Pobre_dummy)
Eval_ResultadosMod1$Roc <- predict(logit_lasso_rocMod1,
                               newdata = Evaluation_H_Mod1,
                               type = "prob")[,1]

install.packages("pROC")
library(pROC)
#Se calcula el ROC para la regresión
rf_ROCMod1 <- roc(Eval_ResultadosMod1$Pobre, Eval_ResultadosMod1$Roc, levels = rev(levels(Eval_ResultadosMod1$Pobre)))

rf_ROCMod1

#Se calcula el Cut off
rf_ThreshMod1 <- coords(rf_ROCMod1, x = "best", best.method = "closest.topleft")
rf_ThreshMod1

#Se evalúan los resultados
Eval_ResultadosMod1<-Eval_ResultadosMod1 %>% mutate(hat_def_05Mod1=ifelse(Eval_ResultadosMod1$Roc>0.5,"Si","No"),
                                            hat_def_rf_ThreshMod1=ifelse(Eval_ResultadosMod1$Roc>rf_ThreshMod1$threshold,"Si","No"))


#Cuando el threshold es igual a 0.5 (regla de Bayes)
with(Eval_ResultadosMod1,table(Pobre,hat_def_05Mod1))
#Cuando el threshold es obtenido del ROC
with(Eval_ResultadosMod1,table(Pobre,hat_def_rf_ThreshMod1))

#Up-sampling
set.seed(10101)
upSampled_Train_HMod1<- upSample(x = DaTRAIN_H_mini_Mod1,
                              y = DaTRAIN_H_mini_Mod1$Pobre_dummy,
                              ## Mantener la variable de clasificación con el mismo nombre:
                              yname = "Pobre_dummy")

dim(upSampled_Train_HMod1)
table(upSampled_Train_HMod1$Pobre_dummy)

set.seed(10101)
logit_lasso_upsampleMod1 <- train(
  Pobre_dummy ~factor(OcVivl),
  data = upSampled_Train_HMod1,
  method = "glmnet",
  trControl = ctrl_def_Mod1,
  family = "binomial",
  metric = "ROC",
  tuneGrid = expand.grid(alpha = 0,lambda=lambdasMod1),
  preProcess = c("center", "scale")
)
logit_lasso_upsampleMod1

#Down-sampling
set.seed(10101)
downSampled_Train_HMod1 <- downSample(x = DaTRAIN_H_mini_Mod1,
                                  y = DaTRAIN_H_mini_Mod1$Pobre_dummy,
                                  ## keep the class variable name the same:
                                  yname = "Pobre_dummy")

table(downSampled_Train_HMod1$Pobre_dummy)

set.seed(10101)
logit_lasso_downsampleMod1 <- train(
  Pobre_dummy ~factor(OcVivl),
  data = downSampled_Train_HMod1,
  method = "glmnet",
  trControl = ctrl_def_Mod1,
  family = "binomial",
  metric = "ROC",
  tuneGrid = expand.grid(alpha = 0,lambda=lambdasMod1),
  preProcess = c("center", "scale")
)

logit_lasso_downsampleMod1

#SMOTE resampling
install.packages("smotefamily")
library(smotefamily)
require("smotefamily")
predictorsMod1<-c("OcVivl") 

head(DaTRAIN_H_mini_Mod1[predictorsMod1])

######################################################################################################
salida_smoteMod1 = SMOTE(X = DaTRAIN_H_mini_Mod1[predictorsMod1], #No está funcionando porque las variables no son numéricas
                     target = DaTRAIN_H_mini_Mod1$Pobre_dummy)
Oversampled_dataMod1 = salida_smoteMod1$data
table(DaTRAIN_H_mini_Mod1$Pobre_dummy)
table(Oversampled_dataMod1$class)

set.seed(10101)
logit_lasso_smoteMod1<- train(
  class ~factor(Dominio),
  data = Oversampled_dataMod1,
  method = "glmnet",
  trControl = ctrl_def_Mod1,
  family = "binomial",
  metric = "ROC",
  tuneGrid = expand.grid(alpha = 0,lambda=lambdasMod1),
  preProcess = c("center", "scale")
)
#################################################################################################


testResultsMod1 <- data.frame(Pobre = Testing_H_Mod1$Pobre_dummy)
testResultsMod1$logitm1<- predict(logit_caret_Mod1,
                            newdata = Testing_H_Mod1,
                            type = "prob")[,1]
testResultsMod1$lassom1<- predict(logit_lasso_rocMod1,
                            newdata = Testing_H_Mod1,
                            type = "prob")[,1]
testResultsMod1$lasso_threshm1<- predict(logit_lasso_rocMod1,
                                   newdata = Testing_H_Mod1,
                                   type = "prob")[,1]
testResultsMod1$lasso_upsamplem1<- predict(logit_lasso_upsampleMod1,
                                     newdata = Testing_H_Mod1,
                                     type = "prob")[,1]
testResultsMod1$mylogit_lasso_downsamplem1<- predict(logit_lasso_downsampleMod1,
                                               newdata = Testing_H_Mod1,
                                               type = "prob")[,1]
#################################################################################################
testResults$mylogit lasso smote<- predict(mylogit lasso smote,
                                          newdata = testing,
                                          type = "prob")[,1]
##################################################################################################

testResultsMod1<-testResultsMod1 %>%
  mutate(logitm1=ifelse(logitm1>0.5,"Si","No"),
         lassom1=ifelse(lassom1>0.5,"Si","No"),
         lasso_threshm1=ifelse(lasso_threshm1>rf_Thresh$threshold,"Si","No"),
         lasso_upsamplem1=ifelse(lasso_upsamplem1>0.5,"Si","No"),
         mylogit_lasso_downsamplem1=ifelse(mylogit_lasso_downsamplem1>0.5,"Si","No")#,
         #mylogit lasso smote=ifelse(mylogit lasso smote>0.5,"Si","No"),
  )

with(testResultsMod1,table(Pobre,logitm1))
with(testResultsMod1,table(Pobre,lassom1))
with(testResultsMod1,table(Pobre,lasso_threshm1))
with(testResultsMod1,table(Pobre,lasso_upsamplem1))
with(testResultsMod1,table(Pobre,mylogit_lasso_downsamplem1))
##
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
require(caret)
DaTRAIN_H<-DaTRAIN_H %>% mutate(Pobre_dummy=factor(Pobre,levels=c(1,0), labels=c("Si", "No")))

set.seed(10101)
Split_1Mod2<- createDataPartition(DaTRAIN_H$Pobre, p = .7) [[1]]
length(Split_1Mod2)
other_Mod2 <- DaTRAIN_H[-Split_1Mod2,]
DaTRAIN_H_mini_Mod2<- DaTRAIN_H[ Split_1Mod2,] #Base mini train

set.seed(10101)
Split_2Mod2<- createDataPartition(other_Mod2$Pobre, p = 1/3) [[1]]
Evaluation_H_Mod2 <- other_Mod2[ Split_2Mod2,] #Base evaluacion para ROC
Testing_H_Mod2 <- other_Mod1[-Split_2Mod2,] #Base mini test

#Se realiza el K-fold como método de control del modelo
Varios_parametrosMod2<-function(...)c(twoClassSummary(...), defaultSummary(...))

ctrl_def_Mod2 <- trainControl(method = "cv",
                              number = 5,
                              summaryFunction = Varios_parametrosMod2,
                              classProbs = TRUE,
                              verbose=FALSE,
                              savePredictions = T)
#logit
set.seed(10101)
DaTRAIN_H_mini_Mod2$Pobre_dummy
#Se realiza el modelo de clasificacón con la base de control 
logit_caret_Mod2 <- train(
  Pobre_dummy ~ Lp,
  data =DaTRAIN_H_mini_Mod2 ,
  method = "glm", #Para logit
  trControl = ctrl_def_Mod2,
  family = "binomial",
  preProcess = c("center", "scale"))
logit_caret_Mod2

#Lambdas para Lasso
lambdasMod2<- 10^seq(-4, 0.01, length = 200)

#Ahora, se hará la prueba tomando como métrica la Sensibilidad
view(DaTRAIN_H_mini_Mod2$Lp)
set.seed(10101)

logit_lasso_SensMod2 <- train(
  Pobre_dummy ~ Lp,
  data = DaTRAIN_H_mini_Mod2,
  method = "glmnet",
  trControl = ctrl_def_Mod2,
  family = "binomial",
  metric = "Sens",
  tuneGrid = expand.grid(alpha = 0,lambda=lambdasMod2),
  preProcess = c("center", "scale"))

logit_lasso_SensMod2
#Aqui hay error
#Ahora, se hará la prueba tomando como métrica el ROC

set.seed(10101)
logit_lasso_rocMod2 <- train(
  Pobre_dummy~Lp,
  data = DaTRAIN_H_mini_Mod2,
  method = "glmnet",
  trControl = ctrl_def_Mod2,
  family = "binomial",
  metric = "ROC",
  tuneGrid = expand.grid(alpha = 0,lambda=lambdasMod2),
  preProcess = c("center", "scale"))

logit_lasso_rocMod2

#Calcularemos la regla para realizar la clasificación (Cut off)

Eval_ResultadosMod2 <- data.frame(Pobre = Evaluation_H_Mod2$Pobre_dummy)
Eval_ResultadosMod2$Roc <- predict(logit_lasso_rocMod2,
                                   newdata = Evaluation_H_Mod2,
                                   type = "prob")[,1]

install.packages("pROC")
library(pROC)
#Se calcula el ROC para la regresión
rf_ROCMod2 <- roc(Eval_ResultadosMod2$Pobre, Eval_ResultadosMod2$Roc, levels = rev(levels(Eval_ResultadosMod2$Pobre)))

rf_ROCMod2

#Se calcula el Cut off
rf_ThreshMod2 <- coords(rf_ROCMod2, x = "best", best.method = "closest.topleft")
rf_ThreshMod2

#Se evalúan los resultados
Eval_ResultadosMod2<-Eval_ResultadosMod1 %>% mutate(hat_def_05Mod2=ifelse(Eval_ResultadosMod2$Roc>0.5,"Si","No"),
                                                    hat_def_rf_ThreshMod2=ifelse(Eval_ResultadosMod2$Roc>rf_ThreshMod2$threshold,"Si","No"))


#Cuando el threshold es igual a 0.5 (regla de Bayes)
with(Eval_ResultadosMod2,table(Pobre,hat_def_05Mod2))
#Cuando el threshold es obtenido del ROC
with(Eval_ResultadosMod2,table(Pobre,hat_def_rf_ThreshMod2))

#Up-sampling
set.seed(10101)
upSampled_Train_HMod2<- upSample(x = DaTRAIN_H_mini_Mod2,
                                 y = DaTRAIN_H_mini_Mod2$Pobre_dummy,
                                 ## Mantener la variable de clasificación con el mismo nombre:
                                 yname = "Pobre_dummy")

dim(upSampled_Train_HMod2)
table(upSampled_Train_HMod2$Pobre_dummy)

set.seed(10101)
logit_lasso_upsampleMod2 <- train(
  Pobre_dummy ~Lp,
  data = upSampled_Train_HMod2,
  method = "glmnet",
  trControl = ctrl_def_Mod2,
  family = "binomial",
  metric = "ROC",
  tuneGrid = expand.grid(alpha = 0,lambda=lambdasMod2),
  preProcess = c("center", "scale")
)
logit_lasso_upsampleMod2

#Down-sampling
set.seed(10101)
downSampled_Train_HMod2 <- downSample(x = DaTRAIN_H_mini_Mod2,
                                      y = DaTRAIN_H_mini_Mod2$Pobre_dummy,
                                      ## keep the class variable name the same:
                                      yname = "Pobre_dummy")

table(downSampled_Train_HMod2$Pobre_dummy)

set.seed(10101)
logit_lasso_downsampleMod2 <- train(
  Pobre_dummy ~Lp,
  data = downSampled_Train_HMod2,
  method = "glmnet",
  trControl = ctrl_def_Mod2,
  family = "binomial",
  metric = "ROC",
  tuneGrid = expand.grid(alpha = 0,lambda=lambdasMod1),
  preProcess = c("center", "scale")
)

logit_lasso_downsampleMod2

#SMOTE resampling
install.packages("smotefamily")
library(smotefamily)
require("smotefamily")
predictorsMod2<-c("Lp") 

head(DaTRAIN_H_mini_Mod1[predictorsMod2])

######################################################################################################
salida_smoteMod2 = SMOTE(X = DaTRAIN_H_mini_Mod2[predictorsMod2], #No está funcionando porque las variables no son numéricas
                         target = DaTRAIN_H_mini_Mod2$Pobre_dummy)
Oversampled_dataMod2 = salida_smoteMod2$data
table(DaTRAIN_H_mini_Mod2$Pobre_dummy)
table(Oversampled_dataMod2$class)

set.seed(10101)
logit_lasso_smoteMod2<- train(
  class ~ Lp,
  data = Oversampled_dataMod2,
  method = "glmnet",
  trControl = ctrl_def_Mod2,
  family = "binomial",
  metric = "ROC",
  tuneGrid = expand.grid(alpha = 0,lambda=lambdasMod1),
  preProcess = c("center", "scale")
)
#################################################################################################


testResultsMod2 <- data.frame(Pobre = Testing_H_Mod2$Pobre_dummy)
testResultsMod2$logitm2<- predict(logit_caret_Mod2,
                                  newdata = Testing_H_Mod1,
                                  type = "prob")[,1]
testResultsMod2$lassom2<- predict(logit_lasso_rocMod2,
                                  newdata = Testing_H_Mod2,
                                  type = "prob")[,1]
testResultsMod2$lasso_threshm2<- predict(logit_lasso_rocMod2,
                                         newdata = Testing_H_Mod2,
                                         type = "prob")[,1]
testResultsMod2$lasso_upsamplem2<- predict(logit_lasso_upsampleMod2,
                                           newdata = Testing_H_Mod2,
                                           type = "prob")[,1]
testResultsMod2$mylogit_lasso_downsamplem2<- predict(logit_lasso_downsampleMod2,
                                                     newdata = Testing_H_Mod2,
                                                     type = "prob")[,1]
#################################################################################################
testResults$mylogit lasso smote<- predict(mylogit lasso smote,
                                          newdata = testing,
                                          type = "prob")[,1]
##################################################################################################

testResultsMod2<-testResultsMod2 %>%
  mutate(logitm2=ifelse(logitm2>0.5,"Si","No"),
         lassom2=ifelse(lassom2>0.5,"Si","No"),
         lasso_threshm2=ifelse(lasso_threshm2>rf_Thresh$threshold,"Si","No"),
         lasso_upsamplem2=ifelse(lasso_upsamplem2>0.5,"Si","No"),
         mylogit_lasso_downsamplem2=ifelse(mylogit_lasso_downsamplem2>0.5,"Si","No")#,
         #mylogit lasso smote=ifelse(mylogit lasso smote>0.5,"Si","No"),
  )

with(testResultsMod2,table(Pobre,logitm2))
with(testResultsMod2,table(Pobre,lassom2))
with(testResultsMod2,table(Pobre,lasso_threshm2))
with(testResultsMod2,table(Pobre,lasso_upsamplem2))
with(testResultsMod2,table(Pobre,mylogit_lasso_downsamplem2))
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
cm_log1

#Modelo3

model3 <- as.formula("Pobre ~ Lp + OcVivl")
require(caret)
set.seed(10101)
DaTRAIN_H<-DaTRAIN_H %>% mutate(Pobre_dummy=factor(Pobre,levels=c(1,0), labels=c("Si", "No")))

Split_1Mod3<- createDataPartition(DaTRAIN_H$Pobre, p = .7) [[1]]
length(Split_1Mod3)
other_Mod3 <- DaTRAIN_H[-Split_1Mod3,]
DaTRAIN_H_mini_Mod3<- DaTRAIN_H[ Split_1Mod3,] #Base mini train
view(DaTRAIN_H_mini_Mod3)
set.seed(10101)
Split_2Mod3<- createDataPartition(other_Mod3$Pobre, p = 1/3) [[1]]
Evaluation_H_Mod3 <- other_Mod3[ Split_2Mod3,] #Base evaluacion para ROC
Testing_H_Mod3 <- other_Mod3[-Split_2Mod3,] #Base mini test

#Se realiza el K-fold como método de control del modelo
Varios_parametrosMod3<-function(...)c(twoClassSummary(...), defaultSummary(...))

ctrl_def_Mod3 <- trainControl(method = "cv",
                              number = 5,
                              summaryFunction = Varios_parametrosMod3,
                              classProbs = TRUE,
                              verbose=FALSE,
                              savePredictions = T)
#logit
set.seed(10101)

#Se realiza el modelo de clasificacón con la base de control


logit_caret_Mod3 <- train(
  Pobre_dummy ~Lp + OcVivl,
  data =DaTRAIN_H_mini_Mod3 ,
  method = "glm", #Para logit
  trControl = ctrl_def_Mod3,
  family = "binomial",
  preProcess = c("center", "scale"))
logit_caret_Mod3

#Lambdas para Lasso
lambdasMod3<- 10^seq(-4, 0.01, length = 200)

#Ahora, se hará la prueba tomando como métrica la Sensibilidad
set.seed(10101)
logit_lasso_SensMod3 <- train(
  Pobre_dummy ~Lp + OcVivl,
  data = DaTRAIN_H_mini_Mod3,
  method = "glmnet",
  trControl = ctrl_def_Mod3,
  family = "binomial",
  metric = "Sens",
  tuneGrid = expand.grid(alpha = 0,lambda=lambdasMod3),
  preProcess = c("center", "scale"))

logit_lasso_SensMod3

#Ahora, se hará la prueba tomando como métrica el ROC

set.seed(10101)
logit_lasso_rocMod3 <- train(
  Pobre_dummy ~Lp + OcVivl,
  data = DaTRAIN_H_mini_Mod3,
  method = "glmnet",
  trControl = ctrl_def_Mod3,
  family = "binomial",
  metric = "ROC",
  tuneGrid = expand.grid(alpha = 0,lambda=lambdasMod3),
  preProcess = c("center", "scale"))

logit_lasso_rocMod3

#Calcularemos la regla para realizar la clasificación (Cut off)

Eval_ResultadosMod3 <- data.frame(Pobre = Evaluation_H_Mod3$Pobre_dummy)
Eval_ResultadosMod3$Roc <- predict(logit_lasso_rocMod3,
                                   newdata = Evaluation_H_Mod3,
                                   type = "prob")[,1]

install.packages("pROC")
library(pROC)
#Se calcula el ROC para la regresión
rf_ROCMod3 <- roc(Eval_ResultadosMod3$Pobre, Eval_ResultadosMod3$Roc, levels = rev(levels(Eval_ResultadosMod3$Pobre)))

rf_ROCMod3

#Se calcula el Cut off
rf_ThreshMod3 <- coords(rf_ROCMod3, x = "best", best.method = "closest.topleft")
rf_ThreshMod3

#Se evalúan los resultados
Eval_ResultadosMod3<-Eval_ResultadosMod3 %>% mutate(hat_def_05Mod3=ifelse(Eval_ResultadosMod3$Roc>0.5,"Si","No"),
                                                    hat_def_rf_ThreshMod3=ifelse(Eval_ResultadosMod3$Roc>rf_ThreshMod3$threshold,"Si","No"))


#Cuando el threshold es igual a 0.5 (regla de Bayes)
with(Eval_ResultadosMod3,table(Pobre,hat_def_05Mod3))
#Cuando el threshold es obtenido del ROC
with(Eval_ResultadosMod3,table(Pobre,hat_def_rf_ThreshMod3))

#Up-sampling
set.seed(10101)
upSampled_Train_HMod3<- upSample(x = DaTRAIN_H_mini_Mod3,
                                 y = DaTRAIN_H_mini_Mod3$Pobre_dummy,
                                 ## Mantener la variable de clasificación con el mismo nombre:
                                 yname = "Pobre_dummy")

dim(upSampled_Train_HMod3)
table(upSampled_Train_HMod3$Pobre_dummy)

set.seed(10101)
logit_lasso_upsampleMod3 <- train(
  Pobre_dummy ~ Lp + OcVivl,
  data = upSampled_Train_HMod3,
  method = "glmnet",
  trControl = ctrl_def_Mod3,
  family = "binomial",
  metric = "ROC",
  tuneGrid = expand.grid(alpha = 0,lambda=lambdasMod3),
  preProcess = c("center", "scale")
)
logit_lasso_upsampleMod3

#Down-sampling
set.seed(10101)
downSampled_Train_HMod3 <- downSample(x = DaTRAIN_H_mini_Mod3,
                                      y = DaTRAIN_H_mini_Mod3$Pobre_dummy,
                                      ## keep the class variable name the same:
                                      yname = "Pobre_dummy")

table(downSampled_Train_HMod3$Pobre_dummy)

set.seed(10101)
logit_lasso_downsampleMod3 <- train(
  Pobre_dummy ~Lp + OcVivl,
  data = downSampled_Train_HMod3,
  method = "glmnet",
  trControl = ctrl_def_Mod3,
  family = "binomial",
  metric = "ROC",
  tuneGrid = expand.grid(alpha = 0,lambda=lambdasMod3),
  preProcess = c("center", "scale")
)

logit_lasso_downsampleMod3

#SMOTE resampling
install.packages("smotefamily")
library(smotefamily)
require("smotefamily")
predictorsMod3<-c(" Lp + OcVivl") 

head(DaTRAIN_H_mini_Mod3[predictorsMod3])

######################################################################################################
salida_smoteMod1 = SMOTE(X = DaTRAIN_H_mini_Mod1[predictorsMod1], #No está funcionando porque las variables no son numéricas
                         target = DaTRAIN_H_mini_Mod1$Pobre_dummy)
Oversampled_dataMod1 = salida_smoteMod1$data
table(DaTRAIN_H_mini_Mod1$Pobre_dummy)
table(Oversampled_dataMod1$class)

set.seed(10101)
logit_lasso_smoteMod1<- train(
  class ~factor(Dominio),
  data = Oversampled_dataMod1,
  method = "glmnet",
  trControl = ctrl_def_Mod1,
  family = "binomial",
  metric = "ROC",
  tuneGrid = expand.grid(alpha = 0,lambda=lambdasMod1),
  preProcess = c("center", "scale")
)
#################################################################################################


testResultsMod3 <- data.frame(Pobre = Testing_H_Mod3$Pobre_dummy)
testResultsMod3$logitm3<- predict(logit_caret_Mod3,
                                  newdata = Testing_H_Mod3,
                                  type = "prob")[,1]
testResultsMod3$lassom3<- predict(logit_lasso_rocMod3,
                                  newdata = Testing_H_Mod3,
                                  type = "prob")[,1]
testResultsMod3$lasso_threshm3<- predict(logit_lasso_rocMod3,
                                         newdata = Testing_H_Mod3,
                                         type = "prob")[,1]
testResultsMod3$lasso_upsamplem3<- predict(logit_lasso_upsampleMod3,
                                           newdata = Testing_H_Mod3,
                                           type = "prob")[,1]
testResultsMod3$mylogit_lasso_downsamplem3<- predict(logit_lasso_downsampleMod3,
                                                     newdata = Testing_H_Mod3,
                                                     type = "prob")[,1]
#################################################################################################
testResults$mylogit lasso smote<- predict(mylogit lasso smote,
                                          newdata = testing,
                                          type = "prob")[,1]
##################################################################################################

testResultsMod3<-testResultsMod3 %>%
  mutate(logitm3=ifelse(logitm3>0.5,"Si","No"),
         lassom3=ifelse(lassom3>0.5,"Si","No"),
         lasso_threshm3=ifelse(lasso_threshm3>rf_ThreshMod3$threshold,"Si","No"),
         lasso_upsamplem3=ifelse(lasso_upsamplem3>0.5,"Si","No"),
         mylogit_lasso_downsamplem3=ifelse(mylogit_lasso_downsamplem3>0.5,"Si","No")#,
         #mylogit lasso smote=ifelse(mylogit lasso smote>0.5,"Si","No"),
  )

with(testResultsMod3,table(Pobre,logitm3))
with(testResultsMod3,table(Pobre,lassom3))
with(testResultsMod3,table(Pobre,lasso_threshm3))
with(testResultsMod3,table(Pobre,lasso_upsamplem3))
with(testResultsMod3,table(Pobre,mylogit_lasso_downsamplem3))
Mod_log_3 <- stats::glm(model3,family=binomial(link = "logit"), data= DaTRAIN_H)
tidy(Mod_log_3)
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
model4 <- as.formula("Pobre ~ P5000 + OcVivl + Dominio")
require(caret)
set.seed(10101)
DaTRAIN_H<-DaTRAIN_H %>% mutate(Pobre_dummy=factor(Pobre,levels=c(1,0), labels=c("Si", "No")))

Split_1Mod4<- createDataPartition(DaTRAIN_H$Pobre, p = .7) [[1]]
length(Split_1Mod4)
other_Mod4 <- DaTRAIN_H[-Split_1Mod4,]
DaTRAIN_H_mini_Mod4<- DaTRAIN_H[ Split_1Mod4,] #Base mini train
view(DaTRAIN_H_mini_Mod4)
set.seed(10101)
Split_2Mod4<- createDataPartition(other_Mod4$Pobre, p = 1/3) [[1]]
Evaluation_H_Mod4 <- other_Mod4[ Split_2Mod4,] #Base evaluacion para ROC
Testing_H_Mod4 <- other_Mod4[-Split_2Mod4,] #Base mini test

#Se realiza el K-fold como método de control del modelo
Varios_parametrosMod4<-function(...)c(twoClassSummary(...), defaultSummary(...))

ctrl_def_Mod4 <- trainControl(method = "cv",
                              number = 5,
                              summaryFunction = Varios_parametrosMod4,
                              classProbs = TRUE,
                              verbose=FALSE,
                              savePredictions = T)
#logit
set.seed(10101)

#Se realiza el modelo de clasificacón con la base de control
logit_caret_Mod4 <- train(
  Pobre_dummy ~P5000 + OcVivl + Dominio,
  data =DaTRAIN_H_mini_Mod4 ,
  method = "glm", #Para logit
  trControl = ctrl_def_Mod4,
  family = "binomial",
  preProcess = c("center", "scale"))
logit_caret_Mod4

#Lambdas para Lasso
lambdasMod4<- 10^seq(-4, 0.01, length = 200)

#Ahora, se hará la prueba tomando como métrica la Sensibilidad
set.seed(10101)
logit_lasso_SensMod4 <- train(
  Pobre_dummy ~P5000 + OcVivl + Dominio,
  data = DaTRAIN_H_mini_Mod4,
  method = "glmnet",
  trControl = ctrl_def_Mod4,
  family = "binomial",
  metric = "Sens",
  tuneGrid = expand.grid(alpha = 0,lambda=lambdasMod4),
  preProcess = c("center", "scale"))

logit_lasso_SensMod4

#Ahora, se hará la prueba tomando como métrica el ROC

set.seed(10101)
logit_lasso_rocMod4 <- train(
  Pobre_dummy ~P5000 + OcVivl + Dominio,
  data = DaTRAIN_H_mini_Mod4,
  method = "glmnet",
  trControl = ctrl_def_Mod4,
  family = "binomial",
  metric = "ROC",
  tuneGrid = expand.grid(alpha = 0,lambda=lambdasMod4),
  preProcess = c("center", "scale"))

logit_lasso_rocMod4

#Calcularemos la regla para realizar la clasificación (Cut off)

Eval_ResultadosMod4 <- data.frame(Pobre = Evaluation_H_Mod4$Pobre_dummy)
Eval_ResultadosMod4$Roc <- predict(logit_lasso_rocMod4,
                                   newdata = Evaluation_H_Mod4,
                                   type = "prob")[,1]

install.packages("pROC")
library(pROC)
#Se calcula el ROC para la regresión
rf_ROCMod4 <- roc(Eval_ResultadosMod4$Pobre, Eval_ResultadosMod4$Roc, levels = rev(levels(Eval_ResultadosMod4$Pobre)))

rf_ROCMod4

#Se calcula el Cut off
rf_ThreshMod4 <- coords(rf_ROCMod4, x = "best", best.method = "closest.topleft")
rf_ThreshMod4

#Se evalúan los resultados
Eval_ResultadosMod4<-Eval_ResultadosMod4 %>% mutate(hat_def_05Mod4=ifelse(Eval_ResultadosMod4$Roc>0.5,"Si","No"),
                                                    hat_def_rf_ThreshMod4=ifelse(Eval_ResultadosMod4$Roc>rf_ThreshMod4$threshold,"Si","No"))


#Cuando el threshold es igual a 0.5 (regla de Bayes)
with(Eval_ResultadosMod4,table(Pobre,hat_def_05Mod4))
#Cuando el threshold es obtenido del ROC
with(Eval_ResultadosMod4,table(Pobre,hat_def_rf_ThreshMod4))

#Up-sampling
set.seed(10101)
upSampled_Train_HMod4<- upSample(x = DaTRAIN_H_mini_Mod4,
                                 y = DaTRAIN_H_mini_Mod4$Pobre_dummy,
                                 ## Mantener la variable de clasificación con el mismo nombre:
                                 yname = "Pobre_dummy")

dim(upSampled_Train_HMod4)
table(upSampled_Train_HMod4$Pobre_dummy)

set.seed(10101)
logit_lasso_upsampleMod4 <- train(
  Pobre_dummy ~ P5000 + OcVivl + Dominio,
  data = upSampled_Train_HMod4,
  method = "glmnet",
  trControl = ctrl_def_Mod4,
  family = "binomial",
  metric = "ROC",
  tuneGrid = expand.grid(alpha = 0,lambda=lambdasMod4),
  preProcess = c("center", "scale")
)
logit_lasso_upsampleMod4

#Down-sampling
set.seed(10101)
downSampled_Train_HMod4 <- downSample(x = DaTRAIN_H_mini_Mod4,
                                      y = DaTRAIN_H_mini_Mod4$Pobre_dummy,
                                      ## keep the class variable name the same:
                                      yname = "Pobre_dummy")

table(downSampled_Train_HMod4$Pobre_dummy)

set.seed(10101)
logit_lasso_downsampleMod4 <- train(
  Pobre_dummy ~P5000 + OcVivl + Dominio,
  data = downSampled_Train_HMod4,
  method = "glmnet",
  trControl = ctrl_def_Mod4,
  family = "binomial",
  metric = "ROC",
  tuneGrid = expand.grid(alpha = 0,lambda=lambdasMod4),
  preProcess = c("center", "scale")
)

logit_lasso_downsampleMod4

#SMOTE resampling
install.packages("smotefamily")
library(smotefamily)
require("smotefamily")
predictorsMod4<-c(" P5000 + OcVivl + Dominio") 

head(DaTRAIN_H_mini_Mod4[predictorsMod4])

######################################################################################################
salida_smoteMod1 = SMOTE(X = DaTRAIN_H_mini_Mod1[predictorsMod1], #No está funcionando porque las variables no son numéricas
                         target = DaTRAIN_H_mini_Mod1$Pobre_dummy)
Oversampled_dataMod1 = salida_smoteMod1$data
table(DaTRAIN_H_mini_Mod4$Pobre_dummy)
table(Oversampled_dataMod1$class)

set.seed(10101)
logit_lasso_smoteMod1<- train(
  class ~factor(Dominio),
  data = Oversampled_dataMod1,
  method = "glmnet",
  trControl = ctrl_def_Mod1,
  family = "binomial",
  metric = "ROC",
  tuneGrid = expand.grid(alpha = 0,lambda=lambdasMod4),
  preProcess = c("center", "scale")
)
#################################################################################################


testResultsMod4 <- data.frame(Pobre = Testing_H_Mod4$Pobre_dummy)
testResultsMod4$logitm4<- predict(logit_caret_Mod4,
                                  newdata = Testing_H_Mod4,
                                  type = "prob")[,1]
testResultsMod4$lassom4<- predict(logit_lasso_rocMod4,
                                  newdata = Testing_H_Mod4,
                                  type = "prob")[,1]
testResultsMod4$lasso_threshm4<- predict(logit_lasso_rocMod4,
                                         newdata = Testing_H_Mod4,
                                         type = "prob")[,1]
testResultsMod4$lasso_upsamplem4<- predict(logit_lasso_upsampleMod4,
                                           newdata = Testing_H_Mod4,
                                           type = "prob")[,1]
testResultsMod4$mylogit_lasso_downsamplem4<- predict(logit_lasso_downsampleMod4,
                                                     newdata = Testing_H_Mod4,
                                                     type = "prob")[,1]
#################################################################################################
testResults$mylogit lasso smote<- predict(mylogit lasso smote,
                                          newdata = testing,
                                          type = "prob")[,1]
##################################################################################################

testResultsMod4<-testResultsMod4 %>%
  mutate(logitm4=ifelse(logitm4>0.5,"Si","No"),
         lassom4=ifelse(lassom4>0.5,"Si","No"),
         lasso_threshm4=ifelse(lasso_threshm4>rf_ThreshMod4$threshold,"Si","No"),
         lasso_upsamplem4=ifelse(lasso_upsamplem4>0.5,"Si","No"),
         mylogit_lasso_downsamplem4=ifelse(mylogit_lasso_downsamplem4>0.5,"Si","No")#,
         #mylogit lasso smote=ifelse(mylogit lasso smote>0.5,"Si","No"),
  )

with(testResultsMod4,table(Pobre,logitm4))
with(testResultsMod4,table(Pobre,lassom4))
with(testResultsMod4,table(Pobre,lasso_threshm4))
with(testResultsMod4,table(Pobre,lasso_upsamplem4))
with(testResultsMod4,table(Pobre,mylogit_lasso_downsamplem4))

Mod_log_4 <- stats::glm(model4,family=binomial(link = "logit"), data= DaTRAIN_H)
tidy(Mod_log_2)
#Predicción
###Prediccion
model4
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
require(caret)
set.seed(10101)
DaTRAIN_H<-DaTRAIN_H %>% mutate(Pobre_dummy=factor(Pobre,levels=c(1,0), labels=c("Si", "No")))

Split_1Mod5<- createDataPartition(DaTRAIN_H$Pobre, p = .7) [[1]]
length(Split_1Mod5)
other_Mod5 <- DaTRAIN_H[-Split_1Mod5,]
DaTRAIN_H_mini_Mod5<- DaTRAIN_H[ Split_1Mod5,] #Base mini train
view(DaTRAIN_H_mini_Mod5)
set.seed(10101)
Split_2Mod5<- createDataPartition(other_Mod5$Pobre, p = 1/3) [[1]]
Evaluation_H_Mod5 <- other_Mod5[ Split_2Mod5,] #Base evaluacion para ROC
Testing_H_Mod5 <- other_Mod5[-Split_2Mod5,] #Base mini test

#Se realiza el K-fold como método de control del modelo
Varios_parametrosMod5<-function(...)c(twoClassSummary(...), defaultSummary(...))

ctrl_def_Mod5 <- trainControl(method = "cv",
                              number = 5,
                              summaryFunction = Varios_parametrosMod5,
                              classProbs = TRUE,
                              verbose=FALSE,
                              savePredictions = T)
#logit
set.seed(10101)

#Se realiza el modelo de clasificacón con la base de control
logit_caret_Mod5 <- train(
  Pobre_dummy ~P5000 + OcVivl,
  data =DaTRAIN_H_mini_Mod5 ,
  method = "glm", #Para logit
  trControl = ctrl_def_Mod5,
  family = "binomial",
  preProcess = c("center", "scale"))
logit_caret_Mod5

#Lambdas para Lasso
lambdasMod5<- 10^seq(-4, 0.01, length = 200)

#Ahora, se hará la prueba tomando como métrica la Sensibilidad
set.seed(10101)
logit_lasso_SensMod5 <- train(
  Pobre_dummy ~P5000 + OcVivl,
  data = DaTRAIN_H_mini_Mod5,
  method = "glmnet",
  trControl = ctrl_def_Mod5,
  family = "binomial",
  metric = "Sens",
  tuneGrid = expand.grid(alpha = 0,lambda=lambdasMod5),
  preProcess = c("center", "scale"))

logit_lasso_SensMod5

#Ahora, se hará la prueba tomando como métrica el ROC

set.seed(10101)
logit_lasso_rocMod5 <- train(
  Pobre_dummy ~P5000 + OcVivl,
  data = DaTRAIN_H_mini_Mod5,
  method = "glmnet",
  trControl = ctrl_def_Mod5,
  family = "binomial",
  metric = "ROC",
  tuneGrid = expand.grid(alpha = 0,lambda=lambdasMod5),
  preProcess = c("center", "scale"))

logit_lasso_rocMod5

#Calcularemos la regla para realizar la clasificación (Cut off)

Eval_ResultadosMod5 <- data.frame(Pobre = Evaluation_H_Mod5$Pobre_dummy)
Eval_ResultadosMod5$Roc <- predict(logit_lasso_rocMod5,
                                   newdata = Evaluation_H_Mod5,
                                   type = "prob")[,1]

install.packages("pROC")
library(pROC)
#Se calcula el ROC para la regresión
rf_ROCMod5 <- roc(Eval_ResultadosMod5$Pobre, Eval_ResultadosMod5$Roc, levels = rev(levels(Eval_ResultadosMod5$Pobre)))

rf_ROCMod5

#Se calcula el Cut off
rf_ThreshMod5 <- coords(rf_ROCMod4, x = "best", best.method = "closest.topleft")
rf_ThreshMod5

#Se evalúan los resultados
Eval_ResultadosMod5<-Eval_ResultadosMod5 %>% mutate(hat_def_05Mod5=ifelse(Eval_ResultadosMod5$Roc>0.5,"Si","No"),
                                                    hat_def_rf_ThreshMod5=ifelse(Eval_ResultadosMod5$Roc>rf_ThreshMod5$threshold,"Si","No"))


#Cuando el threshold es igual a 0.5 (regla de Bayes)
with(Eval_ResultadosMod5,table(Pobre,hat_def_05Mod5))
#Cuando el threshold es obtenido del ROC
with(Eval_ResultadosMod5,table(Pobre,hat_def_rf_ThreshMod5))

#Up-sampling
set.seed(10101)
upSampled_Train_HMod5<- upSample(x = DaTRAIN_H_mini_Mod5,
                                 y = DaTRAIN_H_mini_Mod5$Pobre_dummy,
                                 ## Mantener la variable de clasificación con el mismo nombre:
                                 yname = "Pobre_dummy")

dim(upSampled_Train_HMod5)
table(upSampled_Train_HMod5$Pobre_dummy)

set.seed(10101)
logit_lasso_upsampleMod5<- train(
  Pobre_dummy ~ P5000 + OcVivl,
  data = upSampled_Train_HMod5,
  method = "glmnet",
  trControl = ctrl_def_Mod5,
  family = "binomial",
  metric = "ROC",
  tuneGrid = expand.grid(alpha = 0,lambda=lambdasMod5),
  preProcess = c("center", "scale")
)
logit_lasso_upsampleMod5

#Down-sampling
set.seed(10101)
downSampled_Train_HMod5 <- downSample(x = DaTRAIN_H_mini_Mod5,
                                      y = DaTRAIN_H_mini_Mod5$Pobre_dummy,
                                      ## keep the class variable name the same:
                                      yname = "Pobre_dummy")

table(downSampled_Train_HMod5$Pobre_dummy)

set.seed(10101)
logit_lasso_downsampleMod5 <- train(
  Pobre_dummy ~P5000 + OcVivl,
  data = downSampled_Train_HMod5,
  method = "glmnet",
  trControl = ctrl_def_Mod5,
  family = "binomial",
  metric = "ROC",
  tuneGrid = expand.grid(alpha = 0,lambda=lambdasMod5),
  preProcess = c("center", "scale")
)

logit_lasso_downsampleMod5

#SMOTE resampling
install.packages("smotefamily")
library(smotefamily)
require("smotefamily")
predictorsMod5<-c(" P5000 + OcVivl ") 
model5
head(DaTRAIN_H_mini_Mod5[predictorsMod5])

######################################################################################################
salida_smoteMod1 = SMOTE(X = DaTRAIN_H_mini_Mod1[predictorsMod1], #No está funcionando porque las variables no son numéricas
                         target = DaTRAIN_H_mini_Mod1$Pobre_dummy)
Oversampled_dataMod1 = salida_smoteMod1$data
table(DaTRAIN_H_mini_Mod1$Pobre_dummy)
table(Oversampled_dataMod1$class)

set.seed(10101)
logit_lasso_smoteMod1<- train(
  class ~factor(Dominio),
  data = Oversampled_dataMod1,
  method = "glmnet",
  trControl = ctrl_def_Mod1,
  family = "binomial",
  metric = "ROC",
  tuneGrid = expand.grid(alpha = 0,lambda=lambdasMod1),
  preProcess = c("center", "scale")
)
#################################################################################################


testResultsMod5<- data.frame(Pobre = Testing_H_Mod5$Pobre_dummy)
testResultsMod5$logitm5<- predict(logit_caret_Mod5,
                                  newdata = Testing_H_Mod5,
                                  type = "prob")[,1]
testResultsMod5$lassom5<- predict(logit_lasso_rocMod5,
                                  newdata = Testing_H_Mod5,
                                  type = "prob")[,1]
testResultsMod5$lasso_threshm5<- predict(logit_lasso_rocMod5,
                                         newdata = Testing_H_Mod5,
                                         type = "prob")[,1]
testResultsMod5$lasso_upsamplem5<- predict(logit_lasso_upsampleMod5,
                                           newdata = Testing_H_Mod5,
                                           type = "prob")[,1]
testResultsMod5$mylogit_lasso_downsamplem5<- predict(logit_lasso_downsampleMod5,
                                                     newdata = Testing_H_Mod5,
                                                     type = "prob")[,1]
#################################################################################################
testResults$mylogit lasso smote<- predict(mylogit lasso smote,
                                          newdata = testing,
                                          type = "prob")[,1]
##################################################################################################

testResultsMod5<-testResultsMod5 %>%
  mutate(logitm5=ifelse(logitm5>0.5,"Si","No"),
         lassom5=ifelse(lassom5>0.5,"Si","No"),
         lasso_threshm5=ifelse(lasso_threshm5>rf_ThreshMod4$threshold,"Si","No"),
         lasso_upsamplem5=ifelse(lasso_upsamplem5>0.5,"Si","No"),
         mylogit_lasso_downsamplem5=ifelse(mylogit_lasso_downsamplem5>0.5,"Si","No")#,
         #mylogit lasso smote=ifelse(mylogit lasso smote>0.5,"Si","No"),
  )

with(testResultsMod5,table(Pobre,logitm5))
with(testResultsMod5,table(Pobre,lassom5))
with(testResultsMod5,table(Pobre,lasso_threshm5))
with(testResultsMod5,table(Pobre,lasso_upsamplem5))
with(testResultsMod5,table(Pobre,mylogit_lasso_downsamplem5))
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



##Modelo de regresión 


#Se realizará la división del data set de "training" en 2, una base de mini training y otra para mini test, con el fin de poder calcular
#los MSE en términos de una variable predicha.
DTRAIN_HR <- data.frame()
DTRAIN_HR<- subset(DTRAIN_H, select = c("id", "Ingtotugarr","Ingtotug", "Ingpcug", "Npersug", "Lp", "Dominio", "Pobre", "P5090", "P5000"))
DTEST_HR<- data.frame()
DTEST_HR<- subset(DTEST_H, select = c("id", "Npersug", "Lp", "Dominio", "P5090", "P5000"))
DTEST_PR <- data.frame()
DTEST_PR<- subset(DTEST_P, select = c("id", "P6210", "P6020", "P6040", "Dominio", "Oficio"))
DTRAIN_PR<- data.frame()
DTRAIN_PR<- subset(DTRAIN_P, select = c("id", "P6210", "P6020", "P6040", "Dominio", "Oficio", "Ingtot"))

cantidad_naR <- sapply(DTRAIN_HR, function(x) sum(is.na(x)))
cantidad_naR <- data.frame(cantidad_naR)
porcentaje_naR <- cantidad_naR/nrow(DTRAIN_HR)
porcentaje_naR <-porcentaje_naR*100
porcentaje_naR 
cantidad_naPR <- sapply(DTRAIN_PR, function(x) sum(is.na(x)))
cantidad_naPR <- data.frame(cantidad_naPR)
porcentaje_naPR <- cantidad_naPR/nrow(DTRAIN_PR)
porcentaje_naPR <-porcentaje_naPR*100
porcentaje_naPR 

DTRAIN_PR$Oficio[is.na(DTRAIN_PR$Oficio)] = 0 
DTRAIN_PR$P6210[is.na(DTRAIN_PR$P6210)] = 9 
DTRAIN_PR$Ingtot[is.na(DTRAIN_PR$Ingtot)] = 0 

cantidad_naPRT <- sapply(DTEST_PR, function(x) sum(is.na(x)))
cantidad_naPRT <- data.frame(cantidad_naPRT)
porcentaje_naPRT <- cantidad_naPRT/nrow(DTEST_PR)
porcentaje_naPRT <-porcentaje_naPRT*100
porcentaje_naPRT 

DTEST_PR$Oficio[is.na(DTEST_PR$Oficio)] = 0 
DTEST_PR$P6210[is.na(DTEST_PR$P6210)] = 9 

#Descripción
Edad <- DTRAIN_PR$P6040
Sexo <- DTRAIN_PR$P6020
Educ <- DTRAIN_PR$P6210
Edad2 <- (DTRAIN_PR$Edad)^2 
View(DTRAIN_PR)
DTRAIN_PR <- cbind(DTRAIN_PR, Edad)
DTRAIN_PR <- cbind(DTRAIN_PR, Edad2)
DTRAIN_PR <- cbind(DTRAIN_PR, Sexo, Educ)

View(DTRAIN_PR)
DTRAIN_PR <- DTRAIN_PR %>% #Se vuelven categóricas las variables que así lo sean en la BD
  mutate_at(.vars = c(
    "Sexo", "Educ", "Dominio", "Oficio"),
    .funs = factor)
SexoT <- DTEST_PR$P6020
EdadT<- DTEST_PR$P6040
EducT <- DTEST_PR$P6210
Edad2T <- (DTEST_PR$EdadT)^2


DTEST_PR <- cbind(DTEST_PR, EdadT, SexoT, EducT, Edad2T)
View(DTEST_PR)
DTEST_PR <- DTEST_PR %>% #Se vuelven categóricas las variables que así lo sean en la BD
  mutate_at(.vars = c(
    "SexoT", "EducT", "Dominio", "Oficio"),
    .funs = factor)

set.seed(10101)
id_train_subset_H <- sample(1:nrow(DTRAIN_HR), size = 0.7*nrow(DTRAIN_HR), replace = FALSE)
id_train_subset_P <- sample(1:nrow(DTRAIN_PR), size = 0.7*nrow(DTRAIN_PR), replace = FALSE)

datos_subtrain_H <- DTRAIN_HR[id_train_subset_H, ]
datos_subtest_H  <- DTRAIN_HR[-id_train_subset_H, ]

datos_subtrain_P <- DTRAIN_PR[id_train_subset_P, ]
datos_subtest_P  <- DTRAIN_PR[-id_train_subset_P, ]

#Se hará regresión con respecto al Ingtot por cada persona de la unidad de gasto
model_base_1 <- lm(Ingtot ~ Edad + Edad2 + Sexo + factor(Educ) + factor (Oficio) + factor (Dominio), data = datos_subtrain_P)
summary(model_base_1)

#Se visualizan los coeficientes de OLS para el modelo 1
stargazer(model_base_1,type = "text")
modelb1_coeficientes <- model_base_1$coefficients %>%
  enframe(name = "predictor", value = "coeficiente")

modelb1_coeficientes %>%
  filter(predictor != "(Intercept)") %>%
  ggplot(aes(x = predictor, y = coeficiente)) +
  geom_col() +
  labs(title = "Coeficientes del modelo OLS para modelo 1") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 5, angle = 45))

# Predicciones de entrenamiento para modelo 1
# ==============================================================================
predicciones_train_mb1 <- predict(model_base_1, newdata = datos_subtrain_P)

# MSE de entrenamiento para modelo 1
# ==============================================================================
training_mse_mb1 <- mean((predicciones_train_mb1 - datos_subtrain_P$Ingtot)^2)
paste("Error (mse) de entrenamiento modelo 1:", training_mse_mb1)


# Predicciones de test modelo 1
# ==============================================================================
predicciones_test_mb1 <- predict(model_base_1, newdata = datos_subtest_P)

# MSE de test modelo 1
# ==============================================================================
test_mse_ols_mb1 <- mean((predicciones_test_mb1 - datos_subtest_P$Ingtot)^2)
paste("Error (mse) de test modelo 1:", test_mse_ols_mb1)
## Se realizará la regularización Ridge para ambos modelos

#Para el modelo 1
x_subtrain_P <- model.matrix(Ingtot~ Edad + Edad2 + Sexo + factor(Educ) + factor (Oficio) + factor (Dominio), data = datos_subtrain_P)[, -1]
y_subtrain_P <- datos_subtrain_P$Ingtot

x_subtest_P <- model.matrix(Ingtot~ Edad + Edad2 + Sexo + factor(Educ) + factor (Oficio) + factor (Dominio), data = datos_subtrain_P)[, -1]
y_subtest_P <- datos_subtrain_P$Ingtot



#Se obtiene ajuste con regularización Ridge para el modelo 1
modelobase1Ri <- glmnet(
  x           = x_subtrain_P,
  y           = y_subtrain_P,
  alpha       = 0,
  nlambda     = 200,
  standardize = TRUE
)

regularizacion_mb1 <- modelobase1Ri$beta %>% 
  as.matrix() %>%
  t() %>% 
  as_tibble() %>%
  mutate(lambda = modelobase1Ri$lambda)

regularizacion_mb1 <- regularizacion_mb1 %>%
  pivot_longer(
    cols = !lambda, 
    names_to = "predictor",
    values_to = "coeficientes"
  )

regularizacion_mb1 %>%
  ggplot(aes(x = lambda, y = coeficientes, color = predictor)) +
  geom_line() +
  scale_x_log10(
    breaks = trans_breaks("log10", function(x) 10^x),
    labels = trans_format("log10", math_format(10^.x))
  ) +
  labs(title = "Coeficientes del modelo 1 en función de la regularización") +
  theme_bw() +
  theme(legend.position = "none")

set.seed(10101)
cv_errormod1Ri <- cv.glmnet(
  x           = x_subtrain_P,
  y           = y_subtrain_P,
  alpha  = 0,
  nfolds = 10,
  type.measure = "mse",
  standardize  = TRUE
)

plot(cv_errormod1Ri)
paste("Mejor valor de lambda encontrado para modelo 1:", cv_errormod1Ri$lambda.min)
# Mayor valor de lambda con el que el test-error no se aleja más de 1sd del mínimo.
paste("Mejor valor de lambda encontrado + 1 desviación estándar para modelo 1:", cv_errormod1Ri$lambda.1se)

modelo_1_Ridge <- glmnet(
  x           = x_subtrain_P,
  y           = y_subtrain_P,
  alpha       = 0,
  lambda      = cv_errormod1Ri$lambda.1se,
  standardize = TRUE
)

# Coeficientes del modelo
# ==============================================================================
df_coeficientes_mod1_Ri <- coef(modelo_1_Ridge) %>%
  as.matrix() %>%
  as_tibble(rownames = "predictor") %>%
  rename(coeficiente = s0)

df_coeficientes_mod1_Ri %>%
  filter(predictor != "(Intercept)") %>%
  ggplot(aes(x = predictor, y = coeficiente)) +
  geom_col() +
  labs(title = "Coeficientes del modelo Ridge") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 6, angle = 45))

#Predicciones de entrenamiento
# ==============================================================================
predicciones_train_mb1ri <- predict(modelo_1_Ridge, newx = x_subtrain_P)

# MSE de entrenamiento
# ==============================================================================
training_mse_mb1ri <- mean((predicciones_train_mb1ri - y_subtrain_P)^2)
paste("Error (mse) de entrenamiento modelo 1:", training_mse_mb1ri)

#Predicción
predicciones_test_mb1ri <- predict(modelo_1_Ridge, newx = x_subtest_P)

# MSE de test
# ==============================================================================
test_mse_mod1ridge <- mean((predicciones_test_mb1ri - y_subtest_P)^2)
paste("Error (mse) de test:", test_mse_mod1ridge)

#Para el modelo 1 Lasso

modelobase1lass <- glmnet(
  x           = x_subtrain_P,
  y           = y_subtrain_P,
  alpha       = 1,
  nlambda     = 200,
  standardize = TRUE
)

regularizacionmb1lass <- modelobase1lass$beta %>% 
  as.matrix() %>%
  t() %>% 
  as_tibble() %>%
  mutate(lambda1 = modelobase1lass$lambda)

regularizacionmb1lass <- regularizacionmb1lass %>%
  pivot_longer(
    cols = !lambda1, 
    names_to = "predictor",
    values_to = "coeficientes"
  )

regularizacionmb1lass %>%
  ggplot(aes(x = lambda1, y = coeficientes, color = predictor)) +
  geom_line() +
  scale_x_log10(
    breaks = trans_breaks("log10", function(x) 10^x),
    labels = trans_format("log10", math_format(10^.x))
  ) +
  labs(title = "Coeficientes del modelo 1 en función de la regularización") +
  theme_bw() +
  theme(legend.position = "none")

# Evolución del error en función de lambda

set.seed(10101)
cv_error_m1_Lass <- cv.glmnet(
  x      = x_subtrain_P,
  y      = y_subtrain_P,
  alpha  = 1,
  nfolds = 10,
  type.measure = "mse",
  standardize  = TRUE
)

plot(cv_error_m1_Lass)

paste("Mejor valor de lambda encontrado para modelo 1:", cv_error_m1_Lass$lambda.min)
paste("Mejor valor de lambda encontrado + 1 desviación estándar para modelo 1:", cv_error_m1_Lass$lambda.1se)

# Mejor modelo lambda óptimo + 1sd
# ==============================================================================
modelo1_Lasso <- glmnet(
  x           = x_subtrain_P,
  y           = y_subtrain_P,
  alpha       = 1,
  lambda      = cv_error_m1_Lass$lambda.1se,
  standardize = TRUE
)

# Coeficientes del modelo
# ==============================================================================
df_coeficientes_mod1lass <- coef(modelo1_Lasso) %>%
  as.matrix() %>%
  as_tibble(rownames = "predictor") %>%
  rename(coeficiente = s0)

df_coeficientes_mod1lass %>%
  filter(predictor != "(Intercept)") %>%
  ggplot(aes(x = predictor, y = coeficiente)) +
  geom_col() +
  labs(title = "Coeficientes del modelo 1 Lasso") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 6, angle = 45))

df_coeficientes_mod1lass %>%
  filter(
    predictor != "(Intercept)",
    coeficiente != 0
  ) 


# Predicciones de entrenamiento modelo 1

predicciones_train_mod1_Lass <- predict(modelo1_Lasso, newx = x_subtrain_P)

# MSE de entrenamiento modelo 1

training_mse_mod1_Lass <- mean((predicciones_train_mod1_Lass - y_subtrain_P)^2)
print(paste("Error (mse) de entrenamiento:", training_mse_mod1_Lass ))

predicciones_test_mod1_Lass <- predict(modelo1_Lasso, newx = x_subtest_P)

# MSE de test modelo 1

test_mse_mod1_lasso <- mean((predicciones_test_mod1_Lass - y_subtest_P)^2)
print(paste("Error (mse) de test:", test_mse_mod1_lasso))


# ==============================================================================

