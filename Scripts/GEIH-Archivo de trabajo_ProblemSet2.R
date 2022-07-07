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


stargazer(regmodRi,type = "text")
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



##Modelo de regresión 

xmodRi<- model.matrix(~Npersug+Lp + factor(Dominio)+P5000+factor(OcVivl), DaTRAIN_H)
ymodRi <- DaTRAIN_H$Pobre
Ri.mod1 <- glmnet(xmodRi, ymodRi, alpha = 1 , lambda = 0.03)
modeloRi <- glmnet(
  x           = xmodRi,
  y           = ymodRi,
  alpha       = 0,
  nlambda     = 100,
  standardize = TRUE
)
regmodRi <- modeloRi$beta %>% 
  as.matrix() %>%
  t() %>% 
  as_tibble() %>%
  mutate(lambda = modeloRi$lambda)
regmodRi <- regmodRi %>%
  pivot_longer(
    cols = !lambda, 
    names_to = "predictor",
    values_to = "coeficientes"
  )

regmodRi %>%
  ggplot(aes(x = lambda, y = coeficientes, color = predictor)) +
  geom_line() +
  scale_x_log10(
    breaks = trans_breaks("log10", function(x) 10^x),
    labels = trans_format("log10", math_format(10^.x))
  ) +
  labs(title = "Coeficientes del modelo en función de la regularización") +
  theme_bw() +
  theme(legend.position = "none")

set.seed(10101)
cv_errormodRi <- cv.glmnet(
  x           = xmodRi,
  y           = ymodRi,
  alpha  = 0,
  nfolds = 10,
  type.measure = "mse",
  standardize  = TRUE
)

plot(cv_errormodRi)
paste("Mejor valor de lambda encontrado:", cv_errormodRi$lambda.min)
# Mayor valor de lambda con el que el test-error no se aleja más de 1sd del mínimo.
paste("Mejor valor de lambda encontrado + 1 desviación estándar:", cv_errormodRi$lambda.1se)
##






