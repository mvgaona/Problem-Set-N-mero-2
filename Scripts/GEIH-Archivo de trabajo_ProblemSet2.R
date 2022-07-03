# Valeria Gaona - 202214418
# Andrea Beleño - 200620739

#### PROBLEM SET 2 #####

#Con este script se realizará el scraping de las bases de datos que serán usada 
#para el desarrollo del Problem Set, específicamente puntos 1 y 2.

install.packages("pacman") #Instalar librería si no cuenta con esta 
library(pacman) #Llamar librería
p_load(rio, #Instalar librerías que falten
       tidyverse,
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

Datos_test_hogares<-read.csv("../Elementos_Guardados/test_hogares.csv") #Guardar las bases de datos
Datos_test_personas<-read.csv("../Elementos_Guardados/test_personas.csv") #Guardar las bases de datos

Datos_training_hogares<-read.csv("../Elementos_Guardados/train_hogares.csv") #Guardar las bases de datos
Datos_training_personas<-read.csv(unzip("../Elementos_Guardados/train_personas.zip","train_personas.csv"))#Se extrae del .zip, teniendo en cuenta que es un archivo muy grande para subir a Github
file.remove('train_personas.csv')#"Por si se requiere borrar algún archivo


#####BASE DE DATOS GEIH Punto 1.1.1

#Se importará la base de datos cada base de datos y se volverá data.frame para poder tenerlo en matriz.
Base1 <- read_html("https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_1.html")%>%
  html_table()
Base1 <- data.frame(Base1)
Base2 <- read_html("https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_2.html")%>% 
  html_table()
Base2 <- data.frame(Base2)
Base3 <- read_html("https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_3.html")%>% 
  html_table()
Base3 <- data.frame(Base3)
Base4 <- read_html("https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_4.html")%>% 
  html_table()
Base4 <- data.frame(Base4)
Base5 <- read_html("https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_5.html")%>% 
  html_table()
Base5 <- data.frame(Base5)
Base6 <- read_html("https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_6.html")%>% 
  html_table()
Base6 <- data.frame(Base6)
Base7 <- read_html("https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_7.html")%>% 
  html_table()
Base7 <- data.frame(Base7)
Base8 <- read_html("https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_8.html")%>% 
  html_table()
Base8 <- data.frame(Base8)
Base9 <- read_html("https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_9.html")%>% 
  html_table()
Base9 <- data.frame(Base9)
Base10 <- read_html("https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_10.html")%>% 
  html_table()
Base10<- data.frame(Base10)
#Al observar que cada una de las bases de datos si pudo ser importada, se procede a unir cada base de datos
#Con la fusión de todas las bases de datos, tendremos oficialmente los Datos completos de la GEIH de 2018
DatosGEIH<- rbind(Base1, Base2, Base3, Base4, Base5, Base6, Base7, Base8, Base9, Base10)

DatosGEIH<-readRDS("Datos_GEIH.Rds") #Para cargar la base, en caso que no se tenga tiempo para cargar desde la página web-Solo se debe poner la base de datos en la ubicación de la sesión y carga esta tabla
#saveRDS(DatosGEIH, file = "Datos_GEIH.rds") #Crea el archivo RDS en el directorio de trabajo, en caso de necesitarse.

ggplot(DatosGEIH) + geom_point(aes (x= DatosGEIH$age, y = DatosGEIH$ingtot))+xlab("Edad")+ylab("Ingreso total")+ ggtitle("Ingreso vs Edad")+theme(plot.title = element_text(hjust = 0.5))
ggsave("Grafica_1.png")

ggplot(DatosGEIH) + geom_point(aes (x= DatosGEIH$age, y = DatosGEIH$ingtot))+xlab("Edad")+ylab("Ingreso total")+ ggtitle("Ingreso vs Edad")+theme(plot.title = element_text(hjust = 0.5))
ggsave("Grafica_2.png")

plot(hist(DatosGEIH$age),main="Main title",
     xlab="X axis title",
     ylab="Y axis title")
ggsave("Grafica_3.png")

png(file="E:/MAESTRIA UNIANDES/BIG DATA/Problem Set 2/conference-latex-template_10-17-19/Conference-LaTeX-template_10-17-19/Graficca_3.png",
    width=627, height=540)
hist(DatosGEIH$age, col="gold",main="Main title",
     xlab="X axis title",
     ylab="Y axis title")
dev.off()

#####Punto 1.1.2
DatosGEIH_18<-DatosGEIH[DatosGEIH$age>=18,] #Se realizará el análisis para individuos 
#con edad mayor o igual a 18 años
#Ahora, procederemos a realizar la clasificación de variables.
exp <- floor(c(DatosGEIH_18$p6426/12)) #Se anualiza la variable relacionada con la experiencia
view(exp)
educ <- DatosGEIH_18$p6210 #Se asigna la variable educación
View(educ)
DGEIH<-subset(DatosGEIH_18, select = c( "ingtot", "pet", "mes", "age", "sex","ocu", "oficio") ) #Hacer un subset con las variables a usar
DGEIH<-cbind(DGEIH, exp, educ) #Incluir las variables calculadas
View(DGEIH)
DGEIH<- DGEIH[DGEIH$ingtot>0,]
#####Punto 1.2.2
summary(DGEIH) #Se hace una inspección general de esta base de datos

#Se realiza la inspección para determinar cuántas variables contienen NA
cantidad_na <- sapply(DGEIH, function(x) sum(is.na(x)))
cantidad_na <- data.frame(cantidad_na)
porcentaje_na <- cantidad_na/nrow(DGEIH)
porcentaje_na <-porcentaje_na*100
porcentaje_na #Visualizo el porcentaje de los datos que tienen NA
DGEIH$oficio[is.na(DGEIH$oficio)] = 100 #Se imputa la nueva categoría 1oo a Oficio

DGEIH <- DGEIH %>% #Se vuelven categóricas las variables que así lo sean en la BD
  mutate_at(.vars = c(
    "pet","sex", "ocu", "educ", "oficio"),
    .funs = factor)

DGEIH[is.na(DGEIH)] = 0 #Se asigna 0 a las NA de la variable "exp" (Ver documento para explicación)
#DGEIH %>% subset(ingtot <= 2*iqr | is.na(ingtot)==T)
summary(DGEIH) #Se verifica que no existan NAs

#####Punto 1.2.3
View(DGEIH)
nrow(DGEIH)
ncol(DGEIH)
dim(DGEIH)
head(DGEIH)
tail(DGEIH)
#Descripción age (Edad)
Edad<- DGEIH$age
class(Edad)
plot(hist(Edad))
mean(Edad)
min(Edad)
max(Edad)
mean(Edad)
modeEdad(Edad)
modeEdad <- function(Edad){
  return(as.numeric(names(which.max(table(Edad)))))
}
modeEdad(Edad)
#Descripción PET
PET <- DGEIH$pet
View(DGEIH$pet)
class(PET)
levels(PET)
summary(PET)
#Educ
plot(hist(educ))
class(educ)
mean(educ)
modeEduc(educ)
modeEduc <- function(educ){
  return(as.numeric(names(which.max(table(educ)))))
}
modeEduc(educ)
#Descripción Ocupación
ocu<- DGEIH$ocu
class(ocu)
levels(ocu)
summary(ocu)
table(ocu)
pie(table(ocu))#generando un gráfico de torta
#Genero
sex<- DGEIH$sex
class(sex)
levels(sex)
summary(sex)
table(sex)
barplot(table(sex))
#Descripción de Experiencia
expp<- DGEIH$exp
View(expp)
class(expp)
plot(hist(expp))
mean(expp)
min(expp)
max(expp)
modeExp(expp)
modeExp <- function(expp){
  return(as.numeric(names(which.max(table(expp)))))
}
modeExp(expp)
#Descripción oficio (oficio)
library(modeest)
Oficio_<- DGEIH$oficio
class(Oficio_)
levels(Oficio_)
summary(Oficio_)
table(Oficio_)
barplot(table((Oficio_)))
mlv(Oficio_, method = "mfv")
#####PUNTO 1.3.1
#Se va a analizar la variable que describe el ingreso
#Primero decido analizar el ingreso total, ingreso total imputado y el observado.
View(subset(DatosGEIH, select = c(ingtot, ingtotes, ingtotob)))
#Por lo tanto, el ingreso total sería la suma del ingreso observado y el imputado.
#Sin embargo, es pertinente analizar las demás variables que pueden componer el ingreso, siendo estos, los ingresos por intereses, por ayudas, monetarios, arriendos, especie y monetarios: 
View(subset(DatosGEIH, select = c(ingtot, iof1 , iof1es, iof2, iof2es, iof3h, iof3hes, iof6, iof6es, isa, isaes, ie, iees, imdies , impa, impaes)))
#Al observar que efectivamente algunas de estas variables de ingreso suman en algunas observaciones el ingreso total (ingtot), se asume que efectivamente, la variable que describe todo el ingreso.
#Ahora, se comparará con las primas
View(subset(DatosGEIH, select = c(ingtot, y_primas_m, y_primaVacaciones_m	, y_primaServicios_m, y_primaNavidad_m	, y_subEducativo_m , y_subFamiliar_m)))
#Algunas variables son parte del ingreso total, por lo tanto, se compararán todas las variables contra Ingreso total para hacer la última verificación acerca de la variable ingtot
#De acuerdo a todas las verificaciones anteriores, se comprueba que la variable que describe el ingreso es igntot (Ingreso total), ya que esta contiene todas las demás variables acerca del ingreso que se encuentran en la base de datos de la GEIH

###PUNTO 1.3.2

#Se realizará la creación de la variable Age^2 para proceder con la estimación del modelo
lningtot<- log(DGEIH$ingtot)
DGEIH_AGE2<- DGEIH %>% mutate(Age2=age^2)
view(DGEIH_AGE2)
DGEIH_AGE2 <- cbind(DGEIH_AGE2, lningtot)
View(DGEIH_AGE2)
#Estimación del modelo:
require(tidyverse)
#ggplot(DGEIH_AGE2)+ geom_point(aes(x= age, y = lningtot)) #Gráfica de los datos para la observación de la relación visual de la variable age e ingtot
modelo1 <- lm(lningtot~age + Age2, data = DGEIH_AGE2)
summary(modelo1)
require("stargazer")
stargazer(modelo1,type="text")
#Se observa que tanto los residuales se ajustan a la recta de la regresión. 
ggplot(DGEIH_AGE2)+ geom_point(aes(x= modelo1$residuals, y = lningtot))
#Punto 1.3.3 Se analiza la predicción y su respectiva gráfica
predingtot<- 12.881+0.048*DGEIH_AGE2$age - 0.0005* DGEIH_AGE2$Age2
predingtot <- c(predingtot)
predingtot<- data.frame(predingtot)
View(predingtot)
ggplot(predingtot) + geom_point(aes (x= DGEIH_AGE2$age, y = predingtot))
#Punto 1.3.4 Bootstrap
#Ahora, vamos a medir la maximización, dentro de todos los puntos de la distribución, se escogerá la media de cada variable para contar con la maximización y poder generar el peakage 
age_bar<- mean(DGEIH_AGE2$age)
age2_bar<- mean(DGEIH_AGE2$Age2)
coeficientes<- modelo1$coefficients
beta0<-coeficientes[1]
beta0
beta1<-coeficientes[2]
beta1
beta2<-coeficientes[3]
beta2
require("tidyverse")
set.seed(10101)
R<- 1000
est_modelo1 <- rep(0,R)
for(i in 1: R){ age_bar<- mean(DGEIH_AGE2$age)
age2_bar<- mean(DGEIH_AGE2$Age2)
ingtot_sam<- sample_frac(DGEIH_AGE2, size = 1,replace = TRUE)
modf<- lm(lningtot~age + Age2, data = ingtot_sam)
coefic<- modf$coefficients
coefic
beta0 <- coefic[1]
beta1<- coefic[2]
beta2 <- coefic[3]
est_modelo1[i]<- beta1/(-2*beta2)
}
plot(hist(est_modelo1))
#Este vector está centrado al de rededor de 48 y tiene una forma aproximadamente normal.
mean(est_modelo1)
sqrt(var(est_modelo1))
quantile(est_modelo1, c(0.025, 0.975))
estimboot <- function(DGEIH_AGE2, index, 
                      age_bar = mean(DGEIH_AGE2$age), 
                      age2_bar =  mean(DGEIH_AGE2$Age2) ){
  coef(lm(lningtot~age+ Age2, data = DGEIH_AGE2, subset = index))}
resultado<- boot(DGEIH_AGE2, statistic = estimboot, R = 1000)
resultado
PeakAge <- beta1/(-2*beta2)
PeakAge


#####PUNTO 1.4.1

#Crear columna con datos para mujer (negación lógica de la columna sex)
DGEIH_AGE2 <- DGEIH_AGE2 %>% 
  mutate(sex_female= ifelse(test = sex ==1, 
                            yes = 0, 
                            no = 1))

#Se arman bases de datos con los datos de mujeres y hombres
DGEIH_AGE2_female <- DGEIH_AGE2[DGEIH_AGE2$sex_female==1,]
DGEIH_AGE2_male <- DGEIH_AGE2[DGEIH_AGE2$sex_female==0,]


modelo3<-lm(lningtot~sex_female,data=DGEIH_AGE2)#Regresión para el modelo de lningtot y sex_female

require("stargazer")
stargazer(modelo3,type="text")

#1.4.2

modelo5<-lm(lningtot~age + Age2,data=DGEIH_AGE2, subset=sex_female==1) #Realiza la regresión de la edad para mujeres
modelo6<-lm(lningtot~age + Age2, data=DGEIH_AGE2, subset=sex_female==0)#Realiza la regresión de la edad para hombres

stargazer(modelo5, modelo6,type="text") #Visualizar la tabla de la regresión

#Para graficar modelo
x <- DGEIH_AGE2$age
y <- DGEIH_AGE2$lningtot

#Para graficar el scatter plot
plot(x, y, main = "Regresión edad por género",
     xlab = "Edad", ylab = "LnIngtot",
     pch = 19, frame = FALSE)
# Agregar la línea de regresión
plot(x, y, main = "Regresión edad por género",
     xlab = "Edad", ylab = "LnIngtot",
     pch = 19, frame = FALSE)
abline(lm(lningtot~age + Age2,data=DGEIH_AGE2, subset=sex_female==1), col = "red", label)
abline(lm(lningtot~age + Age2,data=DGEIH_AGE2, subset=sex_female==0), col = "blue")

#1.4.3 - Bootstrap
#Ahora, vamos a medir la maximización, dentro de todos los puntos de la distribución, 
#se escogerá la media de cada variable para contar con la maximización y poder generar el peak age para mujeres, inicialmente
library(boot)
age_bar_5<- mean(DGEIH_AGE2_female $age)
age2_bar_5<- mean(DGEIH_AGE2_female $Age2)
coeficientes_5<- modelo5$coefficients
beta0_5<-coeficientes_5[1]
beta0_5
beta1_5<-coeficientes_5[2]
beta1_5
beta2_5<-coeficientes_5[3]
beta2_5
require("tidyverse")
set.seed(10101)
R<- 1000
est_modelo5 <- rep(0,R)
for(i in 1: R){ age_bar_5<- mean(DGEIH_AGE2_female$age)
age2_bar_5<- mean(DGEIH_AGE2_female$Age2)
ingtot_sam_5<- sample_frac(DGEIH_AGE2_female, size = 1,replace = TRUE)
modf_5<- lm(lningtot~age + Age2, data = ingtot_sam_5)
coefic5<- modf_5$coefficients
coefic5
beta0_5 <- coefic5[1]
beta1_5<- coefic5[2]
beta2_5 <- coefic5[3]
est_modelo5[i]<- beta1_5/(-2*beta2_5)
}
plot(hist(est_modelo5))
#Este vector está centrado al de rededor de 44 y tiene una forma aproximadamente normal.
mean(est_modelo5)
sqrt(var(est_modelo5))
quantile(est_modelo5, c(0.025, 0.975)) #Los intervalos de confianza
estimboot5 <- function(DGEIH_AGE2_female, index, 
                      age_bar_5 = mean(DGEIH_AGE2_female$age), 
                      age2_bar_5 =  mean(DGEIH_AGE2_female$Age2) ){
  coef(lm(lningtot~age+ Age2, data = DGEIH_AGE2_female, subset = index))}
resultado5<- boot(DGEIH_AGE2_female, statistic = estimboot5, R = 1000)
resultado5
PeakAge5 <- beta1_5/(-2*beta2_5)
PeakAge5

#Se hará el mismo análisis anterior, pero para los hombres
age_bar_6<- mean(DGEIH_AGE2_male $age)
age2_bar_6<- mean(DGEIH_AGE2_male $Age2)
coeficientes_6<- modelo6$coefficients
beta0_6<-coeficientes_6[1]
beta0_6
beta1_6<-coeficientes_6[2]
beta1_6
beta2_6<-coeficientes_6[3]
beta2_6
require("tidyverse")
set.seed(10101)
R<- 1000
est_modelo6 <- rep(0,R)
for(i in 1: R){ age_bar_6<- mean(DGEIH_AGE2_male$age)
age2_bar_6<- mean(DGEIH_AGE2_male$Age2)
ingtot_sam_6<- sample_frac(DGEIH_AGE2_male, size = 1,replace = TRUE)
modf_6<- lm(lningtot~age + Age2, data = ingtot_sam_6)
coefic6<- modf_6$coefficients
coefic6
beta0_6 <- coefic6[1]
beta1_6<- coefic6[2]
beta2_6 <- coefic6[3]
est_modelo6[i]<- beta1_6/(-2*beta2_6)
}
plot(hist(est_modelo6))
#Este vector está centrado al de rededor de 50 y tiene una forma aproximadamente normal.
mean(est_modelo6)
sqrt(var(est_modelo6))
quantile(est_modelo6, c(0.025, 0.975)) #Los intervalos de confianza
estimboot6 <- function(DGEIH_AGE2_male, index, 
                       age_bar_6 = mean(DGEIH_AGE2_male$age), 
                       age2_bar_6 =  mean(DGEIH_AGE2_male$Age2) ){
  coef(lm(lningtot~age+ Age2, data = DGEIH_AGE2_male, subset = index))}
resultado6<- boot(DGEIH_AGE2_male, statistic = estimboot6, R = 1000)
resultado6
PeakAge6 <- beta1_6/(-2*beta2_6)
PeakAge6

#1.4.4

#Se agregan las columnas de oficio y de ingreso total
DatosOficio <-subset(DGEIH_AGE2, select = c( "ingtot", "oficio") ) 

DatosOficio <- DatosOficio %>% #Se vuelven categóricas las variables que así lo sean en la BD
  mutate_at(.vars = c(
    "oficio"),
    .funs = factor)

#Se crea una nueva matriz para el cálculo del promedio por categoría de "oficio" 
Datosoficios<-aggregate(DatosOficio$ingtot, list(DatosOficio$oficio), FUN=mean)
Datosoficios <- Datosoficios[order(Datosoficios$x), ] #Se ordena la base

q<-quantile(Datosoficios$x, probs=c(0.2,0.4,0.6,0.8)) #Se calculan los cuantiles
Datosoficios= Datosoficios %>% #Se asignan los cuantiles dependiendo del rango del promedio del ingreso
  mutate(Clasificacion_oficio_similar=case_when(x <q[1]  ~ "1", 
                                                x >= q[1] & x < q[2] ~ "2", 
                                                x >= q[2] & x < q[3] ~ "3", 
                                                x >= q[3] & x < q[4] ~ "4", 
                                                x >= q[4] ~ "5"))  

DGEIH_AGE2 <- DGEIH_AGE2 %>% #Se le asigna la nueva categoría en la nueva variable "Categor_oficio" en la base de datos de trabajo
  mutate(Categor_oficio= case_when((oficio==78|oficio==80|oficio==75| oficio==56| oficio==99| oficio==54|oficio==36|oficio==55|oficio==94|oficio==79|oficio==82|oficio==93|oficio==57|oficio==97|oficio==81|oficio==53) ~ "1", 
                                   (oficio==91|oficio==72|oficio==37| oficio==63| oficio==74| oficio==76|oficio==87|oficio==89|oficio==77|oficio==90|oficio==45|oficio==52|oficio==95|oficio==83|oficio==62|oficio==38) ~ "2", 
                                   (oficio==14|oficio==59|oficio==92| oficio==84| oficio==100| oficio==98|oficio==85|oficio==34|oficio==18|oficio==41|oficio==33|oficio==58|oficio==86|oficio==51|oficio==39|oficio==3) ~ "3", 
                                   (oficio==49|oficio==17|oficio==32| oficio==60| oficio==50| oficio==88|oficio==16|oficio==35|oficio==61|oficio==70|oficio==40|oficio==19|oficio==42|oficio==20|oficio==7|oficio==44) ~ "4", 
                                   (oficio==73|oficio==15|oficio==13| oficio==11| oficio==43| oficio==30|oficio==5|oficio==8|oficio==2|oficio==6|oficio==1|oficio==31|oficio==9|oficio==21|oficio==12|oficio==96|oficio==4) ~ "5"))  

modelo7<-lm(lningtot~sex_female+Categor_oficio, data = DGEIH_AGE2) #Se hace la regresión con el modelo propuesto y la variable construida
stargazer(modelo7, type="text")

#FWL Theorem
modelo8<-lm(lningtot~Categor_oficio, data = DGEIH_AGE2) #Regresión con variable de ingreso y las que no se consideran relevantes para beta 2
modelo9<-lm(sex_female~Categor_oficio, data = DGEIH_AGE2) #Regresión de variable de interés con la otra variable contemplada en el modelo.
modelo10<-lm(modelo8$residuals~modelo9$residuals, data=DGEIH_AGE2) #Regresión de los residuos de los modelos 8 y 9
stargazer(modelo7,modelo8, modelo9, modelo10, type="text") #Visualización de los resultados


### PUNTO 1.5

#Punto 1.5.1.1

set.seed(10101)
DGEIH_AGE2 <- DGEIH_AGE2 %>%
          mutate(holdout= as.logical(1:nrow(DGEIH_AGE2) %in%
                               sample(nrow(DGEIH_AGE2), nrow(DGEIH_AGE2)*.3)))
         #genera un indicador general para definir el train y el test (30%)

#Se crean las bases para el test y train         
test<-DGEIH_AGE2[DGEIH_AGE2$holdout==T,]
train<-DGEIH_AGE2[DGEIH_AGE2$holdout==F,]


#Empezamos a estimar un modelo con solo la constante
model1<- lm(lningtot~1, data = train)
summary(model1)
coef(model1)
test$model1<-predict(model1,newdata = test)
#Se calcula el MSE
MSE_modelos_1<-with(test,mean((lningtot-model1)^2))
stargazer(model1, type="text")
#Es decir, el MSE se encuentra en: 


#Punto 1.5.1.2

#Se estiman los modelos de los puntos anteriores (del 1.3, 1.4)
#Modelo del punto 1.3
model2<- lm(lningtot~age + Age2, data = train)
summary(model2)
coef(model2)
test$model2<-predict(model2,newdata = test)
#Se calcula el MSE
MSE_modelos_2<-with(test,mean((lningtot-model2)^2))

#Se calcula el MSE para el Modelo del punto 1.4.1
model3<- lm(lningtot~sex_female, data = train)
summary(model3)
coef(model3)
test$model3<-predict(model3,newdata = test)
#Se calcula el MSE
MSE_modelos_3<-with(test,mean((lningtot-model3)^2))
stargazer(model3, type="text")

#Se calcula el MSE para el Modelo del punto 1.4.4
model4<- lm(lningtot~sex_female+Categor_oficio, data = train)
summary(model4)
coef(model4)
test$model4<-predict(model4,newdata = test)
#Se calcula el MSE
MSE_modelos_4<-with(test,mean((lningtot-model4)^2))
stargazer(model2, model3, model4, type="text")


#Punto 1.5.1.3

#Estimar diversos modelos,con la base train y el error con la base test

model5<- lm(lningtot~sex_female+ocu,data= train)
#summary(model5)
test$model5<-predict(model5,newdata = test)
MSE_modelos_5<-with(test,mean((lningtot-model5)^2))

model6<- lm(lningtot~ ocu +age+exp+ sex_female*Age2, data= train)
#summary(model6)
test$model6 <- predict(model6, newdata = test)
MSE_modelos_6<-with(test, mean(lningtot-model6)^2)

model7<- lm(lningtot~sex_female+ocu+exp+age+Age2,data= train)
#summary(model7)
test$model7<-predict(model7,newdata = test)
MSE_modelos_7<-with(test,mean((lningtot-model7)^2))

model8<- lm(lningtot~sex_female+ocu+exp+poly(age,3),data= train)
#summary(model8)
test$model8<-predict(model8,newdata = test)
MSE_modelos_8<-with(test,mean((lningtot-model8)^2))

model9<- lm(lningtot~+sex_female+ocu+exp+poly(age,4),data= train)
#summary(model9)
test$model9<-predict(model9,newdata = test)
MSE_modelos_9<-with(test,mean((lningtot-model9)^2))


#Punto 1.5.1.4

MSE_modelos<-c(MSE_modelos_1, MSE_modelos_2, MSE_modelos_3, MSE_modelos_4, MSE_modelos_5, MSE_modelos_6,MSE_modelos_7, MSE_modelos_8, MSE_modelos_9)
x_label<-c('modelo1','modelo 2', 'modelo3', 'modelo 4', 'modelo5','modelo6','modelo7','modelo8', 'modelo9')
MSE_<-data.frame(x_label,MSE_modelos)

#Se grafica los MSE para cada modelo
ggplot(data=MSE_, aes(x = x_label, y = MSE_modelos, group=1)) + 
  geom_line()+   geom_point()

#Para tener idea de cuáles modelos tienen el MSE más bajo
MSE_ordenado <- MSE_[order(MSE_$MSE_modelos), ]
View(MSE_ordenado)
#Punto 1.5.1.5

#Leverage statistic

#Definición de las variables

alpha <- c()
u <- c()
h <- c()

#El modelo con menor MSE es el siguiente y se calcula para el test:
modell6<-lm(lningtot~ ocu+age+exp+ sex_female*Age2, data = test)

#Calcular el leverage para el modelo con el menor MSE

alphass <- c()
for (j in 1:nrow(test)) {
  uj <- modell6$residual[j]
  hj <- lm.influence(modell6)$hat[j]
  alpha <- uj/(1-hj)
  alphass <- c(alphass, alpha)
} 

#Otra alternativa en vez del for:
#u<-modell6$residual
#u
#h<-lm.influence(modell6)$hat
#h
#alpha<-u/(1-h)

#Se determinó que mayor a 1 o menor que -1, podrían ser leverages altos
#Se calculará si el leverage es importante para las observaciones en este modelo
alphass<-data.frame(alphass)
leverage<-alphass[alphass$alphass>=1|alphass<=-1,]
leverage<-data.frame(leverage)
Porcentaje_leverage<-((nrow(leverage)/nrow(alphass)*100))
xlabel_alpha<-1:nrow(test)
xlabel_alpha<-data.frame(xlabel_alpha)
alphass<-cbind(alphass, xlabel_alpha)
view(Porcentaje_leverage)
ggplot(data=alphass, aes(x =xlabel_alpha, y = alphass, group=1)) + 
     geom_point()

max(alphass$alphass)
min(alphass$alphass)

#1.5.2 K-Fold

#Se utilizará la librería "caret"
library(caret)

#Para crear columna de 1 para hacer calcular el RMSE para la regresión con una
#constante, de acuerdo con lo realizado en el punto anterior
c_1 <- c()
for (j in 1:nrow(DGEIH_AGE2)) {
  c1 <- 1
  c_1 <- c(c_1, c1)
}

DGEIH_AGE2<-cbind(DGEIH_AGE2,c_1)

#Se realiza la validación de los modelos usando la validación cruzada K-fold, con k=5
modelll1<-train(lningtot~c_1, 
                          data=DGEIH_AGE2,
                          trControl=trainControl(method="cv", number=5),
                          method="null")
RMSE_modelll1<-modelll1$resample
RMSE_modellll1<-RMSE_modelll1$RMSE
RMSE_modellll1<-mean(RMSE_modellll1) #Se obtiene finalmente la media del RMSE


modelll2<-train(lningtot~age + Age2, 
                data=DGEIH_AGE2,
                trControl=trainControl(method="cv", number=5),
                method="lm")
RMSE_modelll2<-modelll2$resample
RMSE_modellll2<-RMSE_modelll2$RMSE
RMSE_modellll2<-mean(RMSE_modellll2) #Se obtiene finalmente la media del RMSE

modelll3<-train(lningtot~sex_female, 
                data=DGEIH_AGE2,
                trControl=trainControl(method="cv", number=5),
                method="lm")
RMSE_modelll3<-modelll3$resample
RMSE_modellll3<-RMSE_modelll3$RMSE
RMSE_modellll3<-mean(RMSE_modellll3) #Se obtiene finalmente la media del RMSE

modelll4<-train(lningtot~sex_female+Categor_oficio, 
                data=DGEIH_AGE2,
                trControl=trainControl(method="cv", number=5),
                method="lm")

RMSE_modelll4<-modelll4$resample
RMSE_modellll4<-RMSE_modelll4$RMSE
RMSE_modellll4<-mean(RMSE_modellll4) #Se obtiene finalmente la media del RMSE

modelll5<-train(lningtot~sex_female+ocu, 
                data=DGEIH_AGE2,
                trControl=trainControl(method="cv", number=5),
                method="lm")

RMSE_modelll5<-modelll5$resample
RMSE_modellll5<-RMSE_modelll5$RMSE
RMSE_modellll5<-mean(RMSE_modellll5) #Se obtiene finalmente la media del RMSE

modelll6<-train(lningtot~ ocu +age+exp+ sex_female*Age2, 
                data=DGEIH_AGE2,
                trControl=trainControl(method="cv", number=5),
                method="lm")

RMSE_modelll6<-modelll6$resample
RMSE_modellll6<-RMSE_modelll6$RMSE
RMSE_modellll6<-mean(RMSE_modellll6) #Se obtiene finalmente la media del RMSE


modelll7<-train(lningtot~sex_female+ocu+exp+age+Age2, 
                data=DGEIH_AGE2,
                trControl=trainControl(method="cv", number=5),
                method="lm")
RMSE_modelll7<-modelll7$resample
RMSE_modellll7<-RMSE_modelll7$RMSE
RMSE_modellll7<-mean(RMSE_modellll7)

modelll8<-train(lningtot~sex_female+ocu+exp+poly(age,3), 
                data=DGEIH_AGE2,
                trControl=trainControl(method="cv", number=5),
                method="lm")
RMSE_modelll8<-modelll8$resample
RMSE_modellll8<-RMSE_modelll8$RMSE
RMSE_modellll8<-mean(RMSE_modellll8)

modelll9<-train(lningtot~+sex_female+ocu+exp+poly(age,4), 
                data=DGEIH_AGE2,
                trControl=trainControl(method="cv", number=5),
                method="lm")
RMSE_modelll9<-modelll9$resample
RMSE_modellll9<-RMSE_modelll9$RMSE
RMSE_modellll9<-mean(RMSE_modellll9)


#Se conforma la base de datos con los resultados del RMSE
RMSE_modelos<-c(RMSE_modellll1, RMSE_modellll2, RMSE_modellll3, RMSE_modellll4,RMSE_modellll5,RMSE_modellll6,RMSE_modellll7, RMSE_modellll8, RMSE_modellll9)
x_label<-c('modelo1','modelo 2', 'modelo3', 'modelo 4', 'modelo5','modelo6','modelo7','modelo8', 'modelo9')
RMSE_<-data.frame(x_label,RMSE_modelos)

#Se grafica el resultado
ggplot(data=RMSE_, aes(x = x_label, y = RMSE_modelos, group=1)) + 
  geom_line()+   geom_point()

RMSE_ordenado <- RMSE_[order(RMSE_$RMSE_modelos), ]
view(RMSE_ordenado)


#Se realiza la validación de los modelos usando la validación cruzada K-fold, con k=10
modelll1_10<-train(lningtot~c_1, 
                data=DGEIH_AGE2,
                trControl=trainControl(method="cv", number=10),
                method="null")
RMSE_modelll1_10<-modelll1_10$resample
RMSE_modellll1_10<-RMSE_modelll1_10$RMSE
RMSE_modellll1_10<-mean(RMSE_modellll1_10) #Se obtiene finalmente la media del RMSE


modelll2_10<-train(lningtot~age + Age2, 
                data=DGEIH_AGE2,
                trControl=trainControl(method="cv", number=10),
                method="lm")
RMSE_modelll2_10<-modelll2_10$resample
RMSE_modellll2_10<-RMSE_modelll2_10$RMSE
RMSE_modellll2_10<-mean(RMSE_modellll2_10) #Se obtiene finalmente la media del RMSE

modelll3_10<-train(lningtot~sex_female, 
                data=DGEIH_AGE2,
                trControl=trainControl(method="cv", number=10),
                method="lm")
RMSE_modelll3_10<-modelll3_10$resample
RMSE_modellll3_10<-RMSE_modelll3_10$RMSE
RMSE_modellll3_10<-mean(RMSE_modellll3_10) #Se obtiene finalmente la media del RMSE

modelll4_10<-train(lningtot~sex_female+Categor_oficio, 
                data=DGEIH_AGE2,
                trControl=trainControl(method="cv", number=10),
                method="lm")

RMSE_modelll4_10<-modelll4_10$resample
RMSE_modellll4_10<-RMSE_modelll4_10$RMSE
RMSE_modellll4_10<-mean(RMSE_modellll4_10) #Se obtiene finalmente la media del RMSE

modelll5_10<-train(lningtot~sex_female+ocu, 
                data=DGEIH_AGE2,
                trControl=trainControl(method="cv", number=10),
                method="lm")

RMSE_modelll5_10<-modelll5_10$resample
RMSE_modellll5_10<-RMSE_modelll5_10$RMSE
RMSE_modellll5_10<-mean(RMSE_modellll5_10) #Se obtiene finalmente la media del RMSE

modelll6_10<-train(lningtot~ ocu +age+exp+ sex_female*Age2, 
                data=DGEIH_AGE2,
                trControl=trainControl(method="cv", number=10),
                method="lm")

RMSE_modelll6_10<-modelll6_10$resample
RMSE_modellll6_10<-RMSE_modelll6_10$RMSE
RMSE_modellll6_10<-mean(RMSE_modellll6_10) #Se obtiene finalmente la media del RMSE


modelll7_10<-train(lningtot~sex_female+ocu+exp+age+Age2, 
                data=DGEIH_AGE2,
                trControl=trainControl(method="cv", number=10),
                method="lm")
RMSE_modelll7_10<-modelll7_10$resample
RMSE_modellll7_10<-RMSE_modelll7_10$RMSE
RMSE_modellll7_10<-mean(RMSE_modellll7_10)

modelll8_10<-train(lningtot~sex_female+ocu+exp+poly(age,3), 
                data=DGEIH_AGE2,
                trControl=trainControl(method="cv", number=10),
                method="lm")
RMSE_modelll8_10<-modelll8_10$resample
RMSE_modellll8_10<-RMSE_modelll8_10$RMSE
RMSE_modellll8_10<-mean(RMSE_modellll8_10)

modelll9_10<-train(lningtot~+sex_female+ocu+exp+poly(age,4), 
                data=DGEIH_AGE2,
                trControl=trainControl(method="cv", number=10),
                method="lm")
RMSE_modelll9_10<-modelll9_10$resample
RMSE_modellll9_10<-RMSE_modelll9_10$RMSE
RMSE_modellll9_10<-mean(RMSE_modellll9_10)


#Se conforma la base de datos con los resultados del RMSE
RMSE_modelos_10<-c(RMSE_modellll1_10, RMSE_modellll2_10, RMSE_modellll3_10, RMSE_modellll4_10,RMSE_modellll5_10,RMSE_modellll6_10,RMSE_modellll7_10, RMSE_modellll8_10, RMSE_modellll9_10)
x_label<-c('modelo1','modelo 2', 'modelo3', 'modelo 4', 'modelo5','modelo6','modelo7','modelo8', 'modelo9')
RMSE_10<-data.frame(x_label,RMSE_modelos_10)

#Se grafica el resultado
ggplot(data=RMSE_10, aes(x = x_label, y = RMSE_modelos_10, group=1)) + 
  geom_line()+   geom_point()

RMSE_ordenado_10 <- RMSE_10[order(RMSE_10$RMSE_modelos_10), ]
view(RMSE_ordenado_10)




#1.5.3 LOOCV

#Se realizará el LOOCV para el modelo 4, utilizando la función de la librería caret y modificando los datos para su implementación
#El correr esta función demoró 40 minutos.
modelll20<-train(lningtot~sex_female+Categor_oficio, 
                 data=DGEIH_AGE2,
                 trControl=trainControl(method="LOOCV", number=nrow(DGEIH_AGE2)),
                 method="lm")
RMSE_modelll20<-modelll20$results
RMSE_modellll20<-RMSE_modelll20$RMSE
RMSE_modellll20<-mean(RMSE_modellll20)
modelll20
view(RMSE_modellll20)

#Se calcula el leverage de los datos con respecto al modelo 4
#El código demora aproximadamente 20 minutos en correr
alphas_m4<-c()
modelo4_alpha<-lm(lningtot~ ocu+age+exp+ sex_female*Age2, data = DGEIH_AGE2)
for (j in 1:nrow(DGEIH_AGE2)) {
  uj4 <- modelo4_alpha$residual[j]
  hj4 <- lm.influence(modelo4_alpha)$hat[j]
  alpham4 <- uj4/(1-hj4)
  alphas_m4 <- c(alphas_m4, alpham4)
}
#Hasta aquí demora los 20 minutos
alphas_m4<-data.frame(alphas_m4)
leverage_m4<-alphas_m4[alphas_m4$alphas_m4>=1|alphas_m4<=-1,]
leverage_m4<-data.frame(leverage_m4)
Porcentaje_leverage_m4<-((nrow(leverage_m4)/nrow(alphas_m4)*100))
xlabel_alpha_m4<-1:nrow(DGEIH_AGE2)
xlabel_alpha_m4<-data.frame(xlabel_alpha_m4)
max(alphas_m4$alphas_m4)
min(alphas_m4$alphas_m4)

alphas_m4<-cbind(alphas_m4, xlabel_alpha_m4)
view(Porcentaje_leverage_m4)
ggplot(data=alphas_m4, aes(x =xlabel_alpha_m4, y = alphas_m4, group=1)) + 
  geom_point()




