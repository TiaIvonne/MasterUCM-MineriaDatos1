setwd("/Users/ivonne.mendoza/Desktop/MasterUCM/Semana7/Documentación minería de datos y modelización predictiva-20220502/evaluacion")



# Cargo las funciones que voy a utilizar después
source("FuncionesMineriaAida.R")


# Cargo las librerias que me van a hacer falta
library(caret)
library(questionr)
library(OneR)
library(tibble)
library("readxl")
library("writexl")
library(car)
library(Hmisc)
library(corrplot)
library(ggplot2)
library(tidyr)
library(caret)
library(grid)
library(gridExtra)

options("scipen" = 10)
# Parto de los datos sin atípicos ni ausentes guardados
datosDep<-readRDS("datosEleccionesDep2")
str(datosDep)

#Separo objetivo del set de datos
varObjCont<-datosDep$varObjCont

input<-datosDep[,-(1)]
head(input)
summary(input)
head(input[,(25:26)])

#Obtengo las mejores transformaciones
TransfCont<-Transf_Auto(Filter(is.numeric, input[,-c(25,26)]),varObjCont)
#indico que no quiero transformar las aleatorias (estan en las columnas 14 y 15)
names(TransfCont) #veo las transformaciones que se han aplicado
#todas han sido transformadas


#Pruebo la discretización de las variables cuantitativas
discCont<-droplevels(optbin(data.frame(Filter(is.numeric, 
                                              input[,-c(25,26)]),bin(varObjCont,nbins=5,
                                              method = "content"))))[,-(ncol(Filter(is.numeric, input[,-c(25,26)]))+1)]
names(discCont)<-paste("disc", names(discCont), sep = "_")

names(discCont)
#Verificamos el reparto de las nuevas categorías


apply(discCont,2,freq) # se deben cambiar


# Se fusionan categorias infra representadas

#$disc_Censo
freq(discCont$disc_Censo)
aggregate(varObjCont, by = list(discCont$disc_Censo), mean)
discCont$disc_Censo <- car::recode(discCont$disc_Censo,
                                             "c('(-2.39e+03,1.64e+03]', '(4.39e+03,6.3e+03]') =
                                             ' (-2.39e+03,6.3e+03]'")

discCont$disc_Censo <- car::recode(discCont$disc_Censo,
                                   "c('(3.2e+03,4.39e+03]', '(6.3e+03,2.39e+06]') =
                                             ' (3.2e+03,6.3e+03]'")

freq(discCont$disc_Censo)


#$disc_Age_over65_Ptge
freq(discCont$disc_Age_over65_Ptge)
aggregate(varObjCont, by = list(discCont$disc_Age_over65_Ptge), mean)
discCont$disc_Age_over65_Ptge <- car::recode(discCont$disc_Age_over65_Ptge,
                                             "c('(28.1,28.2]', '(28.2,30.8]') =
                                             '(28.1,30.8]'")
freq(discCont$disc_Age_over65_Ptge)

#$disc_WomanPopulationPtge
freq(discCont$disc_WomanPopulationPtge)
aggregate(varObjCont, by = list(discCont$disc_WomanPopulationPtge), mean)
discCont$disc_WomanPopulationPtge <- car::recode(discCont$disc_WomanPopulationPtge,
                                             "c('(46.9,47.4]', '(47.4,47.7]', '(47.7,48]') =
                                             '(46.9,48]'")
freq(discCont$disc_WomanPopulationPtge)


#$disc_ForeignersPtge
freq(discCont$disc_ForeignersPtge)
aggregate(varObjCont, by = list(discCont$disc_ForeignersPtge), mean)

discCont$disc_ForeignersPtge <- car::recode(discCont$disc_ForeignersPtge,
                                             "c('(4.95,5.85]', '(5.85,6.14]','(6.14,6.45]') = '(4.95, 6.45]'")
freq(discCont$disc_ForeignersPtge)


#$disc_UniversityPtge
freq(discCont$disc_UniversityPtge)
aggregate(varObjCont, by = list(discCont$disc_UniversityPtge), mean)
discCont$disc_UniversityPtge <- car::recode(discCont$disc_UniversityPtge,
                                            "c('(8.76, 9.74] ', '(9.74,11.1]', '(11.1,11.3]' ) = '(8.76, 11.3]'")


discCont$disc_UniversityPtge <- car::recode(discCont$disc_UniversityPtge,
                                            "c('(8.76,11.3]', '(8.77, 11.3] ' ) = '(8.77, 11.3]'")


freq(discCont$disc_UniversityPtge)


#$disc_Densidad
freq(discCont$disc_Densidad) 
aggregate(varObjCont, by = list(discCont$disc_Densidad), mean)
discCont$disc_Densidad <- car::recode(discCont$disc_Densidad, 
                                      "c('(115,132]', '(132,178]', '(178,225]') = '(115, 225]'")
freq(discCont$disc_Densidad)                                      


#$disc_PersonasInmueble
aggregate(varObjCont, by = list(discCont$disc_PersonasInmueble), mean)
discCont$disc_PersonasInmueble <- car::recode(discCont$disc_PersonasInmueble,
                                              "c('(1.25,1.28]', '(1.28,1.3]', '(1.3,1.38]') = '(1.25,1.38]'")
freq(discCont$disc_PersonasInmueble)


#$disc_Explotaciones
freq(discCont$disc_Explotaciones)
aggregate(varObjCont, by = list(discCont$disc_Explotaciones), mean)
discCont$disc_Explotaciones <- car::recode(discCont$disc_PersonasInmueble,
                                           "c('139,144]','(144,158]','(158,175]') = '(139,175]'")
freq(discCont$disc_Explotaciones)

# $disc_WomenUnemploymentPtge
freq(discCont$disc_WomenUnemploymentPtge)
aggregate(varObjCont, by = list(discCont$disc_WomenUnemploymentPtge), mean)
discCont$disc_WomenUnemploymentPtge <- car::recode(discCont$disc_WomenUnemploymentPtge,
                                           "c('(49.4,52.5]','(52.5,52.7]','(52.7,53.1]') = 
                                           '(49.4,53.1]'")
freq(discCont$disc_WomenUnemploymentPtge)

#$disc_UnemployLess25_Ptge
freq(discCont$disc_UnemployLess25_Ptge)
aggregate(varObjCont, by = list(discCont$disc_UnemployLess25_Ptge), mean)
discCont$disc_UnemployLess25_Ptge <- car::recode(discCont$disc_UnemployLess25_Ptge,
                                                   "c('(6.49,6.77]', '(6.74,7.76]') = 
                                           '(6.49, 7.76]'")
freq(discCont$disc_UnemployLess25_Ptge)

#$disc_UnemployMore40_Ptge
aggregate(varObjCont, by = list(discCont$disc_UnemployMore40_Ptge), mean)
discCont$disc_UnemployMore40_Ptge <- car::recode(discCont$disc_UnemployMore40_Ptge,
                                                 "c('(52,52.5]','(52.5,54.1]') = 
                                           '(5.52, 54.1]'")
freq(discCont$disc_UnemployMore40_Ptge)

# $disc_AgricultureUnemploymentPtge

aggregate(varObjCont, by = list(discCont$disc_AgricultureUnemploymentPtge), mean)
discCont$disc_AgricultureUnemploymentPtge <- car::recode(discCont$disc_AgricultureUnemploymentPtge,
                                                 "c('(-0.0444,4.98]', '(4.98,6.1]', '(6.25,9.38]', ' (9.38,44.5] ') = '(-0.0444, 44.5]'")

discCont$disc_AgricultureUnemploymentPtge <- car::recode(discCont$disc_AgricultureUnemploymentPtge,
                                                         "c('(-0.0444, 44.5]', '(6.1,6.25]') = '(6.1, 44.5]'")


freq(discCont$disc_AgricultureUnemploymentPtge)


# disc_ConstructionUnemploymentPtge
freq(discCont$disc_ConstructionUnemploymentPtge)
#aggregate(varObjCont, by = list(discCont$disc_ConstructionUnemploymentPtge), mean)
discCont$disc_ConstructionUnemploymentPtge <- car::recode(discCont$disc_ConstructionUnemploymentPtge,
                                                         "c('(5.88,6.49]',
                                                         '(6.49,7.18]', '(7.18,7.4]') = '(5.88, 7.4]'")

freq(discCont$disc_ConstructionUnemploymentPtge)


# $d$disc_AutonomosPtge
freq(discCont$disc_AutonomosPtge)
#aggregate(varObjCont, by = list(discCont$disc_AutonomosPtge), mean)
discCont$disc_AutonomosPtge <- car::recode(discCont$disc_AutonomosPtge,
                                                 "c('(9.71, 11.4] ', '(11.4,12.8]') = 
                                           '(9.71, 12.7]'")
freq(discCont$disc_AutonomosPtge)


apply(discCont,2,freq) # se deben cambiar



freq(discCont$disc_ServicesUnemploymentPtge)
aggregate(varObjCont, by = list(discCont$disc_ServicesUnemploymentPtge), mean)
discCont$disc_ServicesUnemploymentPtge <- car::recode(discCont$disc_ServicesUnemploymentPtge,
                                           "c('(60.4,62.5] ', '(60.4,64.9]') = 
                                           '(60.4, 64.9]'")
freq(discCont$disc_ServicesUnemploymentPtge)


#Junto las originales, transformaciones y discretizadas
datos_todocont<-data.frame(varObjCont,input,TransfCont,discCont)
names(datos_todocont)



#Hago la partición
set.seed(2701)
trainIndex <- createDataPartition(datos_todocont$varObjCont, p=0.8, list=FALSE)
data_train <- datos_todocont[trainIndex,]
data_test <- datos_todocont[-trainIndex,]

head(data_train)



# Este fue el modelo ganador/ modelo 4
#Modelo 1
modeloManual<-lm(varObjCont~PartidoCCAA + CCAA + AutonomosPtge + UnemploymentPtge, data = data_train)
modeloManual$rank
Rsq(modeloManual,"varObjCont",data_train)
Rsq(modeloManual,"varObjCont",data_test)


## SELECCION CON VARIABLES ORIGINALES
null<-lm(varObjCont~1, data=data_train) #Modelo minimo
head(data_train[,(1:27)])
full<-lm(varObjCont~., data=data_train[,c(1:27)]) #Modelo maximo, seleccionamos las columnas de las variables originales


#AIC STEPWISE null
#Modelo 2
modeloStepAIC<-step(null, scope=list(lower=null, upper=full), direction="both", trace = F)
Rsq(modeloStepAIC,"varObjCont",data_test)
modeloStepAIC$rank


#AIC BACKWISE full
# Modelo 3
modeloBackAIC<-step(full, scope=list(lower=null, upper=full), direction="backward", trace = F)
Rsq(modeloBackAIC,"varObjCont",data_test)
modeloBackAIC$rank #son iguales


#AIC FORWARD
# Modelo 4
modeloForwAIC<-step(null, scope=list(lower=null, upper=full), direction="forward", trace = F)
Rsq(modeloForwAIC,"varObjCont",data_test)
modeloForwAIC$rank #son iguales


# BIC STEPWISE
# Modelo 5
modeloStepBIC<-step(null, scope=list(lower=null, upper=full), direction="both",trace = F, k=log(nrow(data_train)))
Rsq(modeloStepBIC,"varObjCont",data_test) 
modeloStepBIC$rank 

# BIC BACKWISE
# Modelo 6
modeloBackBIC<-step(full, scope=list(lower=null, upper=full), direction="backward",trace = F, k=log(nrow(data_train)))
Rsq(modeloBackBIC,"varObjCont",data_test)
modeloBackBIC$rank

# BIC FORWARD
# Modelo 7
modeloForwBIC<-step(null, scope=list(lower=null, upper=full), direction="forward",trace = F, k=log(nrow(data_train)))
Rsq(modeloForwBIC,"varObjCont",data_test)
modeloBackBIC$rank

# Los bic mejoran en menor cantidad de variables 

#Parecen preferibles los BIC, por el principio de parsimonia



## SELECCION CON ORIGINALES E INTERACCIONES
#Genero interacciones solo con las originales
fullInt<-lm(varObjCont~.^2, data=data_train[,c(1:27)])

# No correr, demora 8 minutos en ejecutar

# Modelos AIC
# Modelo 8
#t1 <- proc.time()
#modeloStepAIC_int<-step(null, scope=list(lower=null, upper=fullInt), direction="both", trace = F)
#print(proc.time() - t1)
#Rsq(modeloStepAIC_int,"varObjCont",data_test)
#modeloStepAIC_int$rank

# Modelo 9
modeloBackAIC_int<-step(full, scope=list(lower=null, upper=fullInt), direction="backward", trace = F)
Rsq(modeloBackAIC_int,"varObjCont",data_test)
modeloBackAIC_int$rank


# no correr, demora demasiado
# Modelo 10
#modeloForwAIC_int<-step(null, scope=list(lower=null, upper=fullInt), direction="forward", trace = F)
# print(proc.time() - t2)
# Rsq(modeloForwAIC_int,"varObjCont",data_test)
# modeloForwAIC_int$rank

# Modelos BIC
# STEPWISE
# Modelo 11
modeloStepBIC_int<-step(null, scope=list(lower=null, upper=fullInt), direction="both",trace = F, k=log(nrow(data_train)))
Rsq(modeloStepBIC_int,"varObjCont",data_test) 
modeloStepBIC_int$rank #Un pelin mejor que los anteriores, sobretodo que el AIC

# BACKWARD
# Modelo 12
modeloBackBIC_int<-step(full, scope=list(lower=null, upper=fullInt), direction="backward",trace = F, k=log(nrow(data_train)))
Rsq(modeloBackBIC_int,"varObjCont",data_test) 
modeloBackBIC_int$rank #Un pelin mejor que los anteriores, sobretodo que el AIC

# FORWARD
# Modelo 13
# No correr demora demasiado
# modeloForwBIC_int<-step(null, scope=list(lower=null, upper=fullInt), direction="forward",trace = F, k=log(nrow(data_train)))
# Rsq(modeloForwBIC_int,"varObjCont",data_test) 
# modeloForwBIC_int$rank #demora demasiado

## SELECCION CON ORIGINALES y TRANSFORMADAS
# Pruebo con las transf y las originales 
head(data_train[,(1:45)])
fullT<-lm(varObjCont~., data=data_train[,c(1:45)])

# Modelos AIC
# Modelo 14
modeloStepAIC_trans<-step(null, scope=list(lower=null, upper=fullT), trace = F, direction="both")
Rsq(modeloStepAIC_trans,"varObjCont",data_test) 
modeloStepAIC_trans$rank 

# Modelo 15
modeloBackAIC_trans<-step(full, scope=list(lower=null, upper=fullT), trace = F, direction="backward")
Rsq(modeloBackAIC_trans,"varObjCont",data_test) 
modeloBackAIC_trans$rank # bajan las variables

# Modelo 16
modeloForwAIC_trans<-step(null, scope=list(lower=null, upper=fullT), trace = F, direction="forward")
Rsq(modeloForwAIC_trans,"varObjCont",data_test) 
modeloForwAIC_trans$rank # bajan las variables


# Modelos BIC
# Modelo 17
modeloStepBIC_trans<-step(null, scope=list(lower=null, upper=fullT), trace = F, direction="both",k=log(nrow(data_train)))
Rsq(modeloStepBIC_trans,"varObjCont",data_test) 
modeloStepBIC_trans$rank
# Modelo 18
modeloBackBIC_trans<-step(full, scope=list(lower=null, upper=fullT), trace = F, direction="backward",k=log(nrow(data_train)))
Rsq(modeloBackBIC_trans,"varObjCont",data_test) 
modeloBackBIC_trans$rank
# Modelo 19
modeloForwBIC_trans<-step(null, scope=list(lower=null, upper=fullT), trace = F, direction="forward",k=log(nrow(data_train)))
Rsq(modeloForwBIC_trans,"varObjCont",data_test) 
modeloForwBIC_trans$rank


# Pruebo con las transf, las originales y las discretizadas
fulltodo<-lm(varObjCont~., data=data_train)


# Modelo 20
modeloStepAIC_todo<-step(null, scope=list(lower=null, upper=fulltodo), trace = F, direction="both")
Rsq(modeloStepAIC_todo,"varObjCont",data_test) # Mejora mucho
modeloStepAIC_todo$rank #muchos param.

# Modelo 21
modeloBackAIC_todo<-step(full, scope=list(lower=null, upper=fulltodo), direction="backward")
Rsq(modeloBackAIC_todo,"varObjCont",data_test) # Mejora mucho
modeloBackAIC_todo$rank #muchos param.

# Modelo 22 

modeloForwkAIC_todo<-step(null, scope=list(lower=null, upper=fulltodo), trace = F, direction="forward")
Rsq(modeloForwkAIC_todo,"varObjCont",data_test) # Mejora mucho
modeloForwkAIC_todo$rank #muchos param.

# Modelos BIC
# Modelo 23
modeloStepBIC_todo<-step(null, scope=list(lower=null, upper=fulltodo), trace = F, direction="both",k=log(nrow(data_train)))
Rsq(modeloStepBIC_todo,"varObjCont",data_test) # un pelin peor
modeloStepBIC_todo$rank #pero casi la mitad de par?metros


# Modelo 24
modeloBackBIC_todo<-step(full, scope=list(lower=null, upper=fulltodo), trace = F, direction="backward",k=log(nrow(data_train)))
Rsq(modeloBackBIC_todo,"varObjCont",data_test)
modeloBackBIC_todo$rank 

# Modelo 25
modeloForwBIC_todo<-step(null, scope=list(lower=null, upper=fulltodo), trace = F, direction="forward",k=log(nrow(data_train)))
Rsq(modeloForwBIC_todo,"varObjCont",data_test) 
modeloForwBIC_todo$rank 


#Pruebo con todas y las interacciones, pueden resultar modelos sobreajustados, sobretodo con AIC
#pruebo solo BIC
fullIntT<-lm(varObjCont~.^2, data=data_train)


# Modelo 26 Stepwise
modeloStepBIC_todoInt<-step(null, scope=list(lower=null, upper=fullIntT), direction="both",trace =F, k=log(nrow(data_train)))
Rsq(modeloStepBIC_todoInt,"varObjCont",data_test) 
modeloStepBIC_todoInt$rank 

# Modelo 27 Backwise
modeloBackBIC_todoInt<-step(full, scope=list(lower=null, upper=fullIntT), direction="backward",trace = F, k=log(nrow(data_train)))
Rsq(modeloBackBIC_todoInt,"varObjCont",data_test) 
modeloBackBIC_todoInt$rank 

# Modelo 28 Forward
modeloForwBIC_todoInt<-step(null, scope=list(lower=null, upper=fullIntT), direction="forward",trace = F, k=log(nrow(data_train)))
Rsq(modeloForwBIC_todoInt, "varObjCont",data_test)
modeloForwBIC_todoInt$rank


# Modelo 29 AIC Forward

modeloForwkAIC_todoInt<-step(null, scope=list(lower=null, upper=fulltodo), trace = F, direction="forward")
Rsq(modeloForwkAIC_todoInt,"varObjCont",data_test) # Mejora mucho
modeloForwkAIC_todoInt$rank #muchos param.

# Modelo 30 AIC Back
modeloBackAIC_todoInt<-step(full, scope=list(lower=null, upper=fulltodo), trace = F, direction="backward")
Rsq(modeloBackAIC_todoInt,"varObjCont",data_test) # Mejora mucho
modeloBackAIC_todoInt$rank #muchos param.

# Modelo 31 AIC Both
modeloStepAIC_todoInt<-step(null, scope=list(lower=null, upper=fulltodo), trace = F, direction="both")
Rsq(modeloStepAIC_todoInt,"varObjCont",data_test) # Mejora mucho
modeloStepAIC_todoInt$rank #muchos param.


#variables <- data.frame(ls())
#write.csv(variables, file="variables.csv", row.names = FALSE)


modelos<-list(modeloManual, 
              modeloStepAIC, modeloBackAIC,modeloForwAIC, 
              modeloStepBIC, modeloBackBIC, modeloForwBIC,
              modeloBackAIC_int, modeloStepBIC_int, modeloBackBIC_int,
              modeloStepAIC_trans, modeloBackAIC_trans, modeloForwAIC_trans,
              modeloStepBIC_trans, modeloBackBIC_trans, modeloForwBIC_trans,
              modeloStepAIC_todo, modeloBackAIC_todo, modeloForwkAIC_todo,
              modeloStepBIC_todo, modeloBackBIC_todo, modeloForwBIC_todo,
              modeloStepBIC_todoInt, modeloBackBIC_todoInt, modeloForwBIC_todoInt,
              modeloForwkAIC_todoInt, modeloBackAIC_todoInt, modeloStepAIC_todoInt)



ranking <- sapply(modelos,function(x) x$rank)
rtest <- sapply(modelos,function(x) Rsq(x,"varObjCont",data_test))
rtrain <- sapply(modelos,function(x) Rsq(x,"varObjCont",data_train))

exceldata <- read_excel("modelos.xlsx")    
df <- data.frame(exceldata, ranking, rtest, rtrain)
df <- rownames_to_column(df, var="Modelo")

write_xlsx(df,"tabla.xlsx")

knitr::kable(df,caption = "Comparacion de modelos de regresion lineal", align = "ccccccc",
      format = 'pipe') 

## Pruebo los modelos con validacion cruzada repetida 
vcrTodosModelos<-list()
formulaModelos<-sapply(modelos,formula)
for (i in 1:length(modelos)){
  set.seed(1712)
  vcr<-train(as.formula(formulaModelos[[i]]), data = data_train,
             method = "lm",
             trControl = trainControl(method="repeatedcv", number=5, repeats=20)
  )
  vcrTodosModelos[[i]]<-vcr
}
names(vcrTodosModelos)<-paste0("Model",1:length(modelos),
                               "_",sapply(modelos,function(x) x$rank))
bwplot(resamples(vcrTodosModelos),metric=c("Rsquared"))
summary(resamples(vcrTodosModelos),metric=c("Rsquared"))

coef(modeloStepBIC_trans)
par(oma=c(3,10,3,3)) # all sides have 3 lines of space
par(mar=c(5,4,4,2) + 0.1)
importanciaVariables(modeloStepBIC_trans)
summary(modeloStepBIC_trans)

