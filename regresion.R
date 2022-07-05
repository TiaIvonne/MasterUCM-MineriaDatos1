setwd("/Users/ivonne.mendoza/Desktop/MasterUCM/Semana7/Documentación minería de datos y modelización predictiva-20220502/evaluacion")


# Cargo las funciones que voy a utilizar después
source("FuncionesMineriaAida.R")

# Cargo las librerias que me van a hacer falta
library(questionr)
library(car)
library(Hmisc)
library(corrplot)
library(ggplot2)
library(tidyr)
library(caret)
library(grid)
library(gridExtra)



# Parto de los datos sin atípicos ni ausentes guardados
datos<-readRDS("datosEleccionesDep")

str(datos)
varObjCont<-datos$varObjCont
input<-datos[,-(1)]


# Creo la variable aleatoria
set.seed(987654)
input$aleatorio<-runif(nrow(input))
input$aleatorio2<-runif(nrow(input))


#Evalúo el V de cramer con respecto 

par(oma=c(10,3,3,3)) # all sides have 3 lines of space
par(mar=c(5,4,4,2) + 0.1)
#Evalúo el V de cramer con respecto a Porcentaje
graficoVcramer(input,varObjCont) #prop_missing y M_ están por debajo de las aleatorias
#las más importantes son PartidoCCAA, CCAA, UnemployementPtge, AutonomosPtge

#Veo gráficamente el efecto de los dos factores más importantes
#Cualitativas
#Variable objetivo cuantitativa e input de tipo factor


plota <- boxplot_cuantcuali(varObjCont, input$PartidoCCAA, "PartidoCCAA")
plotb <- boxplot_cuantcuali(varObjCont, input$CCAA, "CCAA")
plota
plotb

#Todas las variables numéricas frente a la objetivo continua
png(file="/Users/ivonne.mendoza/Desktop/MasterUCM/Semana7/Documentación minería de datos y modelización predictiva-20220502/evaluacion/corrplot.png")
corrplot(cor(data.frame(varObjCont,Filter(is.numeric, input)), use="pairwise", method="pearson"), method = "ellipse",type = "upper")
dev.off()
png(file="/Users/ivonne.mendoza/Desktop/MasterUCM/Semana7/Documentación minería de datos y modelización predictiva-20220502/evaluacion/dispersion.png")
dispersion(Filter(is.numeric, input),varObjCont)
dev.off()




# Se guarda un archivo que incluya los aleatorios
saveRDS(data.frame(varObjCont, input), "datosEleccionesDep2")

# ##### regresion lineal

# Unir los input con la variable objetivo
todo <- data.frame(input, varObjCont)
head(todo)


# Se realiza la particion train-test
set.seed(2701)
trainIndex <- createDataPartition(todo$varObjCont, p=0.8, list=FALSE)
data_train <- todo[trainIndex,]
data_test <- todo[-trainIndex,]

#Primer modelo manual con TODAS LAS VARIABLES
par(oma=c(3,10,3,3)) # all sides have 3 lines of space
par(mar=c(5,4,4,2) + 0.1)


t1 <- proc.time()
modelo1<-lm(varObjCont~.,data=data_train)
print(proc.time() - t1)
summary(modelo1)
modelo1$rank #vemos el número de parámetros que tiene

Rsq(modelo1,"varObjCont",data_train)
Rsq(modelo1,"varObjCont",data_test) 

importanciaVariables(modelo1)



#Pruebo un modelo con menos variables, eliminando las que son similares a las aleatorias
modelo2<-lm(varObjCont~PartidoCCAA + CCAA + AutonomosPtge + UnemploymentPtge + 
              AgricultureUnemploymentPtge + Age_over65_Ptge + Densidad + UniversityPtge +
              ForeignersPtge + Explotaciones + PersonasInmueble + WomanPopulationPtge, data = 
              data_train)
summary(modelo2)
          
modelo2$rank
Rsq(modelo2,"varObjCont",data_train)
Rsq(modelo2,"varObjCont",data_test) 
importanciaVariables(modelo2) 


#Otro modelo
modelo3<-lm(varObjCont~PartidoCCAA + CCAA + AutonomosPtge + UnemploymentPtge + 
              Age_over65_Ptge + AgricultureUnemploymentPtge + Explotaciones, 
            data = data_train)

modelo3$rank
Rsq(modelo3,"varObjCont",data_train)
Rsq(modelo3,"varObjCont",data_test) 
importanciaVariables(modelo3) 

#Modelo agresivo solo con cuatro variables
modelo4<-lm(varObjCont~PartidoCCAA + CCAA + AutonomosPtge + UnemploymentPtge, data = data_train)
modelo4$rank
Rsq(modelo4,"varObjCont",data_train)
Rsq(modelo4,"varObjCont",data_test) 
importanciaVariables(modelo4) 


#Modelo con interacciones
# Las interacciones son de tipo cuali-cuanti o cuali cuali

modelo5<-lm(varObjCont~PartidoCCAA + CCAA + AutonomosPtge + UnemploymentPtge +
              PartidoCCAA:UnemploymentPtge, data = data_train)

modelo5$rank
summary(modelo5)
Rsq(modelo5,"varObjCont",data_train)
Rsq(modelo5,"varObjCont",data_test) 
importanciaVariables(modelo5) 


# Otro
modelo6 <- lm(varObjCont~PartidoCCAA + CCAA + AutonomosPtge + UnemploymentPtge+ 
              PartidoCCAA:AutonomosPtge, data = data_train)

modelo6$rank
Rsq(modelo6,"varObjCont",data_train)
Rsq(modelo6,"varObjCont",data_test) 
importanciaVariables(modelo6) 



# Comparacion de modelos
modelos <- list(modelo1, modelo2, modelo3, modelo4, modelo5, modelo6)
ranking <- sapply(modelos, function(x) x$rank)
rTrain <- sapply(modelos,function(x) Rsq(x,"varObjCont",data_train))
rTest <- sapply(modelos,function(x) Rsq(x,"varObjCont",data_test))

df <- data.frame( Modelo = c("Modelo1", "Modelo2", "Modelo3", "Modelo4", "Modelo5", "Modelo6") ,
                Rank = c(ranking),
                 rTrain = c(rTrain),
                 rTest = c(rTest))

colnames(df) <- c("Modelo", "Rank", "r2 Train", "r2 Test")


knitr::kable(df,caption = "Comparacion de modelos de regresion lineal", align = "ccccccc",
             format = 'pipe') 

train <- c(rTrain)
test <- c(rTest)
diferencias <- train-test
diferencias

# Modelo4 por simplicidad

coef(modelo4) 

# Estabilidad del modelo
Rsq(modelo4,"varObjCont",data_train)
Rsq(modelo4,"varObjCont",data_test) 


# Variables mas importantes del modelo
importanciaVariables(modelo4)

summary(modelo4)

