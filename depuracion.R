setwd("/Users/ivonne.mendoza/Desktop/MasterUCM/Semana7/Documentación minería de datos y modelización predictiva-20220502/evaluacion")

source("FuncionesMineriaAida.R")

library(questionr)
library(car)
library(Hmisc)
library(readxl)
library(corrplot)
library(ggplot2)
library(tidyr)
library(gridExtra)
library(knitr)
library(dplyr)
library(tibble)


options("scipen" = 10)
## ---- chunk-tiposVariables ----
datos <- read_excel('DatosEleccionesEuropeas2019.xlsx')

# Crea columna Porcentaje la cual es el % de votos de partidos de izquierda sobre los votos emitidos
datos$Porcentaje <- (rowSums(cbind(datos$Podemos, datos$PSOE), na.rm = T ) * 100) / datos$VotosEmitidos

# Al crear la columna esta se agrega por defecto al final del set de datos, se relocaliza
datos <- datos %>%
  relocate(Porcentaje, .before = VotosEmitidos)

# Se descartan las variables que no serán utilizadas (en excel son las variables en amarillo)
datos<-data.frame(datos[,-c(3:13)]) 

# Primera lectura de datos
str(datos)


## ---- chunk-categoricas ----
# Indicar numero de columna a convertir
datos[,c(3, 20)] <- lapply(datos[,c(3, 20)], as.factor)


## ---- chunk-numericas ----
# Cuento el n?mero de valores diferentes para las num?ricas
sapply(Filter(is.numeric, datos),function(x) length(unique(x)))
# estan todas bien representadas

## ---- chunk-analisis ----
summary(datos)

## ---- chunk-descarta ----
# Total de observaciones
nrow(datos)

# Eliminacion de variables con mas de 50% de ausentes
datos$IndustriaPtge <- NULL
datos$ConstruccionPtge <- NULL
datos$ComercTTEHosteleriaPtge <- NULL
datos$ServiciosPtge <- NULL



freq(datos$PartidoCCAA)
freq(datos$CCAA)

datos$CCAA <- car::recode(datos$CCAA, "c('Galicia', 'Asturias', 'Cantabria', 'PaísVasco', 'Rioja',
                          'Navarra') = 'Zona1';
                          c('Aragón', 'Cataluña') = 'Zona2'; 
                          c('ComValenciana', 'Baleares') = 'Zona3';
                          c('CastillaLeón', 'Extremadura') = 'Zona4';
                          c('CastillaMancha', 'Madrid') = 'Zona5';
                          c('Andalucía', 'Murcia', 'Canarias', 'Ceuta', 'Melilla') = 'Zona6'")
                        

freq(datos$CCAA)


## ---- chunk-fuera ----

# Valores fuera de rango de las variables UnemploymentPtge y AutonomosPtge

datos$UnemploymentPtge <- replace(datos$UnemploymentPtge, which(datos$UnemploymentPtge > 100), NA)
datos$AutonomosPtge <- replace(datos$AutonomosPtge, which(datos$AutonomosPtge > 100), NA)


## ---- chunk-resumen ----
summary(datos)


## ---- chunk-separa ----
varObjCont<-datos$Porcentaje
input<-data.frame(datos[,-c(1:2)]) 
row.names(input) <- datos$CodigoINE


## ---- chunk-atipicos ----

for (i in names(which(sapply(input, class)=="numeric"))){
  outliers(paste0("input$",i))
}

# Gestion de ausentes
## ---- chunk-ausentes ----
input$prop_missings <- rowMeans(is.na(input))
summary(input$prop_missings)



## ---- chunk-ausentes2 ----
# Revisar si alguna variable tiene un 50% de ausentes
(prop_missingsVars<-colMeans(is.na(input)))


## ---- chunk-imputacion ----
#Inputacion
# Imputo todas las cuantitativas, seleccionar el tipo de imputación: media (mean, sin comillas), mediana (nada) o aleatorio ("random")
input[,as.vector(which(sapply(input, class)=="numeric"))]<-sapply(Filter(is.numeric, input),function(x) impute(x,"random"))
# Imputo todas las cualitativas, seleccionar el tipo de imputación: moda  (nada) o aleatorio ("random")
input[,as.vector(which(sapply(input, class)=="factor"))]<-sapply(Filter(is.factor, input),function(x) impute(x,"random"))
# Se cambia el tipo de factor a character al imputar, así que hay que indicarle que es factor
input[,as.vector(which(sapply(input, class)=="character"))] <- lapply(input[,as.vector(which(sapply(input, class)=="character"))] , factor)

## ---- chunk-imputacion2 ----
length(unique(input$prop_missings))

input$prop_missings<-as.factor(input$prop_missings)
freq(input$prop_missings)

input$prop_missings<-car::recode(input$prop_missings, "0='Ninguno';else='Alguno'")
freq(input$prop_missings)
summary(input)
stargazer::stargazer(input,summary = TRUE, out = 'tab.txt')
## ---- chunk-imputacion5 ----
saveRDS(data.frame(varObjCont,input),"datosEleccionesDep")
