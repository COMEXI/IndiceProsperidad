#######################################################################################
#######################################################################################
################      COMEXI                      #####################################
#######################################################################################
#######################################################################################

#Rutina para la estimación del Índice de Prosperidad de México con base en los datos de los indicadores y componentes presentados en el reporte

#Paquetes de R a utilizar dentro de la rutina
library(tidyverse)
library(readxl)
library(readr)
library(foreign)
#Información integrada sobre los componentes
setwd("~/Desktop/Carlos/COMEXI/github/data")

#Datos acomodados en el mismo sentido (entre mayor valor es mejor)
data <- read_excel("datos.xlsx",1)
#Datos estandarizados
datos <- select(data,3:ncol(data)) %>% mutate_all(~(scale(.) %>% as.vector))#estandarizado


#Tabla de correlación entre los indicadores ya adaptados
cor(data[,3:ncol(data)], method = c("pearson")) %>% View()


####################################################################################
####################################################################################
### Creación de los indicadores

#Por cada indicador, se agregran los componentes que integran a cada uno. 
#Para integrar los datos se utiliza en algunos casos, el Análisis de Componentes Principales o la estimación de una tasa

####################################################################################
#1: Ingreso
indice1 <- log(exp(data$`01_log_pib_per_capita`)+exp(data$`01_log_remesas_per_capita`))

####################################################################################
#2: Cuidado al medio ambiente
#Al tener un solo componente, no hay niguna agregación

####################################################################################
#3: Afectaciones a grupos vulnerables
#Al tener un solo componente, no hay niguna agregación

####################################################################################
#4: Salud
#PCA sobre los componentes del indicador 
pca4 <- prcomp(select(data,"04_porcentaje_diabetes_mayores20","04_porcentaje_obesidad_mayores20","04_porcentaje_hipertension_mayores20","04_porcentaje_personas_cobertura_salud","04_acceso_hosp_gral"), scale.=T)
pred4 <- predict(pca4, newdata = data) %>% as.data.frame()
pred4 <- (max(pred4$PC1) - pred4$PC1) / (max(pred4$PC1) - min(pred4$PC1))#Estandarización

####################################################################################
#5: Felicidad
#PCA sobre los componentes del indicador 
pca5 <- prcomp(select(data,"05_gini","05_social_unrest","05_esperanza_vida","05_acceso_preparatorias"), scale.=T)
pred5 <- predict(pca5, newdata = data) %>% as.data.frame()
pred5 <- (max(pred5$PC1) - pred5$PC1) / (max(pred5$PC1) - min(pred5$PC1))#Estandarización


####################################################################################
#pca6: seguridad
#Uso una columna de esta matriz en Excel, screo una tasa con la suma de tasa de delitos por homicidio y en accidentes 
#uso la ultima columna que ya tiene la tasa en sentido contrario (entre mas es mejor)
delitos <- read_excel("delitos_bis.xlsx",1) 
colnames(delitos)[1] <- "cve"
delitos <- arrange(delitos,(cve))



MAT <- data.frame(ingreso = indice1, calidad_agua = data$`02_agua_porc_verde`, minorias = data$`03_minority_rights - minority rights`,salud = pred4, felicidad = pred5, seguridad = pred6)
datos <- MAT %>% mutate_all(~(scale(.) %>% as.vector))#estandarizado


####################################################################################
####################################################################################
#Estimación del Índice de Prosperidad de México
indice_comexi <- rowSums(datos) #Mismo peso a cada indicador

#Matriz de componentes con el Índice
Matriz <- cbind(select(data,c(1,2)),indice = indice_comexi,select(data,c(3,4,5,6,7:10,12:18)),select(delitos,"06_tasa_delictiva_final","06_confianza_policia_estatal","06__confianza_mp_fiscalias")) %>%
  arrange(desc(indice))

#Guardar la información
write_csv(Matriz,"IndiceProsperidad.csv")







####################################################################################
#Creación de matriz auxiliar para cálculos posteriores sobre componentes
ind <- Matriz$indice + abs(min(Matriz$indice))
vec <- c()
for(i in 1:nrow(Matriz)){
  vec <- rbind(vec,(max(ind) - ind[i]) / (max(ind)- min(ind)))
}

Matriz_bis <- cbind(select(Matriz,1:3),vec,select(Matriz,c(4:16,19:21)))
write_csv(Matriz_bis,"Matriz_bis.csv")


####################################################################################
#Creación de matriz auxiliar para cálculos posteriores sobre indicadores
MAT <- data.frame(ingreso = indice1, calidad_agua = data$`02_agua_porc_verde`, minorias = data$`03_minority_rights - minority rights`,salud = pred4, felicidad = pred5, seguridad = pred6)
write_csv(MAT,"MAT.csv")

