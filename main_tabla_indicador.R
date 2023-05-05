#######################################################################################
#######################################################################################
################      COMEXI                      #####################################
#######################################################################################
#######################################################################################

#Rutina para las creación de tablas de posición sobre los indicadores de cada estado 

#Paquetes de R a utilizar dentro de la rutina
library(tidyverse)
library(readxl)
library(readr)
library(foreign)

#Información integrada sobre los componentes
setwd("~/Desktop/Carlos/COMEXI/github/data")
data <- read_excel("datos.xlsx",1)
MAT <- read_csv("MAT.csv",1)

#Variables de la matriz
colnames(MAT) <- MAT[1,]
MAT <- MAT[2:nrow(MAT),]
#Variables numéricas
MAT$ingreso <- as.numeric(MAT$ingreso)
MAT$calidad_agua <- as.numeric(MAT$calidad_agua)
MAT$minorias <- as.numeric(MAT$minorias)
MAT$salud <- as.numeric(MAT$salud)
MAT$felicidad <- as.numeric(MAT$felicidad)
MAT$seguridad <- as.numeric(MAT$seguridad)

#######################################################################################
#Posición de cada indicador por estado

#Variables con información sobre los componentes
nueva_MAT <- cbind(select(data,c(1,2)),  MAT %>% mutate_all(~(scale(.) %>% as.vector)) )
edos <- nueva_MAT$estado#Variable con el nombre de los estados

#Primer estado
k <- 1
vec <- which(nueva_MAT$estado == edos[k])
aux <- matrix(0,ncol(nueva_MAT)-2,2) %>% data.frame()
colnames(aux) <- c("indicador","lugar")
aux$indicador <- colnames(nueva_MAT)[3:ncol(nueva_MAT)]

for(i in 3:ncol(nueva_MAT)){
  aux$lugar[i-2] <- (which(nueva_MAT[,i] >   nueva_MAT[vec,i]) %>% length()) + 1
}
colnames(aux)[2] <- edos[k]
A <- aux

#Los estados restantes

for(k in 2:length(edos)){
  vec <- which(nueva_MAT$estado == edos[k])
  
  aux <- matrix(0,ncol(nueva_MAT)-2,2) %>% data.frame()
  colnames(aux) <- c("indicador","lugar")
  
  aux$indicador <- colnames(nueva_MAT)[3:ncol(nueva_MAT)]
  
  for(i in 3:ncol(nueva_MAT)){
    aux$lugar[i-2] <- (which(nueva_MAT[,i] >   nueva_MAT[vec,i]) %>% length()) + 1
  }
  #Colocamos el nombre del estado
  colnames(aux)[2] <- edos[k]
  A <- left_join(x = A,y = aux,by = "indicador")
}

Indicador <- A

#Guardamos el lugar que cada estado tiene por indicador
write_excel_csv(Indicador,"Indicador.csv")







