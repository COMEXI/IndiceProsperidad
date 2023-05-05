#######################################################################################
#######################################################################################
################      COMEXI                      #####################################
#######################################################################################
#######################################################################################

#Rutina para las creación de tablas de posición sobre los componentes de cada estado 

#Paquetes de R a utilizar dentro de la rutina
library(tidyverse)
library(readxl)
library(readr)
library(foreign)

#Información integrada sobre los componentes
setwd("~/Desktop/Carlos/COMEXI/github/data")
data <- read_excel("datos.xlsx",1)
Matriz_bis <- read_csv("Matriz_bis.csv")


#Componentes
nueva_MAT <- cbind(select(data,c(1,2)), arrange(Matriz_bis,cve_estado) %>% select(5:ncol(Matriz_bis)) )
edos <- nueva_MAT$estado

#Primer estado
k <- 1
vec <- which(nueva_MAT$estado == edos[k])

aux <- matrix(0,ncol(nueva_MAT)-2,2) %>% data.frame()
colnames(aux) <- c("indicador","lugar")

aux$indicador <- colnames(nueva_MAT)[3:ncol(nueva_MAT)]

for(i in 3:ncol(nueva_MAT)){
  aux$lugar[i-2] <- (which(nueva_MAT[,i] >   nueva_MAT[vec,i]) %>% length()) + 1
}
#Colocamos el nombre del estado
colnames(aux)[2] <- edos[k]
A <- aux


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

Componentes <- A

#Guardamos el lugar que cada estado tiene por componente
write_excel_csv(Componentes,"Componentes.csv")

