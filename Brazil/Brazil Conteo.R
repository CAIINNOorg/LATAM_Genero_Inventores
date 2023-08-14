library(dplyr)
library(readxl)
library(stringr)
library(genero)
library(openxlsx)

datos<-read.xlsx(file.choose())
wipo<-read.csv("WIPO.csv")

wipo$gender <- gsub("M", "male", wipo$gender) 
wipo$gender <- gsub("F", "female", wipo$gender)

limpieza<-function(columna) {
  columna <- iconv(columna, to = "ASCII//TRANSLIT") #Mantiene los nombres sin caracteres especiales
  col<-(str_count(columna,"/")+1) #Guardamos el numero total de nombres
  limpio<-data.frame(str_split_fixed(columna,"/",max(col,na.rm = TRUE))) #Dividimos la base de datos entre los distintos nombres
  
  separar<-function(x){
    ifelse(is.na(x),"Missing", str_extract(x, "\\b\\w+\\b"))
  } #Nos permitirá extraer el primer nombre en caso de querer evaluar su genero
  
  limpio<-data.frame(sapply(limpio,FUN=separar))
  limpio[is.na(limpio)]<-"Missing" #Reemplazamos NA con Missing
  
  return(limpio)
}
contar_nombres <- function(df) {
  Total <- apply(df, 1, function(row) {
    nombres <- row[row != "Missing"]  # Obtener solo los nombres en la fila
    num_nombres <- length(nombres)    # Contar el número de nombres
    return(num_nombres)               # Devolver el número de nombres
  })
  return(Total)
}

#Aplicamos la funcinon LIMPIEZA a las columnas de Nombres de inventoras/es
mujeres<-limpieza(datos$Nombres.de.inventoras) 
hombres<-limpieza(datos$Nombres.de.inventores)  

#Aplicamos la columna CONTAR_NOMBRES y la reemplazamos directamente en la base de datos principal
datos$Mujeres<-contar_nombres(mujeres)
datos$Hombres<-contar_nombres(hombres)

#Nos basamos en las columnas de conteo para poder decidir si la patente es mixta/hombres o Mujeres
for (i in 1:nrow(datos)) {
  if (datos$Hombres[i] == 0) {
    datos$mixto[i] <- 0
    datos$Solo.Hombres[i] <- 0
    datos$Solo.Mujeres[i] <- 1
  } else if (datos$Mujeres[i] == 0) {
    datos$mixto[i] <- 0
    datos$Solo.Hombres[i] <- 1
    datos$Solo.Mujeres[i] <- 0
  } else {
    datos$mixto[i] <- 1
    datos$Solo.Hombres[i] <- 0
    datos$Solo.Mujeres[i] <- 0
  }
}
  

write.xlsx(datos, "Brazil_conteo.xlsx")



