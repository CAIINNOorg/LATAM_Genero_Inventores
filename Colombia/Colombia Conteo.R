library(dplyr)
library(readxl)
library(stringr)
library(genero)
library(openxlsx)

datos<-read_excel(file.choose())
wipo<-read.csv("WIPO.csv")

#Filtramos para tener unicamente la columna de países
datos<-datos%>%filter(Pais=="CO") 

wipo$gender <- gsub("M", "male", wipo$gender) 
wipo$gender <- gsub("F", "female", wipo$gender)

limpieza<-function(columna) {
  columna <- iconv(columna, to = "ASCII//TRANSLIT") #Mantiene los nombres sin caracteres especiales
  col<-(str_count(columna,";")+1) #Guardamos el numero total de nombres
  limpio<-data.frame(str_split_fixed(columna,";",max(col,na.rm = TRUE))) #Dividimos la base de datos entre los distintos nombres
  
  separar<-function(x){
    ifelse(is.na(x),"Missing", str_extract(x, "\\b\\w+\\b"))
  } #Nos permitirá extraer el primer nombre en caso de querer evaluar su genero
  
  limpio<-data.frame(sapply(limpio,FUN=separar))
  limpio[is.na(limpio)]<-"Missing" #Reemplazamos NA con Missing
  
  return(limpio)
} #Nos permite limpiar y separar los nombres en diferentes columnas

generoWIPO<- function(nombre) {
  indice <- match(nombre, wipo$name)
  resultado <- ifelse(nombre %in% c("GUADALUPE", "YUNUEN", "TAYDE", "ROSARIO", "ALYED"), "neutral", NA)
  resultado[!is.na(indice)] <- wipo$gender[indice[!is.na(indice)]]
  resultado<-data.frame(resultado)
  return(resultado)
} #Asignación de género según base de datos WIPO

generossni <- function(col1, col2) {
  resultado <- vector("character", length = length(col1))
  for (i in 1:length(col1)) {
    if (is.na(col1[i]) & is.na(col2[i])) {
      resultado[i] <- NA
    }
    else if (is.na(col2[i])) {
      resultado[i] <- as.character(col1[i])
    }
    else if (col1[i] == col2[i]) {
      resultado[i] <- as.character(col1[i])
    }
    else if (col1[i] == "neutral") {
      resultado[i] <- as.character(col1[i])
    }
    else {
      resultado[i] <- "checar"
    }
  }
  return(resultado)
} #Permitirá quedarnos con un único genero

contar_hombres <- function(df) {
  Total <- apply(df, 1, function(row) {
    nombres <- row[row == "male"]  # Obtener solo los nombres en la fila
    num_nombres <- length(nombres)    # Contar el número de nombres
    return(num_nombres)               # Devolver el número de nombres
  })
  return(Total)
} #Permite contar cuantos hombres existen 

contar_mujeres <- function(df) {
  Total <- apply(df, 1, function(row) {
    nombres <- row[row == "female"]  # Obtener solo los nombres en la fila
    num_nombres <- length(nombres)    # Contar el número de nombres
    return(num_nombres)               # Devolver el número de nombres
  })
  return(Total)
} #Permite contar cuantas mujeres existen

limpieza_1<-function(columna) {
  columna <- iconv(columna, to = "ASCII//TRANSLIT") #Mantiene los nombres sin caracteres especiales
  col<-(str_count(columna,";")+1) #Guardamos el numero total de nombres
  limpio<-data.frame(str_split_fixed(columna,";",max(col,na.rm = TRUE)))
  return(limpio)
} #Nos permitirá limpiar los nombres para poder separarlos en dos columnas por genero

pegar_nombres <- function(fila) {
  nombres <- na.omit(unlist(fila))  # Omitir los valores NA y convertir a vector
  paste(nombres, collapse = ";")  # Pegar los nombres separados por ";"
} #Permite unir los nombres en una sola columna


#Limpiamos la base de datos y la agregamos en una nueva base de datos
nombres<-limpieza(datos$Inventor) 

#Aplicamos la funcion "generoWIPO" 
wipo_gen<-data.frame(sapply(nombres, generoWIPO))
colnames(wipo_gen)<-paste("GenW_",1:25) #Para un mejor entendimiento cambiamos los nombres de las columnas
wipo_gen[is.na(wipo_gen)]<-"Missing" #Cambiamos los NA's por Missing para poder aplicar las funciones después

#Aplicamos la funcion genero de la libreria genero
gen_results<-data.frame(lapply(nombres,genero))
colnames(gen_results)<-paste("GenG_",1:25) #Para un mejor entendimiento cambiamos los nombres de las columnas
gen_results[is.na(gen_results)]<-"Missing" #Cambiamos los NA's por Missing para poder aplicar las funciones después

#Creamos un nuevo df donde se encontrará el genero final
genero<-data.frame(mapply(generossni,wipo_gen,gen_results)) 

#Asignamos el conteo de hombres al df original
datos$Hombres<-contar_hombres(genero) 

#Asignamos el conteo de mujeres al df original
datos$Mujeres<-contar_mujeres(genero) 

#Clasificamos en Solo.Hombres, Solo.Mujeres o mixto
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

datos[5,17:21]<-NA # La fila 5 no tiene nombre de inventor por lo que se queda como NA para no generar confusiones

#Extraemos la base de datos 


# Separamos los nombres por genero
  
nombres_nuevos<-limpieza_1(datos$Inventor)

female <- nombres_nuevos
female[genero != "female"] <- NA
male <- nombres_nuevos
male[genero != "male"] <- NA

female2<-na.omit(female)

female$Todo<-paste


datos$Nombres.de.inventoras<-apply(female, 1, pegar_nombres)
datos$Nombres.de.inventores<-apply(male, 1, pegar_nombres)

write.xlsx(datos,"Colombia_conteo.xlsx")
