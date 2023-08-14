library(dplyr)
library(tidyverse)
library(readxl)
library(stringr)
library(genero)
library(openxlsx)


# Carga de datos

CH2017<-read_excel(file.choose())

wipo<-read.csv("../WIPO.csv")
wipo$gender <- gsub("M", "male", wipo$gender) 
wipo$gender <- gsub("F", "female", wipo$gender)


# ------------------------------------------------------------------------------------------------------------------------
## Funciones para procesar los nombres y HOMOLOGARLOS
# ------------------------------------------------------------------------------------------------------------------------

### Función para convertir a mayúsculas todos los valores de una columna específica
convertir_a_mayusculas <- function(df, columna) {
  df[[columna]] <- toupper(df[[columna]])
  return(df)
}

### Función para agregar ';' al final de cada patrón encontrado en una columna específica
agregar_punto_y_coma <- function(df, columna, patron) {
  df[[columna]] <- str_replace_all(df[[columna]], patron, "\\1;")
  return(df)
}

### Función para acomodar los nombres completos en cada celda de una columna específica
separar_nombres_completos <- function(df, columna) {
  df[[columna]] <- sapply(df[[columna]], function(x) {
    if (grepl(";", x)) {
      nombres <- unlist(strsplit(x, ";"))
      nombres <- sapply(nombres, function(nombre) {
        if (grepl(",", nombre)) {
          partes <- unlist(strsplit(nombre, ","))
          nombre <- paste0(partes[2], " ", partes[1])
        }
        return(nombre)
      })
      return(paste(nombres, collapse = "; "))
    } else {
      if (grepl(",", x)) {
        partes <- unlist(strsplit(x, ","))
        x <- paste0(partes[2], " ", partes[1])
      }
      return(x)
    }
  })
  return(df)
}

###Función para dejar solo los que digan (CL)
procesar_columna <- function(dataframe, nombre_columna, datos_a_mantener) {
  columna <- dataframe[[nombre_columna]]
  nueva_columna <- rep(NA, length(columna))
  
  for (i in 1:length(columna)) {
    celda <- columna[i]
    datos <- strsplit(celda, ";")[[1]]
    
    if (length(datos) > 1) {
      nuevos_datos <- sapply(datos, function(dato) {
        if (grepl(datos_a_mantener, dato)) {
          dato
        } else {
          NA
        }
      })
      
      nuevos_datos <- nuevos_datos[!is.na(nuevos_datos)]
      if (length(nuevos_datos) > 0) {
        nueva_columna[i] <- paste(nuevos_datos, collapse = ";")
      }
    } else {
      if (grepl(datos_a_mantener, celda)) {
        nueva_columna[i] <- celda
      }
    }
  }
  
  nombre_nueva_columna <- paste(nombre_columna, "Nueva", sep = "_")
  dataframe[[nombre_nueva_columna]] <- nueva_columna
  
  return(dataframe)
}

# ------------------------------------------------------------------------------------------------------------------------
## Funciones para clasificar nombres por géneros y sacar conteos
# ------------------------------------------------------------------------------------------------------------------------

### Re homologación para clasificación
limpieza_nombre <- function(columna) {
  columna <- toupper(columna)
  
  columna <- gsub("[Á]", "A", columna)
  columna <- gsub("[É]", "E", columna)
  columna <- gsub("[Í]", "I", columna)
  columna <- gsub("[Ó]", "O", columna)
  columna <- gsub("[Ø]", "O", columna)
  columna <- gsub("[Ú]", "U", columna)
  
  col <- (str_count(columna, "\\(CL\\)|;") + 1)
  columna <- gsub("\\(CL\\)", "", columna)
  columna <- gsub("(?<!\\w)BR(?=\\.(CL|DE|ES|MX|IN|AR|NO|KR|BR|US|CO|NL|CU|CA|AU|CN)|\\b)|
                  BR\\.(DE|ES|CL|MX|IN|AR|NO|KR|BR|US)|\\.BR\\.(CL|ES|DE|MX|IN|AR|NO|KR|BR|US|CO|NL|CU|CA|AU|CN)|
                  \\.BR\\.(CL|ES|DE|MX|IN|AR|NO|KR|BR|US|CO|NL|CU|CA|AU|CN)\\.|
                  \\(AR\\)|\\(ES\\)|\\(CL\\)|\\(MX\\)|\\(IN\\)|\\(NO\\)|\\(KR\\)|\\(BR\\)|\\(US\\)|\\(CO\\)|\\(NL\\)|\\(CU\\)|\\(CA\\)|\\(AU\\)|\\(CN\\)", "", columna, perl = TRUE)
  nueva <- data.frame(str_split_fixed(columna, ";", max(col, na.rm = TRUE)))
  
  for (i in 1:ncol(nueva)) {
    nueva[, i] <- trimws(gsub("\\.", "", nueva[, i]))
    nueva[, i] <- str_extract(nueva[, i], "\\w+")
  }
  
  nueva[is.na(nueva)] <- "Missing"
  return(nueva)
}

### Asignación de género según base de datos WIPO
generoWIPO<- function(nombre) {
  indice <- match(nombre, wipo$name)
  resultado <- ifelse(nombre %in% c("GUADALUPE", "YUNUEN", "TAYDE", "ROSARIO", "ALYED"), "neutral", NA)
  resultado[!is.na(indice)] <- wipo$gender[indice[!is.na(indice)]]
  resultado<-data.frame(resultado)
  return(resultado)
}

### Dejar género único
generossni <- function(col1, col2) {
  resultado <- vector("character", length = length(col1))
  for (i in 1:length(col1)) {
    if (col1[i]=="Missing" & col2[i]=="Missing") {
      resultado[i] <- NA
    }
    else if (col2[i]=="Missing") {
      resultado[i] <- as.character(col1[i])
    }
    else if (col1[i]=="Missing") {
      resultado[i] <- as.character(col2[i])
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
}

### Conteo de hombres
contar_hombres <- function(df) {
  Total <- apply(df, 1, function(row) {
    nombres <- row[row == "male"]  # Obtener solo los nombres en la fila
    num_nombres <- length(nombres)    # Contar el número de nombres
    return(num_nombres)               # Devolver el número de nombres
  })
  return(Total)
}

### conteo de mujeres
contar_mujeres <- function(df) {
  Total <- apply(df, 1, function(row) {
    nombres <- row[row == "female"]  # Obtener solo los nombres en la fila
    num_nombres <- length(nombres)    # Contar el número de nombres
    return(num_nombres)               # Devolver el número de nombres
  })
  return(Total)
}

...
# Ejecución
...

# ------------------------------------------------------------------------------------------------------------------------
## BASE DE 2017
# ------------------------------------------------------------------------------------------------------------------------

### Paso 0: crear columna a tratar
CH2017$Inventores <- CH2017$Inventors #se duplica columna de invetores

### Paso 1: Convertir a mayúsculas
CH2017 <- convertir_a_mayusculas(CH2017, "Inventores")

### Paso 2: Agregar ';' después de cada patrón encontrado
patron <- "(BR\\.\\(DE\\)|BR\\.\\(ES\\)|BR\\.\\(CL\\)|BR\\.\\(AR\\)|BR\\.\\(NO\\)|BR\\.\\(IN\\)|BR\\.\\(MX\\))|BR\\.\\(KR\\)|BR\\.\\(BR\\)|BR\\.\\(US\\)|BR\\.\\(CO\\)|BR\\.\\(NL\\)|BR\\.\\(CU\\)|BR\\.\\(CA\\)|BR\\.\\(AU\\)|BR\\.\\(CN\\)"
CH2017 <- agregar_punto_y_coma(CH2017, "Inventores", patron)

### Paso 3: ordenar los nombres completos
CH2017 <- separar_nombres_completos(CH2017, "Inventores")

### Paso 4: se dejan solo chilenos
# CH2017 <- procesar_columna(CH2017, "Inventores", "\\(ES\\)|\\(DE\\)")
# esta línea es para excluir a los inventores que no sean chilenos, es decir que no tengan el código CL

### Paso 5: se limpian nombres para asignación de género
nombres<-limpieza_nombre(CH2017$Inventores)

### Paso 6: Aplicamos la funcion "generoWIPO" 
wipo_gen<-data.frame(sapply(nombres, generoWIPO))
colnames(wipo_gen)<-paste("GenW_",1:ncol(nombres)) #Para un mejor entendimiento cambiamos los nombres de las columnas
wipo_gen[is.na(wipo_gen)]<-"Missing" #Cambiamos los NA's por Missing para poder aplicar las funciones después

### Paso 7: Aplicamos el paquete de género de la libreria genero
gen_results<-data.frame(lapply(nombres,genero))
colnames(gen_results)<-paste("GenG_",1:ncol(nombres)) #Para un mejor entendimiento cambiamos los nombres de las columnas
gen_results[is.na(gen_results)]<-"Missing" #Cambiamos los NA's por Missing para poder aplicar las funciones después

### Paso 8: Creamos un nuevo df donde se encontrará el genero final basándose en el diagnóstico de la función de WIPo y la librería de género
genero<-data.frame(mapply(generossni,wipo_gen,gen_results)) 

### Paso 9: limpieza Manual, se hace confirmación manual
genero[187,1]<-"female" #porque la persona es ROSARIO

### Paso 10: asignación del conteo de hombres y mujeres al df original
genero[is.na(genero)]<-""

CH2017$Mujeres<-contar_mujeres(genero)
CH2017$Hombres<-contar_hombres(genero)

CH2021$mixto <- 0
CH2021$Solo.Hombres <- 0
CH2021$Solo.Mujeres <- 0

for (i in 1:nrow(CH2017)) {
  if (CH2017$Hombres[i] == 0) {
    CH2017$mixto[i] <- 0
    CH2017$Solo.Hombres[i] <- 0
    CH2017$Solo.Mujeres[i] <- 1
  } else if (CH2017$Mujeres[i] == 0) {
    CH2017$mixto[i] <- 0
    CH2017$Solo.Hombres[i] <- 1
    CH2017$Solo.Mujeres[i] <- 0
  } else {
    CH2017$mixto[i] <- 1
    CH2017$Solo.Hombres[i] <- 0
    CH2017$Solo.Mujeres[i] <- 0
  }
} 

### Paso 11: creación columnas extras
##### Generar y poner columna de fecha
CH2017$Total<-NULL
CH2017$Anio<-2017

##### Generar y poner columna con total de inventores
CH2017$Total<-CH2017$Hombres+CH2017$Mujeres

### Paso 12: validación de conteo
contar_valores_distintos <- function(fila) {
  longitud <- length(unique(fila[fila != "Missing"]))
  return(longitud)
}

nombres$Total<-apply(nombres, 1, contar_valores_distintos)

comparar <- data.frame(matrix(nrow = nrow(CH2017)))
comparar$oficial<-nombres$Total
comparar$codigo<-CH2017$Total
comparar[,1]<-NULL
comparar$comparacion <- ifelse(comparar$oficial==comparar$codigo,TRUE,FALSE)
comparar$Numero<-CH2017$ApplicationNumber
compararf<-comparar%>%filter(comparacion==FALSE)

### VOLVER XLSX
# write.xlsx(CH2017,"2017_conteo.xlsx")