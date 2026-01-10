library(dplyr)
library(ggplot2)

funcion_121 <- function(html){
  lineas <- readLines(html,
                      encoding = "UTF-8",
                      warn = FALSE)
  
  linea_inicio <- grep("^[^0-9\t\n]+\t[0-9]+\t[0-9]+", lineas)[1]
  
  datos <- lineas[linea_inicio:(linea_inicio + 9999)]
  
  partes <- strsplit(datos, "\t")
  
  formas <- sapply(partes, function(x) x[1])
  frecuencias <- as.numeric(sapply(partes, function(x) x[2]))
  frec_norm <- as.numeric(sapply(partes, function(x) x[3]))
  
  tabla <- data.frame(Forma = formas,
                      Frecuencia = frecuencias,
                      Frec.norm = frec_norm,
                      stringsAsFactors = FALSE)
  funcion_122(tabla)
}

funcion_122 <- function(tabla){
  tabla$min_forma <- tolower(tabla$Forma)
  
  conteo <- table(tolower(tabla$Forma))
  duplicados <- conteo[conteo > 1]
  
  cat("Formas básicas con duplicados no exactos:", length(duplicados), "\n")
  for (i in 1:5) {
    forma <- names(duplicados)[i]
    variantes <- unique(tabla$Forma[tolower(tabla$Forma) == forma])
    cat(i, " '", forma, "' con las varientes: ", paste(variantes, collapse = ", "), "\n", sep = "")
  }
  
}