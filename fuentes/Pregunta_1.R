# Para limpiar
rm(list = ls())

lineas <- readLines("datos/10000_formas_ortograficas.txt", 
                   encoding = "UTF-8")
lineas[0:4]
linea_inicio <- grep("^[^0-9\t\n]+\t[0-9]+\t[0-9]+", lineas)[1]

datos <- lineas[linea_inicio:(linea_inicio + 9999)]

partes <- strsplit(datos, "\t")
partes[0:4]
formas <- sapply(partes, function(x) x[1])
frecuencias <- as.numeric(sapply(partes, function(x) x[2]))
frec_norm <- as.numeric(sapply(partes, function(x) x[3]))

# Crear el dataframe 
tabla <- data.frame(Forma = formas,
                    Frecuencia = frecuencias,
                    Frec.norm = frec_norm,
                    stringsAsFactors = FALSE)
tabla$Forma <- trimws(tabla$Forma)
head(tabla, 5)
tail(tabla, 5)
```

## APARTADO 1
# Creamos la columna de rangos
tabla$rango <- 1:nrow(tabla)

# Gráfico log-log
plot(log10(tabla$rango),
     log10(tabla$Frecuencia),
     pch = 20, cex = 0.5,
     xlab = "log10(Rango)",
     ylab = "log10(Frecuencia)",
     main = "Ley de Zipf: Frecuencia vs Rango (escala log-log)")

# Ajuste lineal para visualizar la tendencia
modelo <- lm(log10(Frecuencia) ~ log10(rango), data = tabla)
abline(modelo, col = "red", lwd = 2)

cat("\nAjuste Zipf (lista total, log-log):\n")
print(summary(modelo)$coefficients)

# Apartado 1.1
duplicadas1 <- tabla$Forma[duplicated(tabla$Forma)]
duplicadas1 <- unique(duplicadas1)

cat("Número de formas repetidas exactas:", length(duplicadas1), "\n")

# 2) Construir lista detallada (forma + línea + frecuencia) para cada copia
if (length(duplicadas1) > 0) {

  lista_repetidas1 <- lapply(duplicadas1, function(f) {
    indices <- which(tabla$Forma == f)
    data.frame(
      Forma = f,
      Linea = indices,
      Frecuencia = tabla$Frecuencia[indices],
      stringsAsFactors = FALSE
    )
  })

  resultado1 <- do.call(rbind, lista_repetidas1)

  cat("Número total de apariciones (filas) dentro de duplicados:", nrow(resultado1), "\n\n")

  
  if (nrow(resultado1) > 20) {
    cat("Mostrando 5 primeras y 5 últimas filas de la lista de duplicados:\n\n")
    print(head(resultado1, 5))
    cat("\n...\n\n")
    print(tail(resultado1, 5))
  } else {
    print(resultado1)
  }

} else {
  cat("No hay formas repetidas exactas en este listado.\n")
}

library(stringi)
## Apartado 1.2

# Pasar todo a minúsculas
tabla$forma_base <- stri_trans_general(tolower(tabla$Forma), "Latin-ASCII")


# Contar cuántas formas básicas tienen variantes
conteo <- table(tabla$forma_base)
duplicados <- conteo[conteo > 1]

cat("Formas básicas con duplicados no exactos:", length(duplicados), "\n")

# Mostrar primeros y últimos 5 ejemplos
n_dups <- length(duplicados)

if (n_dups > 10) {
  # Bloque de los 5 primeros
  for (i in 1:5) {
    base_actual <- names(duplicados)[i]
    # Buscamos en la tabla original todas las formas que coinciden con esa base
    variantes <- unique(tabla$Forma[tabla$forma_base == base_actual])
    cat(i, " '", base_actual, "' tiene las variantes: ", paste(variantes, collapse = ", "), "\n", sep = "")
  }
  
  # Separador visual conforme a la práctica
  cat("...\n")
  
  # Bloque de los 5 últimos
  for (i in (n_dups - 4):n_dups) {
    base_actual <- names(duplicados)[i]
    variantes <- unique(tabla$Forma[tabla$forma_base == base_actual])
    cat(i, " '", base_actual, "' tiene las variantes: ", paste(variantes, collapse = ", "), "\n", sep = "")
  }
  
} else {
  # Si hay pocos resultados, se muestran todos
  for (i in 1:n_dups) {
    base_actual <- names(duplicados)[i]
    variantes <- unique(tabla$Forma[tabla$forma_base == base_actual])
    cat(i, " '", base_actual, "' tiene las variantes: ", paste(variantes, collapse = ", "), "\n", sep = "")
  }
}

# Apartado 1.3
library(stringi)
#Normalización
formas_norm <- stri_trans_nfc(tabla$Forma) # normaliza acentos
formas_norm <- gsub("\u00A0", " ", formas_norm) 
formas_norm <- gsub("\t", " ", formas_norm)

patron <- "^[A-Za-zñÑáéíóúÁÉÍÓÚüÜ0-9[:space:][:punct:]]+$"

# Marcamos como no español todo lo que NO encaje exactamente con ese patrón de letras
no_espanol <- !stri_detect_regex(formas_norm, patron)

# Conteo de resultados
num_no_espanol <- sum(no_espanol)
cat("Con la criba estricta, se han detectado", num_no_espanol, "formas no españolas.")

# Verificación de lo que HEMOS QUITADO 
cat("\nEjemplos de formas EXCLUIDAS (No españolas):\n")
head(tabla$Forma[no_espanol], 5)
tail(tabla$Forma[no_espanol], 5)

#  Verificación de lo que SE QUEDA 
cat("\nEjemplos de formas PERMITIDAS (Españolas):\n")
head(tabla$Forma[!no_espanol], 5)
tail(tabla$Forma[!no_espanol], 5)

