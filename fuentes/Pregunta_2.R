# Para limpiar
rm(list = ls())

lineas2 <- readLines("datos/frecuencia_formas_ortograficas_1_3.txt", 
                     encoding = "UTF-8")
linea_inicio2 <- grep("^[^0-9\t\n]+\t[0-9]+\t[0-9]+", lineas2)[1]

datos2 <- lineas2[linea_inicio2:(linea_inicio2 + length(lineas2)-2)] #El -2 pq sinó aparecen 2 lineas vacias

partes2 <- strsplit(datos2, "\t")
formas2 <- sapply(partes2, function(x) x[1])
frecuencias2 <- as.numeric(sapply(partes2, function(x) x[2]))
frec_norm2 <- as.numeric(sapply(partes2, function(x) x[3]))

# Crear el dataframe 
tabla2 <- data.frame(Forma = formas2,
                     Frecuencia = frecuencias2,
                     Frec.norm = frec_norm2,
                     stringsAsFactors = FALSE)

head(tabla2, 5)
tail(tabla2, 5)

## APARTADO 1
# Creamos la columna de rangos
tabla2_ord <- tabla2[order(-tabla2$Frecuencia), ]
tabla2_ord$rango <- 1:nrow(tabla2_ord)

# Gráfico
plot(
  log10(tabla2_ord$rango),
  log10(tabla2_ord$Frecuencia),
  pch = 20, cex = 0.5,
  xlab = "log10(Rango)",
  ylab = "log10(Frecuencia absoluta)",
  main = "Ley de Zipf (Lista total): Frecuencia vs Rango (log-log)"
)

# Ajuste lineal para visualizar la recta
modelo2 <- lm(log10(Frecuencia) ~ log10(rango), data = tabla2_ord)
abline(modelo2, col = "red", lwd = 2)

cat("\nAjuste Zipf (lista total, log-log):\n")
print(summary(modelo2)$coefficients)

## Apartado 2.1

# 1) Formas duplicadas exactas (mismas letras)
duplicadas2 <- tabla2$Forma[duplicated(tabla2$Forma)]
duplicadas2 <- unique(duplicadas2)

cat("Número de formas repetidas exactas:", length(duplicadas2), "\n")

# 2) Lista detallada: forma + línea(s) + frecuencia(s)
if (length(duplicadas2) > 0) {

  lista_repetidas2 <- lapply(duplicadas2, function(f) {
    indices <- which(tabla2$Forma == f)
    data.frame(
      Forma = f,
      Linea = indices,
      Frecuencia = tabla2$Frecuencia[indices],
      stringsAsFactors = FALSE
    )
  })

  resultado2 <- do.call(rbind, lista_repetidas2)

  # Mostrar cuántas filas (apariciones totales de duplicados)
  cat("Número total de apariciones (filas) dentro de duplicados:", nrow(resultado2), "\n\n")

  # Si es muy largo, mostramos 5 primeras y 5 últimas (como pide el enunciado)
  if (nrow(resultado2) > 20) {
    cat("Mostrando 5 primeras y 5 últimas filas de la lista de duplicados:\n\n")
    print(head(resultado2, 5))
    cat("\n...\n\n")
    print(tail(resultado2, 5))
  } else {
    print(resultado2)
  }

} else {
  cat("No hay formas repetidas exactas en este listado.\n")
}

library(stringi)
## Apartado 2.2

# Pasar todo a minúsculas
tabla2$forma_base <- stri_trans_general(tolower(tabla2$Forma), "Latin-ASCII")
# Contar cuántas formas básicas tienen variantes
conteo <- table(tabla2$forma_base)
duplicados <- conteo[conteo > 1]

cat("Formas básicas con duplicados no exactos:", length(duplicados), "\n")

# Mostrar primeros y últimos 5 ejemplos
n_dups <- length(duplicados)
cat("\nMostrando formas con variantes (5 primeras y 5 últimas):\n")

if (n_dups > 10) {
  # Bloque de los 5 primeros
  for (i in 1:5) {
    base_actual <- names(duplicados)[i]
    # Buscamos en la tabla original todas las formas que coinciden con esa base
    variantes <- unique(tabla2$Forma[tabla2$forma_base == base_actual])
    cat(i, " '", base_actual, "' tiene las variantes: ", paste(variantes, collapse = ", "), "\n", sep = "")
  }
  
  # Separador visual
  cat("...\n")
  
  # Bloque de los 5 últimos
  for (i in (n_dups - 4):n_dups) {
    base_actual <- names(duplicados)[i]
    variantes <- unique(tabla2$Forma[tabla2$forma_base == base_actual])
    cat(i, " '", base_actual, "' tiene las variantes: ", paste(variantes, collapse = ", "), "\n", sep = "")
  }
  
} else {
  # Si hay pocos resultados, se muestran todos
  for (i in 1:n_dups) {
    base_actual <- names(duplicados)[i]
    variantes <- unique(tabla2$Forma[tabla2$forma_base == base_actual])
    cat(i, " '", base_actual, "' tiene las variantes: ", paste(variantes, collapse = ", "), "\n", sep = "")
  }
}

# Apartado 2.3
library(stringi)

#Normalización
formas_norm2 <- stri_trans_nfc(tabla2$Forma) # normaliza acentos
formas_norm2 <- gsub("\u00A0", " ", formas_norm2) 
formas_norm2 <- gsub("\t", " ", formas_norm2)


patron <- "^[A-Za-zñÑáéíóúÁÉÍÓÚüÜ0-9[:space:][:punct:]]+$"

# Marcamos como no español todo lo que NO encaje exactamente con ese patrón de letras
no_espanol2 <- !stri_detect_regex(formas_norm2, patron)

# Conteo de resultados
num_no_espanol2 <- sum(no_espanol2)
cat("Con la criba estricta, se han detectado", num_no_espanol2, "formas no españolas.")

# Verificación de lo que HEMOS QUITADO 
cat("\nEjemplos de formas EXCLUIDAS (No españolas):\n")
print(head(tabla2$Forma[no_espanol2], 5))
print(tail(tabla2$Forma[no_espanol2], 5))

#  Verificación de lo que SE QUEDA 
cat("\nEjemplos de formas PERMITIDAS (Españolas):\n")
print(head(tabla2$Forma[!no_espanol2], 5))
print(tail(tabla2$Forma[!no_espanol2], 5))