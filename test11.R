datos <- read.csv2("bank-additional-full.csv")
copia <- datos

#suma = integer(sum(datos$pdays != 999))

# Convertir la columna categórica en valores enteros
copia$job <- match(datos$job, unique(datos$job))

suma = integer(sum(datos$marital == "unknown"))

sumaEducation = integer(sum(datos$education == "unknown"))

sumaDefault = integer(sum(datos$default == "unknown"))

sumaHousing = integer(sum(datos$housing == "unknown"))

sumaLoan = integer(sum(datos$loan == "unknown"))

copia$contact <- match(datos$contact, unique(datos$contact))




