datos <- read.csv2("bank-additional-full.csv")
# summary(datos)

# Contar la cantidad de veces que aparece "unknown" en cada columna
apply(datos, 2, function(x) sum(x == "unknown", na.rm = TRUE))

