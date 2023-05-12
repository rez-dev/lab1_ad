
# Importar paquetes.
if(!require(tidyverse)){
  install.packages("tidyverse",dependencies = TRUE)
  require(tidyverse)
}

if(!require(ggpubr)){
  install.packages("ggpubr",dependencies = TRUE)
  require(ggpubr)
}

if(!require(ez)){
  install.packages("ez",dependencies = TRUE)
  require(ez)
}
if(!require(nlme)){
  install.packages("nlme",dependencies = TRUE)
  require(nlme)
}
if(!require(emmeans)){
  install.packages("emmeans",dependencies = TRUE)
  require(emmeans)
}

if(!require(ggplot2)){
  install.packages("ggplot2",dependencies = TRUE)
  require(ggplot2)
}

if (!require(boot)){
  install.packages("boot", dependencies = TRUE )
  require (boot)
}
library(dplyr)
library(tidyverse)
library(pROC)
library(caret)
library(leaps)
library(car)
library(mice)
library(MASS)
library(caret)

# Importar datos
poblacion <- read.csv2(file.choose(new = FALSE), encoding="utf8")

# Copia de la BD original
poblacion2 <- poblacion

# verificando si no hay Nan´s
sum(is.na(poblacion))

######## Reducir dimensionalidad #########################

#Eliminar la variable pdays
sum(poblacion$pdays == '999')
# Crear un nuevo data frame sin la variable "pdays"
poblacion2 <- dplyr::select(poblacion2, -pdays)


sum(poblacion$poutcome == 'nonexistent')




################ Eliminar Obs uknowns ####################

# Saber la cantidad de unkmown en un vector de cada variable

# NOTA: SON TODOS CATEÓRICOS
unknown_count <- colSums(poblacion2 == 'unknown')


# 330 en job

# 80 en marital

# 1731 en education

# 8597 default -> son bastantes, sesgaría el modelo, afecta a la precisión 

# 990 housing

# 990 en loan


######################### Ver el N, proporcion en la población y su tasa con respecto a y ######################

############# ESTE ANÁLISIS SE HIZO CON LAS VARIABLES QUE POSEEN MISINGS ###################


# Visualizar datos 
n_categorias <- length(unique(poblacion2$education))


# Graficar las barras

# Paso 1: Obtener conteos
conteos <- table(poblacion2$education)

# Paso 2: Ordenar conteos de menor a mayor
conteos_ordenados <- sort(conteos)

# Paso 3: Crear vector de colores
colores <- rainbow(length(conteos))

# Paso 4: Crear gráfico de barras
barplot(conteos_ordenados, col = colores, main = "Niveles de educación", xlab = "Tipo", ylab = "Cantidad")

# Esto sirve para la suma de 1 en base a un acategoria , ya que estos son los que me interesan para el estudio

# Un ejemplo

# datos <- data.frame(grupo = c("A", "B", "B", "A", "C", "C", "B", "C"),
                    #variable = c(2, 4, 5, 3, 6, 7, 2, 8))

# grupo variable
#1    A     2.5
#2    B     3.7
#3    C     7.0


##################
# Caso job #
##################

# Contar el número de instancias en cada categoría de la variable "job"
job_table <- table(poblacion2$job)

# Convertir la salida en un data frame
job_counts <- data.frame(job = names(job_table), count = as.numeric(job_table))

# Ordenar el data frame por el total de instancias de forma descendente
job_counts <- job_counts[order(-job_counts$count), ]

# Agregar una columna con el porcentaje de instancias para cada categoría
job_counts$percent <- job_counts$count / sum(job_counts$count) * 100

# Calcular el promedio de la variable "y" para cada categoría de la variable "job"
poblacion2$y <- ifelse(poblacion2$y == "yes", 1, 0)
y_means_e <- aggregate(poblacion2$y, by = list(poblacion2$job), FUN = mean)
names(y_means_e) <- c("job", "y_mean")

# Unir las tablas de conteo y promedio por categoría de "job"
job_counts <- merge(job_counts, y_means_e, by = "job")

# Ordenar
job_counts <- job_counts[order(job_counts$y_mean),]





##################
# Caso education #
##################

# Contar el número de instancias en cada categoría de la variable "education"
education_table <- table(poblacion2$education)

# Convertir la salida en un data frame
education_counts <- data.frame(education = names(education_table), count = as.numeric(education_table))

# Ordenar el data frame por el total de instancias de forma descendente
education_counts <- education_counts[order(-education_counts$count), ]

# Agregar una columna con el porcentaje de instancias para cada categoría
education_counts$percent <- education_counts$count / sum(education_counts$count) * 100

# Calcular el promedio de la variable "y" para cada categoría de la variable "education"
y_means_e <- aggregate(poblacion2$y, by = list(poblacion2$education), FUN = mean)
names(y_means_e) <- c("education", "y_mean")

# Unir las tablas de conteo y promedio por categoría de "education"
education_counts <- merge(education_counts, y_means_e, by = "education")

# Ordenar
education_counts <- education_counts[order(education_counts$y_mean),]


##################
# Caso default   #
################## 

# Contar el número de instancias en cada categoría de la variable "default"
default_table <- table(poblacion2$default)

# Convertir la salida en un data frame
default_counts <- data.frame(default = names(default_table), count = as.numeric(default_table))

# Ordenar el data frame por el total de instancias de forma descendente
default_counts <- default_counts[order(-default_counts$count), ]

# Agregar una columna con el porcentaje de instancias para cada categoría
default_counts$percent <- default_counts$count / sum(education_counts$count) * 100

# Calcular el promedio de la variable "y" para cada categoría de la variable "default"
y_means_d <- aggregate(poblacion2$y, by = list(poblacion2$default), FUN = mean)
names(y_means_d) <- c("default", "y_mean")

# Unir las tablas de conteo y promedio por categoría de "default"
default_counts <- merge(default_counts, y_means_d, by = "default")

# Ordenar
default_counts <- default_counts[order(default_counts$y_mean),]


##################
# Caso marital   #  -> tirar a solteros por la tasa de venta de un deposito a plazo, y porque por defecto
##################     una persona es soltera

# Contar el número de instancias en cada categoría de la variable "marital"
marital_table <- table(poblacion2$marital)

# Convertir la salida en un data frame
marital_counts <- data.frame(marital = names(marital_table), count = as.numeric(marital_table))

# Ordenar el data frame por el total de instancias de forma descendente
marital_counts <- marital_counts[order(-marital_counts$count), ]

# Agregar una columna con el porcentaje de instancias para cada categoría
marital_counts$percent <- marital_counts$count / sum(education_counts$count) * 100

# Calcular el promedio de la variable "y" para cada categoría de la variable "marital"
y_means_d <- aggregate(poblacion2$y, by = list(poblacion2$marital), FUN = mean)
names(y_means_d) <- c("marital", "y_mean")

# Unir las tablas de conteo y promedio por categoría de "marital"
marital_counts <- merge(marital_counts, y_means_d, by = "marital")

# Ordenar
marital_counts <- marital_counts[order(marital_counts$y_mean),]


######*CAMBIANDO UKNOWN POR SOLTEROS *#########################
poblacion2$marital[poblacion2$marital == "unknown"] <- "single"
###############################################################

##################
# Caso housing   # -> recomend: tirara uknoiwn a "no" , ya que, se parece a la tasa de contratacion y para tener una casa
##################              hay que hacer algo 


# Contar el número de instancias en cada categoría de la variable "housing"
housing_table <- table(poblacion$housing)

# Convertir la salida en un data frame
housing_counts <- data.frame(housing = names(housing_table), count = as.numeric(housing_table))

# Ordenar el data frame por el total de instancias de forma descendente
housing_counts <- housing_counts[order(-housing_counts$count), ]

# Agregar una columna con el porcentaje de instancias para cada categoría
housing_counts$percent <- housing_counts$count / sum(housing_counts$count) * 100

# Calcular el promedio de la variable "y" para cada categoría de la variable "housing"
y_means_d <- aggregate(poblacion$y, by = list(poblacion$housing), FUN = mean)
names(y_means_d) <- c("housing", "y_mean")

# Unir las tablas de conteo y promedio por categoría de "housing"
housing_counts <- merge(housing_counts, y_means_d, by = "housing")

# Ordenar
housing_counts <- housing_counts[order(housing_counts$y_mean),]


######*CAMBIANDO UKNOWN POR NO *###############################
poblacion2$housing[poblacion2$housing == "unknown"] <- "no"
###############################################################


##################
# Caso loan      # -> evaluar si la tiro a la categoria no, porque en "no" tengo 82% de no equivocarme
##################    y el 18% de si equivcarme

# Contar el número de instancias en cada categoría de la variable "loan"
loan_table <- table(poblacion2$loan)

# Convertir la salida en un data frame
loan_counts <- data.frame(loan = names(loan_table), count = as.numeric(loan_table))

# Ordenar el data frame por el total de instancias de forma descendente
loan_counts <- loan_counts[order(-loan_counts$count), ]

# Agregar una columna con el porcentaje de instancias para cada categoría
loan_counts$percent <- loan_counts$count / sum(loan_counts$count) * 100

# Calcular el promedio de la variable "y" para cada categoría de la variable "loan"
y_means_d <- aggregate(poblacion2$y, by = list(poblacion2$loan), FUN = mean)
names(y_means_d) <- c("loan", "y_mean")

# Unir las tablas de conteo y promedio por categoría de "loan"
loan_counts <- merge(loan_counts, y_means_d, by = "loan")

# Ordenar
loan_counts <- loan_counts[order(loan_counts$y_mean),]

######*CAMBIANDO UKNOWN POR NO *###############################
poblacion2$loan[poblacion2$loan == "unknown"] <- "no"
###############################################################



##################
# evaluar poutcome    
##################   

# Contar el número de instancias en cada categoría de la variable "loan"
poutcome_table <- table(poblacion2$poutcome)

# Convertir la salida en un data frame
poutcome_counts <- data.frame(poutcome = names(poutcome_table), count = as.numeric(poutcome_table))

# Ordenar el data frame por el total de instancias de forma descendente
poutcome_counts <- poutcome_counts[order(-poutcome_counts$count), ]

# Agregar una columna con el porcentaje de instancias para cada categoría
poutcome_counts$percent <- poutcome_counts$count / sum(poutcome_counts$count) * 100

# Calcular el promedio de la variable "y" para cada categoría de la variable "poutcome"
y_means_d <- aggregate(poblacion2$y, by = list(poblacion2$poutcome), FUN = mean)
names(y_means_d) <- c("poutcome", "y_mean")

# Unir las tablas de conteo y promedio por categoría de "poutcome"
poutcome_counts <- merge(poutcome_counts, y_means_d, by = "poutcome")

# Ordenar
poutcome_counts <- poutcome_counts[order(poutcome_counts$y_mean),]


# Aca analice la tendencia, si las tasas se parecen debo sumarla a otra vareiable que se parezca más 
# y tenga lógica y tambien  causalidad (eso se ve con el el step wize)

########################################################################################################
#         NUEVA FUNCION PARA CAMBIAR CATEGORICAS POR NUMERICAS
########################################################################################################
assign_numerical_values1 <- function(data, categorical_var,xd, numerical_var){
  # Convertir la variable categórica a un factor para incluir todas las categorías
  data[, categorical_var] <- factor(data[, categorical_var])
  
  # Calcular la media de la variable numérica para cada categoría de la variable categórica
  mean_vals <- aggregate(data[, numerical_var], list(data[, categorical_var]), mean)
  
  # Ordenar las categorías de la variable categórica en función de la media de la variable numérica
  mean_vals_sorted <- mean_vals[order(mean_vals$x, decreasing = TRUE), ]
  
  # Crea los indices crecientes para asignar los indices a las variables categoricas
  mean_vals_sorted$indice <- seq(from = 1, to = nrow(mean_vals_sorted))
  # rownames(mean_vals_sorted) <- seq(from = 1, to = nrow(mean_vals_sorted))
  
  # crea un vector con los indices ordenados
  numeros_asignados <- mean_vals_sorted$indice
  
  # cambia los valores de las categorias por los indices ordenados de mayor a menor
  # data$new_vals <- numeros_asignados[match(poblacion$loan, mean_vals_sorted$Group.1)]
  xd <- numeros_asignados[match(xd, mean_vals_sorted$Group.1)]
  
  # for(i in seq_along(data$loan)) {
  #   # Aquí va el código que queremos ejecutar para cada índice i
  #   print(paste("Fila", i, ": loan =", data$loan[i], ", new_vals =", data$new_vals[i]))
  # }
  return(xd)
}

str(poblacion2)

# 3era copia de l base de datos
poblacion3 <- poblacion2

# caso job
poblacion3$job <- assign_numerical_values1(poblacion3, "job",poblacion3$job, "y")
# caso de marital 
poblacion3$marital <- assign_numerical_values1(poblacion3, "marital",poblacion3$marital, "y")
# caso de education 
poblacion3$education <- assign_numerical_values1(poblacion3, "education",poblacion3$education, "y")
# caso de default 
poblacion3$default <- assign_numerical_values1(poblacion3, "default",poblacion3$default, "y")
# caso de housing 
poblacion3$housing <- assign_numerical_values1(poblacion3, "housing",poblacion3$housing, "y")
# caso de loan 
poblacion3$loan <- assign_numerical_values1(poblacion3, "loan",poblacion3$loan, "y")
# caso de contact 
poblacion3$contact <- assign_numerical_values1(poblacion3, "contact",poblacion3$contact, "y")
# caso de month 
poblacion3$month <- assign_numerical_values1(poblacion3, "month",poblacion3$month, "y")
# caso de day_of_week 
poblacion3$day_of_week <- assign_numerical_values1(poblacion3, "day_of_week",poblacion3$day_of_week, "y")
# caso de poutcome 
poblacion3$poutcome <- assign_numerical_values1(poblacion3, "poutcome",poblacion3$poutcome, "y")

# Pasar variables chr a int
poblacion3$emp.var.rate <- as.integer(poblacion3$emp.var.rate)
poblacion3$cons.price.idx <- as.integer(poblacion3$cons.price.idx)
poblacion3$cons.conf.idx <- as.integer(poblacion3$cons.conf.idx)
poblacion3$euribor3m <- as.integer(poblacion3$euribor3m)
poblacion3$nr.employed <- as.integer(poblacion3$nr.employed)

# NOTA: SON TODOS CATEÓRICOS
unknown_count <- colSums(poblacion3 == 'unknown')

###########################################################################
###########################################################################

# analizar la correlacion
str(poblacion3)


# Calcular la matriz de correlación
cor_matrix <- cor(poblacion3)

# Crear un filtro para seleccionar las correlaciones mayores al 80%
cor_filter <- cor_matrix > 0.8

# Seleccionar las variables que cumplen el filtro
selected_vars <- which(cor_filter, arr.ind = TRUE)

# Imprimir las variables seleccionadas y sus respectivas correlaciones
for (i in 1:nrow(selected_vars)) {
  var1 <- rownames(selected_vars)[selected_vars[i, 1]]
  var2 <- rownames(selected_vars)[selected_vars[i, 2]]
  corr <- cor_matrix[selected_vars[i, 1], selected_vars[i, 2]]
  cat(sprintf("%s y %s tienen una correlación de %.2f\n", var1, var2, corr))
}


# Eliminr la variable cons.price.idx y emp.var.rate tienen una correlación de 0.93
# SE ELININA: cons.price.idx para evitar multicoleanilidad

# Crear un nuevo data frame sin la variable "cons.price.idx"
poblacion4 <- dplyr::select(poblacion3, -cons.price.idx)

# regresión logistica

# Separar conjuntos de entrenamiento y prueba .
n <- nrow(poblacion4)
n_entrenamiento <- floor(0.8*n)
muestra <- sample.int(n = n, size = n_entrenamiento, replace = FALSE)
entrenamiento <- poblacion4[muestra,]
prueba <- poblacion4[-muestra,]

set.seed(1313)

# Ajustar modelo .
#modelo <- glm(am ~ wt , family = binomial(link="logit"), data=entrenamiento)
#print(summary(modelo))


# Modelo con todas las variables
modelo <- glm(y ~ ., data = entrenamiento, family = "binomial")
summary(modelo)

# Modelo con variables seleccionadas
modelo_stepwise <- stepAIC(modelo, direction = "both", trace = FALSE)
summary(modelo_stepwise)

# Evaluar el modelo con el conjunto de entrenamiento .
probs_e <- predict(modelo_stepwise, entrenamiento, type="response")

umbral <- 0.5
preds_e <- sapply(probs_e,function(p)ifelse(p >=umbral, "1", "0"))
preds_e <- factor(preds_e, levels=levels(poblacion4[["y"]]))

ROC_e<- roc(entrenamiento[["y"]], probs_e)
plot(ROC_e)

# Tiene un AUC de 92,72%

############
prediccion <- predict(modelo_stepwise, newdata = prueba, type = "response")

tabla <- table(prueba$y, prediccion > 0.5)

# La matriz de confusión nos indica cuántos registros fueron clasificados 
# correctamente y cuántos fueron clasificados incorrectamente. En este caso,
# la probabilidad umbral para clasificar como positivo es 0.5

exactitud <- sum(diag(tabla)) / sum(tabla)
# tiene 90,93227% de exactitud

# crear matriz de confusión
matriz_confusion <- matrix(c(7122, 188, 559, 369), nrow = 2, ncol = 2, byrow = TRUE, dimnames = list(c("0", "1"), c("0", "1")))

# calcular métricas
evaluacion <- confusionMatrix(matriz_confusion)

####### Evaluar el modelo con el conjunto de prueba ######.
probs_e <- predict(modelo_stepwise, prueba, type="response")

umbral <- 0.5
preds_e <- sapply(probs_e,function(p)ifelse(p >=umbral, "1", "0"))
preds_e <- factor(preds_e, levels=levels(poblacion4[["y"]]))

ROC_e<- roc(prueba[["y"]], probs_e)
plot(ROC_e)

# Ahora, evaluando con menos variables, es decir, sta vez solo incluyendo las variables significativas
modelo2 <- glm(y ~ job + default + contact + month + duration + previous + poutcome + emp.var.rate + cons.conf.idx + nr.employed, family = "binomial", data = entrenamiento)
summary(modelo2)


# Evaluar el modelo con el conjunto de entrenamiento .
probs_e <- predict(modelo2, entrenamiento, type="response")

umbral <- 0.5
preds_e <- sapply(probs_e,function(p)ifelse(p >=umbral, "1", "0"))
preds_e <- factor(preds_e, levels=levels(poblacion4[["y"]]))

ROC_e<- roc(entrenamiento[["y"]], probs_e)
plot(ROC_e)

prediccion <- predict(modelo_stepwise, newdata = prueba, type = "response")

tabla <- table(prueba$y, prediccion > 0.5)

# La matriz de confusión nos indica cuántos registros fueron clasificados 
# correctamente y cuántos fueron clasificados incorrectamente. En este caso,
# la probabilidad umbral para clasificar como positivo es 0.5

exactitud <- sum(diag(tabla)) / sum(tabla)

# Ahora con los datos de prueba
probs_e <- predict(modelo2, prueba, type="response")

umbral <- 0.5
preds_e <- sapply(probs_e,function(p)ifelse(p >=umbral, "1", "0"))
preds_e <- factor(preds_e, levels=levels(poblacion4[["y"]]))

ROC_e<- roc(prueba[["y"]], probs_e)
plot(ROC_e)









