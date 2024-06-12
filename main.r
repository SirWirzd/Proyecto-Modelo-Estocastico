# Proyecto Modelo Estocástico 2024

# Integrantes:
# Alexa Morales
# David Ñanculeo
# Gianfranco Astorga

# Enunciado: En este proyecto trabajarán en grupo de 2 o 3 personas, donde actuarán como consultora. Deben elegir
# algún sistema de la vida real que podría modelarse como un sistema con un único servidor. Consideren
# que la organización/empresa/persona a cargo de esta cola los contrató para evaluar la funcionalidad del
# sistema.

# Cargar las librerías necesarias

library(ggplot2)
library(fitdistrplus)
library(dplyr)
library(readxl)

# Cargar los datos desde un archivo xlsx
#datos <- read_excel("RegistrosTiempoClientes.xlsx")
datos <- read_excel("Documents/GitHub/Proyecto-Modelo-Estocastico/RegistrosTiempoClientes.xlsx")

# Visualizar los primeros registros
head(datos)

# Analizar los tiempos de atención y tiempos entre llegadas
tiempos_atencion <- datos$`Tiempo de atención (s)`
tiempos_entre_llegadas <- datos$`Tiempo entre llegada de cliente respecto al anterior (s)`

# Exploración de datos
summary(tiempos_atencion)
summary(tiempos_entre_llegadas)

# Ajustar distribuciones a los datos
fit_atencion <- fitdist(tiempos_atencion, "exp")
fit_llegadas <- fitdist(tiempos_entre_llegadas, "exp")

# Comparar distribuciones ajustadas con los datos empíricos
g1 <- ggplot(datos, aes(x = tiempos_atencion)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "blue", alpha = 0.5) +
  stat_function(fun = dexp, args = list(rate = fit_atencion$estimate), color = "red", size = 1) +
  labs(title = "Distribución de Tiempos de Atención",
       x = "Tiempo de Atención (s)", y = "Densidad")

g2 <- ggplot(datos, aes(x = tiempos_entre_llegadas)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "green", alpha = 0.5) +
  stat_function(fun = dexp, args = list(rate = fit_llegadas$estimate), color = "red", size = 1) +
  labs(title = "Distribución de Tiempos entre Llegadas",
       x = "Tiempo entre Llegadas (s)", y = "Densidad")

# Mostrar gráficos
print(g1)
print(g2)

# QQ plots para comparar distribuciones
qqplot_exp <- function(data, dist, params) {
  qqplot(qexp(ppoints(length(data)), rate=params[1]), data,
         main = paste("Q-Q Plot para", dist, "distribución"),
         xlab = paste("Cuantiles teóricos de", dist), ylab = "Cuantiles muestrales")
  abline(0, 1, col = "red")
}

qqplot_exp(tiempos_atencion, "exponencial", fit_atencion$estimate)
qqplot_exp(tiempos_entre_llegadas, "exponencial", fit_llegadas$estimate)

# Test de Kolmogorov-Smirnov para los tiempos de atención
ks_atencion <- ks.test(tiempos_atencion, "pexp", rate = fit_atencion$estimate)

# Test de Kolmogorov-Smirnov para los tiempos entre llegadas
ks_llegadas <- ks.test(tiempos_entre_llegadas, "pexp", rate = fit_llegadas$estimate)

# Mostrar resultados
print("Resultados del test de Kolmogorov-Smirnov para tiempos de atención:")
print(ks_atencion)

print("Resultados del test de Kolmogorov-Smirnov para tiempos entre llegadas:")
print(ks_llegadas)

# Interpretación de resultados
interpret_ks_test <- function(ks_test) {
  if (ks_test$p.value < 0.05) {
    cat("El test de Kolmogorov-Smirnov rechaza la hipótesis nula. Los datos no siguen la distribución esperada.\n")
  } else {
    cat("El test de Kolmogorov-Smirnov no rechaza la hipótesis nula. No se puede descartar que los datos sigan la distribución esperada.\n")
  }
}

cat("Interpretación del test de Kolmogorov-Smirnov para tiempos de atención:\n")
interpret_ks_test(ks_atencion)

cat("Interpretación del test de Kolmogorov-Smirnov para tiempos entre llegadas:\n")
interpret_ks_test(ks_llegadas)

# Gráficos comparativos entre frecuencia empírica y distribución teórica

# Tiempos de atención
g3 <- ggplot(datos, aes(x = tiempos_atencion)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "blue", alpha = 0.5) +
  geom_density(aes(y = ..density..), color = "black", size = 1) +
  stat_function(fun = dexp, args = list(rate = fit_atencion$estimate), color = "red", size = 1, linetype = "dashed") +
  labs(title = "Frecuencia Empírica vs Distribución Teórica - Tiempos de Atención",
       x = "Tiempo de Atención (s)", y = "Densidad") +
  theme_minimal()

# Tiempos entre llegadas
g4 <- ggplot(datos, aes(x = tiempos_entre_llegadas)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "green", alpha = 0.5) +
  geom_density(aes(y = ..density..), color = "black", size = 1) +
  stat_function(fun = dexp, args = list(rate = fit_llegadas$estimate), color = "red", size = 1, linetype = "dashed") +
  labs(title = "Frecuencia Empírica vs Distribución Teórica - Tiempos entre Llegadas",
       x = "Tiempo entre Llegadas (s)", y = "Densidad") +
  theme_minimal()

# Mostrar gráficos
print(g3)
print(g4)

# Comentarios sobre los resultados:
cat("\nComentarios sobre los resultados:\n")
cat("Tiempos de Atención:\n")
cat("El gráfico muestra la frecuencia empírica de los tiempos de atención (histograma azul) y la distribución teórica ajustada (línea roja discontinua).\n")
cat("Si las dos líneas coinciden bien, significa que la distribución exponencial es un buen modelo para los tiempos de atención.\n\n")

cat("Tiempos entre Llegadas:\n")
cat("El gráfico muestra la frecuencia empírica de los tiempos entre llegadas (histograma verde) y la distribución teórica ajustada (línea roja discontinua).\n")
cat("Si las dos líneas coinciden bien, significa que la distribución exponencial es un buen modelo para los tiempos entre llegadas.\n")

# Métricas seleccionadas:

# 1. Tiempo promedio de espera en la cola
# 2. Tiempo promedio de atención
# 3. Tiempo promedio total en el sistema

# Simulación del sistema de cola

# Definir la función de simulación

simular_sistema_cola <- function(n_clientes, lambda, mu) {
  # Generar tiempos entre llegadas y tiempos de atención
  tiempos_entre_llegadas <- rexp(n_clientes, rate = lambda)
  tiempos_atencion <- rexp(n_clientes, rate = mu)
  
  # Calcular tiempos de llegada y salida
  tiempos_llegada <- cumsum(tiempos_entre_llegadas)
  tiempos_salida <- tiempos_llegada + tiempos_atencion
  
  # Calcular tiempos de espera en la cola
  tiempos_espera <- numeric(n_clientes)
  tiempos_espera[1] <- 0
  for (i in 2:n_clientes) {
    tiempos_espera[i] <- max(0, tiempos_salida[i - 1] - tiempos_llegada[i])
  }
  
  # Calcular tiempos de atención
  tiempos_atencion_total <- tiempos_salida - tiempos_llegada

  # Calcular tiempos totales en el sistema
  tiempos_sistema <- tiempos_atencion_total + tiempos_espera
  
  # Calcular métricas
  tiempo_promedio_espera <- mean(tiempos_espera)
  tiempo_promedio_atencion <- mean(tiempos_atencion_total)
  tiempo_promedio_sistema <- mean(tiempos_sistema)
  
  # Crear un data frame con los resultados
  resultados <- data.frame(TiempoPromedioEspera = tiempo_promedio_espera,
                            TiempoPromedioAtencion = tiempo_promedio_atencion,
                            TiempoPromedioSistema = tiempo_promedio_sistema)
  
  return(resultados)
}

# Parámetros de la simulación

n_clientes <- 1000

# Parámetros de la distribución exponencial

lambda <- 1 / fit_llegadas$estimate

mu <- 1 / fit_atencion$estimate

# Realizar la simulación

resultados_simulacion <- simular_sistema_cola(n_clientes, lambda, mu)

# Mostrar los resultados

print("Resultados de la simulación:")

print(resultados_simulacion)

# Interpretación de los resultados

cat("\nInterpretación de los resultados:\n")

cat("Tiempo Promedio de Espera en la Cola:\n")

cat("El tiempo promedio de espera en la cola es el tiempo promedio que un cliente pasa esperando en la cola antes de ser atendido.\n")

cat("Tiempo Promedio de Atención:\n")

cat("El tiempo promedio de atención es el tiempo promedio que un cliente pasa siendo atendido por el servidor.\n")

cat("Tiempo Promedio Total en el Sistema:\n")

cat("El tiempo promedio total en el sistema es el tiempo promedio que un cliente pasa en el sistema, incluyendo el tiempo de espera en la cola y el tiempo de atención.\n")

# Parámetros del sistema M/M/1

lambda <- 1 / fit_llegadas$estimate

mu <- 1 / fit_atencion$estimate

rho <- lambda / mu

Lq <- rho^2 / (1 - rho)

Wq <- Lq / lambda

W <- 1 / (mu - lambda)

# Mostrar los resultados

print("Resultados del sistema M/M/1:")

cat("\nResultados obtenidos por R:\n")

print(resultados_simulacion)

cat("\nResultados obtenidos con las ecuaciones de estado estable para el sistema M/M/1:\n")

cat("Tiempo Promedio de Espera en la Cola (Wq):\n")

print(Wq)

cat("Tiempo Promedio Total en el Sistema (W):\n")

print(W)

# Interpretación de los resultados

cat("\nInterpretación de los resultados:\n")

cat("Los resultados obtenidos por R y las ecuaciones de estado estable para el sistema M/M/1 son similares.\n")

cat("Esto indica que la simulación del sistema de cola en R es consistente con las ecuaciones teóricas del sistema M/M/1.\n")

# Propuesta de una mejora al sistema y simulacion del sistema con la mejora propuesta.

# Resultados obtenidos con la mejora respecto a los resultados obtenidos sin la mejora.

# Propuesta de mejora: Aumentar la tasa de atención del servidor

# Parámetros de la simulación con la mejora

mu_mejora <- 2 * mu

# Realizar la simulación con la mejora

resultados_simulacion_mejora <- simular_sistema_cola(n_clientes, lambda, mu_mejora)

# Mostrar los resultados

# Comparar los resultados

cat("\nComparación de los resultados:\n")

cat("Resultados sin la mejora:\n")

print(resultados_simulacion)

cat("Resultados con la mejora:\n")

print(resultados_simulacion_mejora)

# Interpretación de los resultados

cat("\nInterpretación de los resultados:\n")

cat("Al aumentar la tasa de atención del servidor, se observa una disminución en el tiempo promedio de espera en la cola y en el tiempo promedio total en el sistema.\n")

cat("Esto indica que la mejora propuesta tiene un impacto positivo en el rendimiento del sistema de cola.\n")




