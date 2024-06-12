# Laboratorio 2 Modelo Estocastico 2024

# Fecha: 12 de junio de 2024

# Gianfranco Astorga Saco

# RUT: 20.245.673-1

# Enunciado:

# En una tienda con un unico servidor se estan tomando los tiempos entre llegada de los clientes.
# Despues de haber realizado un analisis estadistico se determino que el tiempo entre llegadas de los clientes se distribuye
# log normal con una media logaritmica de 0.2 y una desviacion estandar logaritmica de 0.3.

# Junto con lo anterior se midio el tiempo de atencion de los clientes en la caja.
# Igualmente, un analisis estadistico determino que los tiempos de atencion se distribuyen log normal
# con una media logaritmica de 0.15 y una desviacion estandar logaritmica de 0.3.

# Con esta informacion se le pide programar en R las siguientes actividades:

# 1. Determinar la hora de llegada de 1000 clientes asumiendo que los tiempos entre llegada distribuyen segun lo mencionado en el enunciado.

# Desarrollo 1:

# Se importa la libreria de distribuciones lognormales

library(MASS)

# Se definen los parametros de la distribucion lognormal de los tiempos entre llegadas

mu_tiempo_entre_llegadas = 0.2

sigma_tiempo_entre_llegadas = 0.3

# Se generan 1000 tiempos entre llegadas

tiempos_entre_llegadas = rlnorm(1000, mu_tiempo_entre_llegadas, sigma_tiempo_entre_llegadas)

# Se suman los tiempos entre llegadas para obtener la hora de llegada de los clientes

hora_llegada_clientes = cumsum(tiempos_entre_llegadas)

# Se muestran las primeras 10 horas de llegada de los clientes

print('Las primeras 10 horas de llegada de los clientes son:')
print(hora_llegada_clientes[1:10])

# 2. Determinar los tiempos de atencion de los 1000 clientes asumiendo que los tiempos de atencion distribuyen segun lo mecionado en el enunciado.

# Se definen los parametros de la distribucion lognormal de los tiempos de atencion

mu_tiempo_atencion = 0.15

sigma_tiempo_atencion = 0.3

# Se generan 1000 tiempos de atencion

tiempos_atencion = rlnorm(1000, mu_tiempo_atencion, sigma_tiempo_atencion)

# Se muestran los primeros 10 tiempos de atencion

print('Los primeros 10 tiempos de atencion son:')
print(tiempos_atencion[1:10])

# 3. Determinar el tiempo promedio de espera de los clientes para ser atendidos.

# Se calcula el tiempo de espera de los clientes

tiempo_espera = numeric(1000)

tiempo_espera[1] = 0

for(i in 2:1000){
  tiempo_espera[i] = max(0, tiempo_espera[i-1] + tiempos_entre_llegadas[i-1] - tiempos_atencion[i-1])
}

# Se calcula el tiempo promedio de espera de los clientes

tiempo_promedio_espera = mean(tiempo_espera)

# Se muestra el tiempo promedio de espera de los clientes

print(paste('El tiempo promedio de espera de los clientes es:', tiempo_promedio_espera))

# 4. Realizar 1000 replicas del sistema y determinar una distribucion de probabilidad del tiempo promedio de espera de los clientes.

# Se definen las funciones para calcular el tiempo promedio de espera de los clientes

calcular_tiempo_promedio_espera = function(){
  tiempos_entre_llegadas = rlnorm(1000, mu_tiempo_entre_llegadas, sigma_tiempo_entre_llegadas)
  tiempos_atencion = rlnorm(1000, mu_tiempo_atencion, sigma_tiempo_atencion)
  
  tiempo_espera = numeric(1000)
  tiempo_espera[1] = 0
  
  for(i in 2:1000){
    tiempo_espera[i] = max(0, tiempo_espera[i-1] + tiempos_entre_llegadas[i-1] - tiempos_atencion[i-1])
  }
  
  return(mean(tiempo_espera))
}

# Se realizan 1000 replicas del sistema

tiempos_promedio_espera = replicate(1000, calcular_tiempo_promedio_espera())

# Se muestra la distribucion de probabilidad del tiempo promedio de espera de los clientes

hist(tiempos_promedio_espera, main='Distribucion de probabilidad del tiempo promedio de espera de los clientes', xlab='Tiempo promedio de espera', ylab='Frecuencia', col='blue')
