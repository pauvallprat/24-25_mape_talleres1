# Preliminares ------------------------------------------------------------
install.packages("pacman")
library(pacman)

# Con p_load() haces el install y el library a la vez, muy útil 
p_load(tidyverse, Titanic)

# Instalamos el paquete r4np
install.packages("devtools")
library(devtools)
install_github("ddauber/r4np")
library(r4np)
library(tidyverse)

r4np::create_project_folder()


# Vamos a trabajar con funciones ------------------------------------------
# Crear un objeto que contenga un número
pepito <- 7

# Creamos una función que calcula la mitad del valor 
f_mitad <- function(x) {
  y <- x/2
  return(y)
}
# Probamos la función 
f_mitad(pepito)
f_mitad(85647)

# Voy a hacer una función para calcular una puta potencia (?)
f_potencia <- function(a,b){
  y <- a^b
  return(y)
}
f_potencia(5,7)


# Usamos pipes ------------------------------------------------------------
# Calcular la media de 1,5 y 8 redondeado a 2 decimales 
v <- c(1, 5, 8)
media <- mean(v)
redondear <- round(media, digits = 2)
redondear

# Otra forma de hacerlo más sencilla 
c(1,5,8) %>% mean() %>% round(digits = 2)


