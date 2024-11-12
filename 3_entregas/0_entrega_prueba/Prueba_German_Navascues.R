# Preliminares ------------------------------------------------------------
# Instalamos el paquete pacman
install.packages("pacman")

# Cargamos pacman
library(pacman)

p_load(tidyverse,titanic)

# Instalamos el paquere r4np
# Devtools para descargar paquetes de GitHub
install.packages("devtools")
library(devtools)
# Instalamos y cargamos el paquete r4np
install_github("ddauber/r4np")
library(r4np)
  
r4np::create_project_folder()


# Vamos a trabajar con funciones ------------------------------------------

# Crear un objeto que contenga un número

pepito <-  7

# Creamos una función que calcula la mitad del valor

f_mitad <- function(x){
  y <- x/2
  return(y)
}

# Probar la función
f_mitad(pepito)


# Vamos a crear una función con 2 argumentos

f_potencia <- function(a,b){
  y <- a^b
  return(y)
}

# Probar la función
f_potencia(pepito,pepito)

# Usamos pipes! -----------------------------------------------------------

library(dplyr)

# Calcular la media de 1, 5 y 8 redondeado a 2 decimales

c(1,5,8) %>% mean() %>% round(digits = 2)


