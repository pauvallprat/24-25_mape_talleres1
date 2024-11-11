# Preliminares ------------------------------------------------------------
# Con ctrl + shift + r
# Instalamos el paquete pacman
# install.packages("pacman")

# Cargamos pacman
library(pacman) # sirve para no tener que instalar y hacer library

p_load(tidyverse,titanic)

# Instalamos el paquete r4np
# Devtools para descargar paquetes de GitHub
install.packages("devtools") 
library(devtools)
# Instalamos y cargamos el paquete r4np
install_github("ddauber/r4np")
library(r4np)

r4np::create_project_folder()

# Funciones ------------------------------------------

# Crear un objeto que contenga un número

pepito <- 7

# Función para calcular la mitad del valor

f_mitad <- function(x){
  y <- x/2
  return(y)
}

# Probar la función
f_mitad(pepito)

# Función con 2 argumentos

f_potencia <- function(a,b){
  y <- a^b
  return(y)
}

# Con pipes -----------------------------------------------------------

# Calcular la media de 1,5 y 8 redondeado a 2 decimales

c(1,5,8) %>% mean() %>% round(digits = 2)


