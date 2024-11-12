# Preliminares ------------------------------------------------------------

install.packages("pacman")
library(pacman)

# Con p_load se instala y library a la vez
p_load(tidyverse, titanic) #todas las librerías que queramos

# Instalamos el paquete r4np
# Primero devtools

install.packages("devtools")
install.packages("vctrs")
library(devtools)

install_github("ddauber/r4np")
library(r4np)

r4np::create_project_folder()


# Vamos a hacer funciones -------------------------------------------------

# Crear objeto que contenga un número

pepito <- 7

# Creamos función para calcular mitad del valor

f_mitad <- function(x){
  y <- x/2
  return(y)
}

# Probar función
f_mitad(pepito)

f_potencia <- function(x,y){
  z <- x^y
  return(z)
}

f_potencia(2,2)

# Calcular media en una misma línea utilizando pipes
c(1, 5, 8) |> mean() |> round(2)

