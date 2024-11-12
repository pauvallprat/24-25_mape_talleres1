
# Preliminares ------------------------------------------------------------
# Ctrl+shift+r --> te crea una sección/escribe.
# Ctrl+shift+c --> te pone almohadilla y puedes comentar directamente.
library(pacman)

p_load(tidyverse,titanic) 
#sirve para descargar y cargar al mismo tiempo un paquete.

# Instalamos el paquete r4np

install.packages("devtools")
library(devtools)
install_github("ddauber/r4np")
library(r4np)

r4np::create_project_folder()


# Trabajo con funciones ---------------------------------------------------

# Crear un objeto que contenga un número.

Joselito<- 7

# Creamos una función que calcula la mitad del valor.

f_mitad<-function(x){
  y <- x/2
  return(y)
}
  
# Alt+ "guión" - --> te pone la flecha de declarar
f_mitad(Joselito)

f_potencia <- function(x,z){
  y <- x^z
  return(y)
}

f_potencia(Joselito,Joselito)
# Función con dos argumentos


# Usamos pipes ------------------------------------------------------------

# Calcular la media de 1,5,8 y redondeando a 2 decimales

library(tidyverse)
c(1,5,8) |> mean() |> round(2)

# Usar las pipes lo máximo posible






