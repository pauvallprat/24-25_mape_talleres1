
# Preliminares ------------------------------------------------------------
library("pacman") 
install.packages("devtools")
library(devtools)
devtools::install_github("ddauber/r4np")

p_load("r4np")       # p_load es una función del paquete pacman. Y r4np es un paquete. 

create_project_folder()


# Crear una función -------------------------------------------------------

# Una función que retorne el valor de la mitad de un número. 

funcionmitad <- function(x) {
  y <- x/2
  return(y)
}

pepito <- 3

funcionmitad(pepito)

f_restar <- function(x) {
  y <-  x-3
  return(y)
}

f_restar(555)


# Función con 2 argumentos. 

funcionpotencia <- function (x,y) {
  potencia <- x^y
  return (potencia)
}

funcionpotencia(pepito,2)

#Función que devuelva texto
funciontexto <- function(x,y,z) {
  potencia <- x^y
  texto <- paste(z, potencia)
  return (texto)
}
funciontexto(3,4, "El resultado de la potencia es:")


# Tidyverse ---------------------------------------------------------------

library(tidyverse)

#Media de los valores 1, 5 y 8, redondeados a dos decimales. 
numeros <- c(1,5,8)
media <- mean(numeros) %>% 
  round(2)

# Ahora sin crear objetos
mean(c(1,5,8)) %>% round(2)

#Más limpio
c(1,5,8) %>% 
  mean() %>% 
  round(2)

  