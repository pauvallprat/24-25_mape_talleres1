
# Preliminares ------------------------------------------------------------
library(pacman)
install.packages("devtools")
library(devtools)
devtools::install_github("ddauber/r4np")
p_load("r4np")

#Creamos las carpetas en el directorio de trabajo
create_project_folder()

pepito <- 3

# Una función que retorne el valor de la mitad de un número

# Lo que tienes que hacer con esta x es dividirla entre 2 y que me muestres el resutado de y

funcionmitad <- function(x) {
  y <- x/2
  return(y)
}

# Calculamos la mitad de pepito
funcionmitad(pepito)

# Probamos otra función
funcionpotencia <- function(x,y) {
  potencia <- x^y
  return(potencia)
}

funcionpotencia(x=pepito,y=4)

# Otra función, con 3 argumentos
# "El resultado de la potencia es:"

funciontexto <- function(x,y,z) {
  potencia <- x^y
  z <- "El resultado de la potencia es:"
  texto <- paste0(z,potencia)
  return(texto)
}

funciontexto(pepito,4)


# Tidyverse ---------------------------------------------------------------
library(tidyverse)
# Uso de pipe
# Cómo obtener la media de los valores 1, 5 y 8 redondeados a 2 decimales
# Primero hay que crear un vector (un conjunto de datos) a través de un objeto. 
numeros <- c(1,5,8)
media <- mean(numeros) %>% round(2)

# Ahora de verdad, solo usando pipes!

mean(c(1,5,8)) %>% round(2)

# Más limpio!

c(1,5,8) %>% mean() %>% round(2)
