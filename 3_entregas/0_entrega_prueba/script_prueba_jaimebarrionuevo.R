
# Preliminares ------------------------------------------------------------
library("pacman")
p_load(r4np)


# Creamos las carpetas en el directorio de trabajo ------------------------
create_project_folder()


# Creamos un objeto numérico ----------------------------------------------


pepito <- 3


# Crear una función -------------------------------------------------------

funcionmitad <- function(x){
  y <- x/2
  return(y)
  }


# Calculamos la mitad de pepito -------------------------------------------

funcionmitad(pepito)


# Probamos otra función ---------------------------------------------------

funcionpotencia <- function(x,y){
  potencia <- x^y
  return(potencia)
}


funcionpotencia(y=4,x=pepito)


# Media de valores 1, 5 y 8 redondeados a 2 decimales (Ejercicio) - Pipes --------------------------------------------

p_load(tidyverse)

numeros <- c(1,5,8)

media <- mean(numeros) %>% round(2)


# Ahora de verdad, solo usando pipes! -------------------------------------

mean(c(1,5,8)) %>% round(2)


# ¡Más limpio! ------------------------------------------------------------

c(1,5,8) %>% mean() %>% round(2)


