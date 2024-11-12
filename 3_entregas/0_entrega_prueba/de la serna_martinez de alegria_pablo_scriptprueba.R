
# Preliminares ------------------------------------------------------------
library("pacman")
library("devtools")
devtools::install_github("ddauber/r4np")
p_load("r4np")
create_project_folder()

# Crear una función -------------------------------------------------------

#Una función que retorne el valor de la mitad de un número

f_mitad <- function(x){
  y <- x/2
  return(y)
}
f_mitad(3)

f_raiz <- function(x){
  y <- x^1/2
  return(y)
}


# Probamos otra función dos argumentos ------------------------------------

funcionpotencia <- function(x,y){
  potencia <- x^y
  return(potencia)
}
       

# Función con tres argumentos ---------------------------------------------

funciontexto <- function (x,y) {
  potencia <- x^y
  z <- "El resultado de la potencia es: "
  texto <- paste0(z,potencia)
  return(texto)
} 

funciontexto(3,4)


# Tidyverse ---------------------------------------------------------------
library(tidyverse)
#Uso de pipe
#Media de los valores 1, 5, 8

media <- mean(c(1,5,8)) %>% round(2) 

c(1,5,8 ) %>% mean() %>% round(2) #más limpio 
