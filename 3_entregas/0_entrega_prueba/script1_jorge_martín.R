
# Preliminares ------------------------------------------------------------
#install.packages("pacman")
library(pacman)
#install.packages("devtrools")
library(devtools)
devtools::install_github("ddauber/r4np")

p_load("r4np") #Este paquete sirve para tener el fichero bien instalado

create_project_folder() #Mirar cómo personalizar las carpetas que se crean y los nombres



# Creamos un objeto numérico ----------------------------------------------

pepito <- 3

# Crear una función -------------------------------------------------------
#Una función que retorne el valor de la mitad de un número
funcionmitad <- function(x){
  y <- x/2
  return(y)
}

funcionmitad(pepito)

f_6_7 <- function(x){
  y<- x*6/7
  return(y)
}

f_6_7(9)

# Fucnión con dos argumentos
funcionpotencia <- function(x,y){
  potencia <- x^y
  return(potencia)
}

funcionpotencia(pepito,4)


funcionpotencia2 <- function(x,y){
  potencia <- x^y
  z <- "El resultado de la potencia es: "
  texto <- paste0(z,potencia)
  return(texto)
}

funcionpotencia2(pepito,4)



# Tidyverse ---------------------------------------------------------------
p_load(tidyverse)

# Uso de pipe
# Media de los valores 1, 5 y 8, redondeados a 2 decimales
numeros <- c(1,5,8)
mean(c(1,5,8)) %>% round(2) # Los pipes son útiles para no tener que meter tantos elementso dentro de una función. Este comando es más claro que usar round(mean(c(1,5,8)))

#Una opción más limpia aún
c(1,5,8) %>% #Primero creo el vector numérico
  mean() %>% #Despús se hace la media
  round(2) #Por último se redondea

#Es mejor usar más pipes que meter muchas funciones unas dentro de otras.








