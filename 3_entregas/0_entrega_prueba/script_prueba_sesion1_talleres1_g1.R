# Preliminares ------------------------------------------------------------

library(pacman)
# install.packages("devtools")
# library(devtools)
devtools::install_github("ddauber/r4np")

p_load("r4np")

# Creamos las carpetas en el directorio de trabajo
create_project_folder()

# Creamos un objeto numérico
pepito <- 3


# Crear una función -------------------------------------------------------

# Una función que retorne el valor de la mitad de un número

funcionmitad <- function(x){
  y <- x/2
  return(y)
}

# Calculamos la mitad de pepito
funcionmitad(pepito)

# Probamos otra función. Con 2 argumentos

funcionpotencia <- function(x,y){
  potencia <- x^y
  return(potencia)
}


funcionpotencia(y=4,x=pepito)

# Otra función, con 3 argumentos
# "El resultado de la potencia es:"

funciontexto <- function(x,y,z){
  potencia <- x^y
  texto <- paste0(z,potencia)
  return(texto)
}

funciontexto(pepito,4,"El resultado es: ")


# Tidyverse ---------------------------------------------------------------
p_load(tidyverse)
# Uso de pipe
# Media de los valores 1, 5 y 8 redondeados a 2 decimales
numeros <- c(1,5,8)
media <- mean(numeros) %>% round(2)

# Ahora de verdad, solo usando pipes!
mean(c(1,5,8))%>% round(2)
# Más limpio!
c(1,5,8) %>% # vector numérico
  mean() %>% # calculo la media
  round(2) # redondeo
