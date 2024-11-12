
# Preliminares ------------------------------------------------------------
## control + shift + R
library(pacman)
install.packages("devtools")
devtools::install_github("ddauber/r4np")
p_load("r4np") 
#combinación entre install y library. r4np sirve para mayor organización. 

#creamos las carpetas en el directorio 
create_project_folder()

#alt + guion 


# Crear una función -------------------------------------------------------
funcionmitad <- function(x){
  y <- x/2
  return(y)
  
}
pepito <- 3
funcionmitad(pepito)

funcioncuadrado <- function(x){
  y <- x^2
  return(y)
  }
funcioncuadrado(500)

funcionpotenciaescrita <- function(x,y,z){
  potencia <- x^y
  z <- "El resultado es: "
  texto <- paste0(z,potencia) #porque mi x ya tiene espacio
  return(texto)
}

funcionpotenciaescrita(pepito,4)

# flechita, ctrl, m: %>% 


# uso de pipes ------------------------------------------------------------
# media de 1,5 y 8, redondeando a 2 decimales 

p_load(tidyverse)
numeros <- c(1,5,8)
media <- mean (numeros) %>% round (2)

mean (c(1,5,8)) %>% round (2)
c(1,5,8) %>% mean() %>% round(2)