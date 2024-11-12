
# Preliminares ------------------------------------------------------------
#instalamos el paquete de pacman
install.packages("pacman")

#Cargamos pacman
library(pacman)

p_load(tidyverse,titanic)

# esto para decirle que cargue todo: pacman::p_load(tidyverse,titanic)

# Instalamos el paquete r4np
install.packages("devtools")
library(devtools)
install_github("ddauber/r4np")
library(r4np)
 1
 
 r4np::create_project_folder()
 
 


# Vamos a trabajar con funciones ------------------------------------------

# Crear un onjeto que contenga un número

pepito <- 7
 
 # Crear un afunción que calcula la mitad del valor

 f_mitad <- function(x){
   y <- x/2
   return(y)
 }
 
 
 # probar la función
 f_mitad(pepito)
 
 
 
 # funcion de dos argumentos
 
 f_doble <- function(a,b){
   y <- a^b
   return(y)
 }
 
 f_doble(pepito,pepito)
 f_doble(b=3,a=4)
 

# Usamos pipes! -----------------------------------------------------------

# Calcular la media de 1, 5, 8 redondeado a 2 decimales
 
library(tidyverse)
v <- c(1,5,8)
 media <- mean (v)
 redondear <- round (media, digits=2)
 
 
 c(1,5,8) %>% mean() %>% round(digits=2)
 
 
 