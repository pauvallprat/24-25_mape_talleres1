# Preliminares ------------------------------------------------------------
# Lo de arriba se crea poniendo ctrl, shift 
# si usas ctrl, shift + c haces que se comente la línea automáticamente

# cargamos el paquete pacman
library("pacman")

# con esto se cargan e instalan paquetes que vayas a usar
# pacman::p_load(tidyverse, titanic) tambien se puede hacer esto para ahorrarte el library de arriba.
p_load(tidyverse, titanic)

# hay que ser ordenados, hay que empezar por preliminares (que es cargar las cosas) 
# y despues poner una seccion que sea base de datos etc

#install.packages("devtools")
devtools::install_github("ddauber/r4np")
library("r4np")

r4np::create_project_folder()

# Vamos a trabajar con funciones ------------------------------------------

# vamos a crear un objeto que contenga un número
pepito <- 7
  
  # creamos una función que calcula mitad del valor

f_mitad <- function(x){
  y <- x/2
  return(y)
  }

# probamos la funcion

f_mitad(585)

# hacemos otra funcion

f_potencia <- function(a,b){
  y <- a^b
  
  return(y)
}

f_potencia(2, pepito)

# tambien vale cambiar el orden
f_potencia (b = 3, a = 4)


# media de 1, 5 y 8 usando pipes redondeado a dos valores ------------------------------------------

numeros <- c(1,5, 8)

media_redondeada <- numeros %>% 
  mean() %>% 
  round(2)
print(media_redondeada)

# otra alternativa es
c(1, 5, 8) %>% mean() %>% round(2)



