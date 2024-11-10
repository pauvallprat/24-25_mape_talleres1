# Preliminares ------------------------------------------------------------
install.packages("pacman")  

# Cargamos pacman para poder usar el p_load
library(pacman)

p_load(tidyverse, titanic)

# Esta función sirve: 1. mirar si el paquete que estoy indicando esta instalado o no, si no lo está, lo instala, y si ya lo está
# 2. lo carga. Con el p_load es suficiente tanto para instalar como para cargar todos los paquetes.
# Esto es util si trabajas con otros y tu no sabes si el paquete lo tiene instalado o no.


# Instalamos el paquete r4np ----------------------------------------------

# Se necesita "devtools" para poder acceder a r4np, que no se encunetra en el repositorio oficial de R. El problema de pacman es que solo instala paquetes del repositorio oficial.

install.packages("devtools")

library(devtools)

install_github("ddauber/r4np")

library(r4np)

# Vamos a crear una función que calcule la mitad de un número

# Vamos a crear un objeto que contenga un número

pepito <- 5

# Creamos una función que calcula la mitad del valor

f_mitad <- function(x){
  y <- x/2
  return(y)
}


# Dentro de los corchetes le digo las funciones que tiene que utilizar R para pasar de un numero a su mitad.
# Se dice que a partir de una funcion x,
# este argumento va a estar dividido entre 2, y el objeto resultado es aquello que quiero que me devuelva

# Probar la función

f_mitad(pepito)

Los argumentos es aquello que se necesita para funcionar, por ejemplo dos argumentos en el caso sería la potencia.

f_potencia <- function(x,y){
  z <- x^y
  return(z)
}

f_potencia (pepito, pepito)
f_potencia (3, 4)

f_potencia(y = 3, x = 4)

library(tidyverse)

# Hay diferentes versiones de pipe: %>% o |> 

# Calcular la media de 1, 5 y 8 redondeando a dos decimales

v <- c(1,5,8)
media <- mean(v)
redondear <-  round(media, digits = 2)
redondear

c(1,5,8) %>% mean() %>% round(digits = 2)

mean(v) %>% round(2)


