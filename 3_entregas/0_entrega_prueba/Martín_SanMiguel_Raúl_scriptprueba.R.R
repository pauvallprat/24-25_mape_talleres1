
# Preliminares ------------------------------------------------------------

# Ctrl + Shift + R: crea una nueva sección dentro del script. 
# ctrl + shift + C: añade o quita la almohadilla en la línea seleccionada.
# Alt + guión: escribe una <- con espacio por delante y por detrás. 

# Instalamos el paquete pacman
install.packages("pacman")

# Cargamos pacman
library(pacman)
library(tidyverse)
p_load(titanic)

# La función "p_load" dentro de la librería "pacman" carga una librería y, 
# además, en el caso de no tener instalado el paquete necesario lo instala.

# Instalamos el paquete r4np
install.packages("devtools")
library(devtools)
install_github("ddauber/r4np")
library(r4np)

r4np::create_project_folder()

# Vamos a trabajar con funciones ------------------------------------------

# Crear un objeto que contenga un número

x <- 7

# Creamos una función que calcula la mitad del valor

f_mitad <- function(m){
  y <- m/2
  return(y)
}

# Probamos la función

f_mitad(x)
f_mitad(585)

# Creamos una función con dos argumentos

f_potencia <- function(a, b){
  y <- a^b
  return(y)
}

# Probamos la función

f_potencia(4, 5)
f_potencia(b=4, a=5)
f_potencia(345, 4)

# La media de un vector redondeada a dos decimales

media <- c(1, 5, 8)
mediamedia <- mean(media)
redondear <- round(mediamedia, digits=2)
redondear

# Más sencillo (con %>%)

c(1, 5, 8) %>% mean() %>% round(digits=2)
mean(media) %>% round(2)

