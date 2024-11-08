# Preliminares ------------------------------------------------------------
# Instalamos el paquete pacman
# install.packages("pacman")

# Cargamos pacman
library(pacman)

# Para mirar si el paquete está o no instalado. Si no está instalado lo instala, si está instalado lo carga.
p_load(tidyverse,titanic)

# Instalamos el paquete r4np
# install.packages("devtools")
library(devtools)
# install_github("ddauber/r4np")
library(r4np)

r4np::create_project_folder()


# Vamos a trabajar con funciones ------------------------------------------

# Crear un objeto que contenga un número

pepito<-7

# Creamos una función que calcula la mitad del valor

f_mitad <- function(x){y <- x/2 
return(y)}

# Probar la función
f_mitad(pepito)

f_potencia <- function(a,b){y <- a^b 
return(y)}

f_potencia(7,7)
f_potencia(b=5,a=8)
f_potencia(pepito,365)


# Usamos pipes ------------------------------------------------------------

# Calcular la media de 1,5,8 y redondeado a 2 decimales
v <- c(1,5,8)
media <- mean(v)
redondear <- round(media,digits=2)
redondear
c(1,5,8) %>% mean() %>% round(digits=2)
