
# Preliminares ------------------------------------------------------------

#Este título se pone con Ctrl + Mayus + r
install.packages("pacman")
#con crtl + mayus + c se comenta la línea automáticamente (y dejan de funcionar como código)
# cargamos pacman:
library(pacman)
# descargamos E instalamos paquetes usando la función p_load de pacman
p_load(tidyverse, Titanic)
# Es conveniente mantener el orden dentro del proyecto, con carpetas para cada tipo de documentación y nombres de ficheros lógicos y ordenados.
# Además, está bien guardar todos los códigos de manera estructurada.
# Una opción es usar el paquete r4np, que crea un proyecto y carpetas para mantener el orden.
install.packages("devtools")
devtools::install_github("ddauber/r4np")
library(r4np)
r4np::create_project_folder()
# Es crucial pensar antes de escribir código. Es un medio de comunicación entre uno y el ordenador, uno y otra gente, y uno y su yo futuro.
#Conviene comentar código para incluir anotaciones y aclaraciones.
#R se basa completamente en objetos, y cualquier dato forma parte de un objeto. Todo elemento en R tiene un nombre.
#Conviene que los nombres tengan consistencia y recognoscibilidad, además de brevedad.
# Todo objeto tiene una clase: character, number, matrix, integer...
# Creamos un objeto que contenga un número 
pepito <- 7
# Creamos una función que calcula la mitad del valor
f_mitad <- function(x) {y <- x/2
return(y)} # Alt + - es directamente la flechita

# Probamos la función
f_mitad(pepito)
f_mitad(33)
# hacemos otra función pero esta vez con dos argumentos
f_potencia <- function(x, z) {y <- x^z
return(y)}
juanito <- 3
f_potencia(7, 3)

library(tidyverse)

numeros <- c(1, 5, 8)
media <- numeros %>% 
  mean() %>%
  round(2)
print(media)

c(1, 5, 8) %>%  mean() %>% round(2)