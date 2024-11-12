# Preliminares ------------------------------------------------------------
# Instalamos el paquete pacman
install.packages("pacman")

#Cargamos Pacman
library(pacman)

p_load(tidyverse,titanic)

# instalamos el paquete r4np
install.packages("devtools")
library(devtools)
install_github(repo = "ddauber/r4np")
library(r4np)

r4np::create_project_folder()
