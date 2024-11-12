# Preliminares ------------------------------------------------------------
rm(list = ls())

# Libraries
library(pacman)
p_load(readxl,tidyverse)


# Cargar los datos de las elecciones de 2023 ------------------------------

# No carga los datos correctamente
elec23 <- read_xlsx("1_datos/202307_elec_congr_municipios.xlsx")

# Hay que hacer algunos ajustes
elec23 <- 
  # Cargar los datos
  read_xlsx("1_datos/202307_elec_congr_municipios.xlsx",
            # Especificando el rango de celdas
            range = "A6:BT8137") |> 
  # Cambiar el nombre de las variables
  rename(
    CA = `Nombre de Comunidad`,
    IdProv = `Código de Provincia`,
    Prov = `Nombre de Provincia`,
    IdMuni = `Código de Municipio`,
    Muni = `Nombre de Municipio`,
    Pob = Población,
    Mesas = `Número de mesas`,
    Censo = `Total censo electoral`,
    Votantes = `Total votantes`,
    VotVal = `Votos válidos`,
    VotCand = `Votos a candidaturas`,
    VotBl = `Votos en blanco`,
    VotNul = `Votos nulos`,
    JxCAT = `JxCAT - JUNTS`,
    BILDU = `EH Bildu`,
    PNV = `EAJ-PNV`,
    BNG = B.N.G.,
    UPN = U.P.N.
  ) |> 
  # Crear una variable que recoja los votos a otras candidaturas
  mutate(Otros = VotCand- PP - PSOE	- VOX	- SUMAR	- 
           ERC	- JxCAT	- BILDU	- PNV	- BNG	- CCa	- UPN) |> 
  # Reubicar esta variable
  relocate(Otros,.after = UPN) |> 
  # Seleccionar las variables relevantes, hasta la variable PACMA
  select(1:which(names(elec23) == "PACMA")) # o 1:26