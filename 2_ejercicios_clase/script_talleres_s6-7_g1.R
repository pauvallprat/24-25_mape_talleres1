setwd("C:/Users/PT00A06D/Documents/Talleres 1/Sesion 6")


# Prelim ------------------------------------------------------------------
rm(list = ls())

library(pacman)
p_load(tidyverse,readxl,writexl,haven)


# Cargo los datos necesarios ----------------------------------------------

# Elecciones 2023
# Cargo la información electoral
elec23 <- read_xlsx("datos/202307_elec_congr_municipios.xlsx",
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
  ) %>% 
  # Necesito crear un Id único de municipio
  mutate(IdProv_c = str_pad(IdProv,width = 2,pad ="0"),
         IdMuni_c = str_pad(IdMuni,width = 3,pad ="0"),
         IdINE = paste0(IdProv_c,IdMuni_c))

# Cargar datos electorales de 2019
elec19 <- read_xlsx("datos/resultados_muni_congreso2019.xlsx")

# Cargar datos demografia 2019
demo19 <- read_dta("datos/datos_muni2019.dta")

# Cargar datos demografia 2023
demo23 <- read_rds("datos/demografia23.rds")

### 2. Combino resultados electorales con demografia

elec19 <- elec19 %>% 
  left_join(demo19,by = "IdINE")

elec23 <- elec23 %>% 
  left_join(demo23,by = "IdINE")

### 3. Verifico que los nombres de las variables sean iguales
setdiff(names(elec19),names(elec23))

setdiff(names(elec23),names(elec19))


### 4. Renombramos algunas variables para poder avanzar
elec19 <- elec19 %>% 
  rename(IdProv = IdProv.x,
         Prov = Prov.x,
         IdMuni = IdMuni.x)

elec23 <- elec23 %>% 
  rename(CUP = "CUP-PR")


### 5. Identificar el origen de los datos

elec23 <- elec23 %>% 
  mutate(year = 2023)

elec19 <- elec19 %>% 
  mutate(year = 2019)

### 6. Combinar

elec <- elec19 %>% 
  bind_rows(elec23)



# Regresiones -------------------------------------------------------------

# Modificamos la base de datos para tener nuevas variables
elec <- elec %>% 
  mutate(part = (Votantes/Censo)*100,
         paro_por = (Paro/Censo)*100,
         paro_f = if_else(paro_por<6.22,
                          0,1) %>% factor(labels = c("Bajo","Alto")),
         vtur_por = (VivFam-VivPrinc)/VivFam*100,
         vtur_f = if_else(vtur_por>=66,1,0) %>%  
           factor(labels = c("No turístico", "Turístico")))

### Regresión 
m1 <- lm(part~ vtur_f,data = elec|> filter(year==2019))

m2 <- lm(part~ paro_por+ vtur_f ,data = elec|> filter(year==2019))

# Regresión con interacción (2 dicotómicas)
m3 <- lm(part ~ paro_f*vtur_f, data = elec|> filter(year==2019))

# Regresión con interacción (1 numérica y 1 dicotómica)
m4 <- lm(part ~ paro_por*vtur_f, data = elec|> filter(year==2019))

# Regresión con interacció 2 continuas
m5 <- lm(part ~ paro_por*vtur_por, data = elec|> filter(year==2019))


p_load(broom)

tidy(m2)
glance(m2)

#### Representación de valores predichos
p_load(marginaleffects)

tidy(m1)
plot_predictions(m1,condition = "vtur_f")

tidy(m2)
plot_predictions(m2,condition = c("paro_por","vtur_f"))


# 2 dicotómicas
plot_predictions(m3,condition = c("vtur_f","paro_f"))+
  theme_minimal()


# Relación no lineal

m7 <- lm(part ~ paro_por + I(paro_por^2),data = elec)
tidy(m7)

plot_predictions(m7,condition = "paro_por")


# Estimación con efectos fijos
p_load(estimatr)

m8 <- lm_robust(part ~ paro_por,
                fixed_effects = year,
                data = elec)
tidy(m8)

m9 <- lm(part ~ paro_por,
         data = elec)

tidy(m9)
tidy(m8)
