# Preliminares ------------------------------------------------------------
rm(list = ls())
library(pacman)
p_load(tidyverse,readxl,haven)


# Datos elecciones 2023 ---------------------------------------------------
elec23 <- 
  # Cargar los datos
  read_xlsx("datos/02_202307_1.xlsx",
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
    CC = CCa,
    BNG = B.N.G.,
    UPN = U.P.N.
  ) |> 
  # Crear una variable que recoja los votos a otras candidaturas
  mutate(Otros = VotCand- PP - PSOE	- VOX	- SUMAR	- 
           ERC	- JxCAT	- BILDU	- PNV	- BNG	- CC	- UPN) |> 
  # Reubicar esta variable
  relocate(Otros,.after = UPN) |> 
  # Seleccionar las variables relevantes, hasta la variable PACMA
  select(1:26) |> # o which(names(elec23) == "PACMA"
  # Crear IdUnico para cada municipio
  mutate(
    IdProv_2 = str_pad(IdProv, width = 2, pad = "0"),
    IdMuni_3 = str_pad(IdMuni, width = 3, pad = "0"),
    IdINE = paste0(IdProv_2,IdMuni_3))

# Datos municipales 2023
muni23 <- read_rds(file = "datos/demografia23.rds") |> 
  select(-Pob)

# Combinar datos 2023
elec23 <- elec23 |> left_join(muni23,by = "IdINE")

# Datos elecciones 2019 ---------------------------------------------------

elec19 <- read_xlsx("datos/resultados_muni_congreso2019.xlsx")

# Demográficos
muni19 <- read_dta("datos/datos_muni2019.dta") |> 
  select(-c("CCAA","IdProv","Prov","IdMuni","Pob"))

# Combinar datos 2019
elec19 <- elec19 |> left_join(muni19,by = "IdINE")



setdiff(names(elec19), names(elec23)) # Names in df1 but not in df2
setdiff(names(elec23), names(elec19)) # Names in df2 but not in df1

# Combinar los datos de ambas elecciones: añadimos filas

elec <- elec19 |> 
  mutate(year = 2019) |>
  bind_rows(elec23 |> mutate(year = 2023))


# Regresión ejemplos QoG --------------------------------------------------

#### Datos QoG ####
qog <- read_dta("https://www.qogdata.pol.gu.se/data/qog_std_cs_jan23_stata14.dta")

### Democracia y esperanza de vida

qog |> 
  group_by(bmr_dem) |> 
  summarise(
    media_exp_vida = mean(wdi_lifexp, na.rm = TRUE))

### Pacificidad y colonialismo
qog |> 
  mutate(colony = if_else(ht_colonial==0,0,1) |> 
           factor(labels = c("Never colony","Colony"))) |> 
  group_by(colony) |> summarise(peace = mean(gpi_gpi, na.rm = TRUE))

qog |> 
  group_by(ht_colonial) |>
  summarise(peace = mean(gpi_gpi, na.rm = TRUE))


# Trabajamos con los datos ------------------------------------------------
# Creo variables de interés
elec <- elec |> 
  mutate(part = Votantes/Censo*100,
         paro_por = Paro/Pob*100,
         paro_f = factor(if_else(Paro<=mean(Paro,na.rm = T),0,1),
                       labels = c("Paro bajo","Paro alto")),
         vtur_por = (VivFam-VivPrinc)/VivFam*100,
         vtur_f = if_else(vtur_por>=66,1,0) |> 
           factor(labels = c("No turístico", "Turístico"))
         )

check <- elec |> 
  select(Muni,VivFam,VivPrinc,vtur_por,vtur_f)

# Regresión simple
r1 <- lm(part ~ paro_f, data = elec |> filter(year==2019))
summary(r1)

# Regresión múltiple
r2 <- lm(part ~ paro_f + vtur_f, data = elec|> filter(year==2019))
summary(r2)

r2b <- lm(part ~ paro_por + vtur_por, data = elec|> filter(year==2019))
summary(r2b)

# Regresión con interacción (2 dicotómicas)
r3 <- lm(part ~ paro_f*vtur_f, data = elec|> filter(year==2019))
summary(r3)

# Regresión con interacción (1 numérica y 1 dicotómica)
r4 <- lm(part ~ paro_por*vtur_f, data = elec|> filter(year==2019))
summary(r4)

# Regresión
r5 <- lm(part ~ paro_por*vtur_por, data = elec|> filter(year==2019))
summary(r5)

# Tablas de regresiones ---------------------------------------------------
#### Broom ####
p_load(broom)

# Create regression tables for r1 to r5
for (i in 1:5) {
  r <- get(paste0("r", i))
  tidy(r) |> print()
}


#### Stargazer ####
p_load(stargazer)

# Create table with stargazer
stargazer(r1,r2,r2b,r3,r4,r5,type = "text",style = "apsr")


# Graficar interacciones --------------------------------------------------
#### jtools ####
p_load(jtools)

# Categorical
effect_plot(r2,pred = paro_f,
            int.width = .90,cat.pred.point.size = 2.5,line.thickness = .2,
            plot.points = T,jitter = .3,point.alpha = .2,point.size = .2)

# Continuous
effect_plot(r2b,pred = paro_por,plot.points = T,point.alpha = .2,point.size = .2)

# Graficar coeficientes de varios modelos
plot_coefs(r1,r2,r2b,r3,r4,r5,
           model.names = c("r1","r2","r2b","r3","r4","r5"))

plot_summs(r1,r2,r2b,r3,r4,r5,
           model.names = c("r1","r2","r2b","r3","r4","r5"),
           inner_ci_level = .9)

#### coefplot ####
p_load(coefplot)

coefplot(r5, intercept = F)

multiplot(r1,r2,r2b,r3,r4,r5, intercept = F,innerCI = 1.96,outerCI = 2.58)



#### ggeffects ####
p_load(ggeffects)

pred <- ggpredict(r3,ci_level = .9,terms = c("paro_f", "vtur_f"))

plot(pred)+ 
  labs(title = "Interacción entre paro y viviendas turísticas sobre participación",
       color = "Viviendas turísticas",x="Paro",y="Participación") + 
  theme_minimal()


pred <- ggpredict(r5,ci_level = .9,terms = c("paro_por", "vtur_por"))

plot(pred)+ 
  labs(title = "Interacción entre paro y viviendas turísticas sobre participación",
       color = "Viviendas turísticas",x="Paro",y="Participación") + 
  theme_minimal()


#### marginaleffects ####
p_load(marginaleffects)

# 2 dicotómicas
plot_predictions(r3,condition = c("paro_f","vtur_f"))
# 1 dicotómica y 1 continua
plot_predictions(r4,condition = c("paro_por","vtur_f"))
# 2 continuas
plot_predictions(r5,condition = c("paro_por","vtur_por"))




# Regresiones con efectos fijos -------------------------------------------
p_load(estimatr)
r5_fe_p <- lm_robust(part ~ paro_por*vtur_por,
                  fixed_effects = year + Prov,
                  # clusters = Prov,
                  data = elec)

summary(r5_fe_p)

# Para graficarlo hay que plantear la estimación de un modo distinto
r5_fe_p <- lm_robust(
  part ~ paro_por * vtur_por + factor(year) + factor(Prov),  # Explicit fixed effects
  data = elec
)

# Now graph
plot_predictions(r5_fe_p,condition = c("paro_por", "vtur_por"))




# Relaciones no lineales --------------------------------------------------

elec <- elec |> 
  mutate(
    paro_cuad = paro_por^2,
    vtur_cuad = vtur_por^2
  )

# Regresión
r6 <- lm(part ~ vtur_por + I(vtur_por^2), data = elec |> filter(year == 2019))
summary(r6)

plot_predictions(r6,condition = c("vtur_por"))


# Old ---------------------------------------------------------------------




# Regresión
r5 <- lm(part ~ paro_por*vtur_por, data = elec|> filter(year==2019))
summary(r5)


# Tablas de regresiones ---------------------------------------------------
# Broom
p_load(broom)

tidy(r5) # tabla de regresión
glance(r5) # r2 y demás
augment(r5) # tabla con información de todas las observaciones

# Stargazer
p_load(stargazer)
stargazer(r1,r2a,r2b,r3,r4,r5,type = "html",style = "apsr")

stargazer(r1,r2a,r2b,r3,r4,r5,digits = 2,p = c(.1,.05,.01,.001))

  # también nos ayuda a presentar estadísticos descriptivos
  e19_sum <- elec19 |> 
    select(part,paro,mtur,por_paro,por_vtur)
  stargazer(e19_sum,type = "text",summary = T)
  
  p_load(skimr)
  elec19 |> 
    select(part,paro,mtur,por_paro,por_vtur) |> 
    skim()
  
  p_load(vtable)
  elec19 |> 
    select(part,paro,mtur,por_paro,por_vtur) |> 
  sumtable()


# Graficar interacciones --------------------------------------------------
# jtools
p_load(jtools)
  
effect_plot(r2b,pred = por_paro,plot.points = T) # para graficar valores predichos
plot_summs(r2b,r5,
           inner_ci_level = .9) # para graficar efectos marginales
  
# Coefplot
p_load(coefplot)

coefplot(r5, intercept = F)

multiplot(r2b,r5,intercept = F) +
  labs(x = "")

# ggpredict
p_load(ggeffects)

p_r5 <- ggpredict(r5,terms = c("por_paro", "por_vtur"))
p_r5
plot(p_r5)+ labs(color="% Viv. Turisticas")

# Marginaleffects 
p_load(marginaleffects)

# Interacción simple (2 binarias)
summary(r3)
plot_predictions(r3,by = "paro")
plot_predictions(r3,by = "mtur")
plot_predictions(r3,by = c("paro","mtur"))

# Interacción (1 cont. 1 binaria)
summary(r4)
plot_predictions(r4,condition = c("por_paro","mtur"))

# Interacción (1 cont. 1 binaria)
summary(r5)
plot_predictions(r5,condition = c("por_paro","por_vtur"))



# 
# Datos QoG
qog <- read_dta("https://www.qogdata.pol.gu.se/data/qog_std_cs_jan23_stata14.dta")

# Tabla
table(qog$ht_colonial)

qog <- qog |> 
  mutate(peace = (gpi_gpi*-1)+4,
         colony = factor(if_else(ht_colonial==0,0,1),
                         labels = c("Never colony","Colony")),
         democracy = factor(bmr_dem,
                            labels = c("Autocracy","Democracy")))
qog  |> 
  group_by(colony) |> 
  summarise(peace = mean(gpi_gpi, na.rm = TRUE))

# Simple regression
r1 <- lm(peace ~ colony, data = qog)
summary(r1)

# Multiple regression
r2 <- lm(peace ~ colony*democracy, data = qog)
summary(r2)

plot_predictions(r2,by = c("colony","democracy"))


glance(r1)
