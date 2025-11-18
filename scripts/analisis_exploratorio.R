# Paquetes
install.packages("ggthemes")
install.packages("viridis")


# Librerias
library(tidyr)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(viridis)
library(forcats)


# Base de Datos
homicidios <- readRDS("datos/Homicidios_Limpios_2015_2024.rds")
names(homicidios)
table(homicidios$escenario_del_hecho)

# Análisis
# Año del Hecho
table(homicidios$ano_del_hecho)



# Mes del Hecho
table(homicidios$mes_del_hecho)



# Día del Hecho
table(homicidios$dia_del_hecho)



# Mecanismo Causal


# Grupo de Edad
table(homicidios$grupo_de_edad_quinquenal)



# Grupo Mayor/Menor de Edad


# Localidad (Bogotá)


# Departamento 
table(homicidios$departamento_del_hecho_dane)




# Sexo
table(homicidios$sexo_de_la_victima)



# Actividad Durante el Hecho




