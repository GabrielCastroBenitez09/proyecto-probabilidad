# Paquetes
install.packages(tidyverse)
install.packages(glue)


# Librerias
library(tidyr)
library(dplyr)
library(forcats)
library(glue)


# Base de Datos 
homicidios <- readRDS("datos/Homicidios_Limpios_2015_2024.rds")


# Promedio de Homicidios por año
# 2015 - 2024
homicidios_por_ano <- homicidios %>%
    group_by(ano_del_hecho) %>%
    summarise(homicidios_totales = n())

promedio_anual <- mean(homicidios_por_ano$homicidios_totales, na.rm = TRUE)

# Posterior al Covid-19
promedio_anual_post_covid <- homicidios %>%
    filter(ano_del_hecho >= 2021) %>%
    group_by(ano_del_hecho) %>%
    summarise(homicidios_totales = n()) %>%
    summarise(promedio_anual_21_24 = mean(homicidios_totales, na.rm = TRUE)) %>%
    pull(promedio_anual_21_24)

print(c("Promedio Anual (2015 - 2024)" = promedio_anual, 
        "Promedio Anual Posterior al Covid-19" = promedio_anual_post_covid))
