# Instalación de Paquetes
#install.packages("tidyverse")
#install.packages("janitor")

# Librerias 
#library(tidyverse)
#library(dplyr)
#library(janitor)


# Base de Datos
Homicidio_2015_2024 <- read.csv("C:/Users/monic/Documents/proyecto-probabilidad/Homicidios_2015_2024.csv")

head(Homicidio_2015_2024)
str(Homicidio_2015_2024)
summary(Homicidio_2015_2024)


# Cambio de Nombre de las Variables
# De formato <Año.del.hecho> a formato <año_del_hecho>
names(Homicidio_2015_2024)

Homicidio_2015_2024 <- Homicidio_2015_2024 %>%
    clean_names()

names(Homicidio_2015_2024)    


# Datos Nulos
nulos <- is.na(Homicidio_2015_2024)
head(nulos)

total_nulos <- sum(is.na(Homicidio_2015_2024))
print(total_nulos)

nulos_columna <- colSums(is.na(Homicidio_2015_2024))
print(nulos_columna) # No se presentan datos nulos


# Eliminación de Variables Innecesarias
Homicidio_2015_2024 <- Homicidio_2015_2024 %>%
    select(-id, -codigo_dane_departamento, -codigo_dane_municipio, 
            -grupo_de_edad_judicial, -manera_de_muerte, -pais_de_nacimiento,
            -diagnostico_topografico_de_la_lesion_fatal, -pueblo_indigena, 
            -ancestro_racial, -orientacion_sexual, -identidad_de_genero, 
            -transgenero, -estado_civil, -escolaridad, -pertenencia_grupal, 
            -pertenencia_etnica, -zona_del_hecho)
names(Homicidio_2015_2024)


# Simplificación
columnas_character <- Homicidio_2015_2024 %>%
    select_if(is.character)
names(columnas_character)

columnas_character <- columnas_character %>%
    select(-municipio_del_hecho_dane, -departamento_del_hecho_dane, -localidad_del_hecho)
names(columnas_character)

for (columna in names(columnas_character)){
    print(paste("---", columna, "---"))
    print(unique(Homicidio_2015_2024[[columna]]))
    }

Homicidio_2015_2024 <- Homicidio_2015_2024 %>%
    mutate(
        rango_de_hora_del_hecho_x_3_horas = gsub("\\(", "", rango_de_hora_del_hecho_x_3_horas), 
        rango_de_hora_del_hecho_x_3_horas = gsub("\\)", "", rango_de_hora_del_hecho_x_3_horas), 
        rango_de_hora_del_hecho_x_3_horas = tolower(rango_de_hora_del_hecho_x_3_horas), 

        dia_del_hecho = tolower(dia_del_hecho),
        mes_del_hecho = tolower(mes_del_hecho),

        grupo_mayor_menor_de_edad = case_when(
            grupo_mayor_menor_de_edad == "b) Mayores de Edad (>18 años)" ~ "b) Mayor de Edad (>18 Años)", 
            grupo_mayor_menor_de_edad == "a) Menores de Edad (<18 años)" ~ "a) Menor de Edad (<18 Años)", 
            TRUE ~ grupo_mayor_menor_de_edad),

        mecanismo_causal_de_la_lesion_fatal = case_when(
            mecanismo_causal_de_la_lesion_fatal == "Cortopunzante" ~ "Corto punzante",
            mecanismo_causal_de_la_lesion_fatal == "corto contundente" ~ "Corto contundente",
            mecanismo_causal_de_la_lesion_fatal == "Cortocontundente" ~ "Corto contundente",
            mecanismo_causal_de_la_lesion_fatal == "Proyectil de Arma de Fuego" ~ "Proyectil de arma de fuego",
            mecanismo_causal_de_la_lesion_fatal == "Agente o mecanismo explosivo" ~ "Mecanismo o agente explosivo",
            mecanismo_causal_de_la_lesion_fatal == "Agentes y mecanismo explosivo" ~ "Mecanismo o agente explosivo",
            mecanismo_causal_de_la_lesion_fatal == "Otros" ~ "Otro",
            mecanismo_causal_de_la_lesion_fatal == "Otra" ~ "Otro",
            TRUE ~ mecanismo_causal_de_la_lesion_fatal)
    )


# Convertir columnas a factor
Homicidio_2015_2024 <- Homicidio_2015_2024 %>%
    mutate_if(is.character, as.factor) %>% 
    mutate(
        dia_del_hecho = factor(
            dia_del_hecho, 
            levels = c("lunes", "martes", "miércoles", "jueves", "viernes", "sábado", "domingo")
        ), 
        
        mes_del_hecho = factor(
            mes_del_hecho, 
            levels = c("enero", "febrero", "marzo", "abril", "mayo", "junio", 
                        "julio", "agosto", "septiembre", "octubre", "noviembre", "diciembre")
        )
    )
str(Homicidio_2015_2024)







