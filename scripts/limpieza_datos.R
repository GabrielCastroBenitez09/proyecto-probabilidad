# Instalación de Paquetes
install.packages("tidyverse")
install.packages("janitor")

# Librerias 
library(tidyverse)
library(dplyr)
library(janitor)
library(stringr)


# Base de Datos
Homicidio_2015_2024 <- read.csv("datos/Homicidios_2015_2024.csv")

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
    select(-municipio_del_hecho_dane)
names(columnas_character)

for (columna in names(columnas_character)){
    print(paste("---", columna, "---"))
    print(unique(Homicidio_2015_2024[[columna]]))
    }
table(Homicidio_2015_2024$presunto_agresor_detallado)

Homicidio_2015_2024 <- Homicidio_2015_2024 %>%
    mutate(
        escenario_del_hecho = as.character(escenario_del_hecho), 
        escenario_del_hecho = tolower(escenario_del_hecho),
        escenario_del_hecho = gsub("\\s*\\(.*\\)", "", escenario_del_hecho),
        escenario_del_hecho = str_trim(escenario_del_hecho),

        actividad_durante_el_hecho = case_when(
            actividad_durante_el_hecho == "Actividades de desplazamiento de un lugar a otro." ~ 
            "Actividades de desplazamiento de un lugar a otro",

            actividad_durante_el_hecho == "Actividades relacionadas con los deportes y el ejercicio físico." ~ 
            "Actividades relacionadas con los deportes y el ejercicio físico",

            actividad_durante_el_hecho == "Actividades relacionadas con manifestaciones públicas (Marchas, protestas, etc)" ~ 
            "Actividades relacionadas con manifestaciones públicas (marchas, protestas, etc)",

            actividad_durante_el_hecho == "Actividades relacionadas con manifestaciones públicas (Marchas, protestas, etc.)" ~ 
            "Actividades relacionadas con manifestaciones públicas (marchas, protestas, etc)",

            actividad_durante_el_hecho == "" ~ "",
            actividad_durante_el_hecho == "" ~ "",
            TRUE ~ actividad_durante_el_hecho
        ),

        presunto_agresor_detallado = case_when(
            presunto_agresor_detallado == "Abuelo (a)" ~ "Abuelo(a)",
            presunto_agresor_detallado == "Amigo" ~ "Amigo(a)",
            presunto_agresor_detallado == "Amigo (a)" ~ "Amigo(a)",
            presunto_agresor_detallado == "Barra(s) futboleta(s)" ~ "Barra(s) futbolera(s)",
            presunto_agresor_detallado == "Barra(s) futboleras" ~ "Barra(s) futbolera(s)",
            presunto_agresor_detallado == "Compañero (a)  permanente" ~ "Compañero(a) permanente",
            presunto_agresor_detallado == "Compañero (a) permanente" ~ "Compañero(a) permanente",
            presunto_agresor_detallado == "Compañero (a) de trabajo" ~ "Compañero(a) de trabajo",
            presunto_agresor_detallado == "Conocido sin ningun trato" ~ "Conocido sin ningún trato",
            presunto_agresor_detallado == "Cuñado (a)" ~ "Cuñado(a)",
            presunto_agresor_detallado == "Eln" ~ "ELN",
            presunto_agresor_detallado == "Empleado (a)" ~ "Empleado(a)",
            presunto_agresor_detallado == "Esposo" ~ "Esposo(a)",
            presunto_agresor_detallado == "Esposo (a)" ~ "Esposo(a)",
            presunto_agresor_detallado == "Ex - Amante" ~ "Ex amante",
            presunto_agresor_detallado == "Ex - compañero (a) permanente" ~ "Ex compañero(a) permanente",
            presunto_agresor_detallado == "Ex compañero (a) permanente" ~ "Ex compañero(a) permanente",
            presunto_agresor_detallado == "Ex - esposo (a)" ~ "Ex esposo(a)",
            presunto_agresor_detallado == "Ex esposo (a)" ~ "Ex esposo(a)",
            presunto_agresor_detallado == "Ex - Novio (a)" ~ "Ex novio(a)",
            presunto_agresor_detallado == "Ex novio (a)" ~ "Ex novio(a)",
            presunto_agresor_detallado == "Hermano (a)" ~ "Hermano(a)",
            presunto_agresor_detallado == "Hijo (a)" ~ "Hijo(a)",
            presunto_agresor_detallado == "Miembro de seguridad privada" ~ " Miembros de seguridad privada",
            presunto_agresor_detallado == "Novio (a)" ~ "Novio(a)",
            presunto_agresor_detallado == "Otras Guerrillas" ~ "Otras guerrillas",
            presunto_agresor_detallado == "Otros familiares civiles y consanguíneos" ~ "Otros familiares civiles o consanguíneos",
            presunto_agresor_detallado == "Otros familiares civiles o consanguineos" ~ "Otros familiares civiles o consanguíneos",
            presunto_agresor_detallado == "Paramilitares" ~ "Paramilitares - Autodefensas",
            presunto_agresor_detallado == "Primo (a)" ~ "Primo(a)",
            presunto_agresor_detallado == "Profesor (a)" ~ "Profesor(a)",
            presunto_agresor_detallado == "Suegro (a)" ~ "Suegro(a)",
            presunto_agresor_detallado == "Tío (a)" ~ "Tío(a)",
            TRUE ~ presunto_agresor_detallado
        )
    )

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
            TRUE ~ mecanismo_causal_de_la_lesion_fatal),
        grupo_de_edad_quinquenal = case_when(
            grupo_de_edad_quinquenal == "Por determinar" ~ "Sin información",
            TRUE ~ grupo_de_edad_quinquenal
        ),

        departamento_del_hecho_dane = case_when(
            departamento_del_hecho_dane == "Bogotá, D.C." ~ "Bogotá D.C.",
            departamento_del_hecho_dane == "Quindio" ~ "Quindío",
            TRUE ~ departamento_del_hecho_dane
        )
    )


# Convertir columnas a factor
Homicidio_2015_2024 <- Homicidio_2015_2024 %>%
    mutate_if(is.character, as.factor) %>% 
    mutate(
        escenario_del_hecho = case_when(
            escenario_del_hecho == "parqueaderos,estacionamientos" ~ "parqueaderos, estacionamientos",
            TRUE ~ escenario_del_hecho
        ),
        dia_del_hecho = factor(
            dia_del_hecho, 
            levels = c("lunes", "martes", "miércoles", "jueves", "viernes", "sábado", "domingo")
        ), 
        
        mes_del_hecho = factor(
            mes_del_hecho, 
            levels = c("enero", "febrero", "marzo", "abril", "mayo", "junio", 
                        "julio", "agosto", "septiembre", "octubre", "noviembre", "diciembre")
        ),

        edad_numerica = case_when(
            grupo_de_edad_quinquenal == "(00 a 04)" ~ 2.5,
            grupo_de_edad_quinquenal == "(05 a 09)" ~ 7.5,
            grupo_de_edad_quinquenal == "(10 a 14)" ~ 12.5,
            grupo_de_edad_quinquenal == "(15 a 17)" ~ 16.0, 
            grupo_de_edad_quinquenal == "(18 a 19)" ~ 18.5, 
            grupo_de_edad_quinquenal == "(20 a 24)" ~ 22.0,
            grupo_de_edad_quinquenal == "(25 a 29)" ~ 27.0,
            grupo_de_edad_quinquenal == "(30 a 34)" ~ 32.0,
            grupo_de_edad_quinquenal == "(35 a 39)" ~ 37.0,
            grupo_de_edad_quinquenal == "(40 a 44)" ~ 42.0,
            grupo_de_edad_quinquenal == "(45 a 49)" ~ 47.0,
            grupo_de_edad_quinquenal == "(50 a 54)" ~ 52.0,
            grupo_de_edad_quinquenal == "(55 a 59)" ~ 57.0,
            grupo_de_edad_quinquenal == "(60 a 64)" ~ 62.0,
            grupo_de_edad_quinquenal == "(65 a 69)" ~ 67.0,
            grupo_de_edad_quinquenal == "(70 a 74)" ~ 72.0,
            grupo_de_edad_quinquenal == "(75 a 79)" ~ 77.0,
            grupo_de_edad_quinquenal == "(80 y más)" ~ 82.5, 
            grupo_de_edad_quinquenal == "Sin información" ~ NA_real_,
            grupo_de_edad_quinquenal == "Por determinar" ~ NA_real_,
            TRUE ~ NA_real_)    
    )
str(Homicidio_2015_2024)
table(Homicidio_2015_2024$escenario_del_hecho)

# Guardar la base de datos limpia 
write.csv(Homicidio_2015_2024, "datos/Homicidios_Limpios_2015_2024.csv", row.names = FALSE)
saveRDS(Homicidio_2015_2024, "datos/Homicidios_Limpios_2015_2024.rds")




