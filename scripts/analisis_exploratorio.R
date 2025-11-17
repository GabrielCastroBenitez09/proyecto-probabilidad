# Paquetes
install.packages("ggthemes")
install.packages("viridis")


# Librerias
library(dplyr)
library(ggplot2)
library(ggthemes)
library(viridis)


# Base de Datos
homicidios <- readRDS("datos/Homicidios_Limpios_2015_2024.rds")
names(homicidios)

# Año del Hecho
ggplot(homicidios, aes(x = factor(ano_del_hecho), fill = factor(ano_del_hecho))) + 
    geom_bar(width = 0.7) + 
    theme_light() +
    scale_fill_viridis(discrete = TRUE, option = "D") + # Viridis escala automáticamente a 10 colores
    labs(x = "Año del Hecho", y = "Número de Homicidios") +
    theme(legend.position = "none") +
    scale_x_discrete(expand = expansion(add = 0.5)) +
    ggtitle("Homicidios por Año (2015 - 2024)")

# Mes del Hecho
homicidios %>%
    drop_na(dia_del_hecho) %>%
    ggplot(aes(x = mes_del_hecho, fill = mes_del_hecho)) + 
        geom_bar(width = 0.7) + 
        theme_light() +
        scale_fill_viridis(discrete = TRUE, option = "D") + # Viridis escala automáticamente a 10 colores
        labs(x = "Mes del Hecho", y = "Número de Homicidios") +
        theme(legend.position = "none") +
        scale_x_discrete(expand = expansion(add = 0.5)) +
        ggtitle("Homicidios por Mes")

# Día del Hecho
homicidios %>%
    drop_na(dia_del_hecho) %>%
    ggplot(aes(x = dia_del_hecho, fill = dia_del_hecho)) + 
        geom_bar(width = 0.7) + 
        theme_light() +s
        scale_fill_viridis(discrete = TRUE, option = "D") + # Viridis escala automáticamente a 10 colores
        labs(x = "Día de la Semana", y = "Número de Homicidios") +
        theme(legend.position = "none") +
        scale_x_discrete(expand = expansion(add = 0.5)) +
        ggtitle("Homicidios por Día de la Semana")

# Mecanismo Causal
ggplot(homicidios, aes(x = mecanismo_causal_de_la_lesion_fatal, fill = mecanismo_causal_de_la_lesion_fatal)) + 
    geom_bar(width = 0.7) + 
    theme_light() +
    coord_flip() +
    scale_fill_viridis(discrete = TRUE, option = "D") + # Viridis escala automáticamente a 10 colores
    labs(x = "Mecanismo Causal", y = "Número de Homicidios") +
    theme(legend.position = "none") +
    scale_x_discrete(expand = expansion(add = 0.5)) +
    ggtitle("Homicidios por Mecanismo Causal")
















