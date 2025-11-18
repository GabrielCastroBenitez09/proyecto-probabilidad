# Paquetes
install.packages("ggthemes")
install.packages("viridis")
install.packages("glue")


# Librerias
library(tidyr)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(viridis)
library(forcats)
library(glue)


# Base de Datos
homicidios <- readRDS("datos/Homicidios_Limpios_2015_2024.rds")
names(homicidios)
table(homicidios$escenario_del_hecho)

# Análisis
# Año del Hecho
table(homicidios$ano_del_hecho)

homicidios_por_año <- homicidios %>%
    group_by(ano_del_hecho) %>%
    summarise(conteo_homicidios = n(), .groups = 'drop') %>%
    mutate(ano_del_hecho = as.numeric(as.character(ano_del_hecho)))

ggplot(homicidios_por_año, aes(x = ano_del_hecho, y = conteo_homicidios)) + 
    geom_line(color = "darkblue", size = 1) + 
    geom_point(color = "darkblue", size = 3) +
    geom_point(size = 2) +
    geom_text(aes(label = conteo_homicidios), vjust = -0.8, size = 3, check_overlap = TRUE) + 
    theme_light() +
    labs(x = "Año del Hecho", y = "Número de Homicidios") +
    scale_x_continuous(breaks = unique(homicidios_por_año$ano_del_hecho)) +
    ggtitle("Homicidios por Año (2015 - 2024)")

conteo <- length(homicidios$ano_del_hecho)
sin_informacion <- sum(homicidios$ano_del_hecho == "Sin información", na.rm = TRUE)
print(c("Conteo" = conteo, "Sin Información" = sin_informacion))
porcentaje <- sin_informacion/conteo*100 
porcentaje <- round(porcentaje, 2)
print(glue("Los casos sin información representan el {porcentaje}%"))

# Mes del Hecho
table(homicidios$mes_del_hecho)

homicidios %>%
    drop_na(dia_del_hecho) %>%
    ggplot(aes(x = mes_del_hecho, fill = mes_del_hecho)) + 
        geom_bar(width = 0.7) + 
        theme_light() +
        scale_fill_viridis(discrete = TRUE, option = "D") + 
        labs(x = "Mes del Hecho", y = "Número de Homicidios") +
        theme(legend.position = "none") +
        scale_x_discrete(expand = expansion(add = 0.5)) +
        ggtitle("Homicidios por Mes")

conteo <- length(homicidios$mes_del_hecho)
sin_informacion <- sum(homicidios$mes_del_hecho == "Sin información", na.rm = TRUE)
print(c("Conteo" = conteo, "Sin Información" = sin_informacion))
porcentaje <- sin_informacion/conteo*100 
porcentaje <- round(porcentaje, 2)
print(glue("Los casos sin información representan el {porcentaje}%"))

# Día del Hecho
table(homicidios$dia_del_hecho)

homicidios %>%
    drop_na(dia_del_hecho) %>%
    ggplot(aes(x = dia_del_hecho, fill = dia_del_hecho)) + 
        geom_bar(width = 0.7) + 
        theme_light() +
        scale_fill_viridis(discrete = TRUE, option = "D") + 
        labs(x = "Día de la Semana", y = "Número de Homicidios") +
        theme(legend.position = "none") +
        scale_x_discrete(expand = expansion(add = 0.5)) +
        ggtitle("Homicidios por Día de la Semana")

conteo <- length(homicidios$dia_del_hecho)
sin_informacion <- sum(homicidios$dia_del_hecho == "Sin información", na.rm = TRUE)
print(c("Conteo" = conteo, "Sin Información" = sin_informacion))
porcentaje <- sin_informacion/conteo*100 
porcentaje <- round(porcentaje, 2)
print(glue("Los casos sin información representan el {porcentaje}%"))

# Mecanismo Causal
table(homicidios$mecanismo_causal_de_la_lesion_fatal)

ggplot(homicidios, aes(x = fct_rev(fct_infreq(mecanismo_causal_de_la_lesion_fatal)), 
    fill = mecanismo_causal_de_la_lesion_fatal)) + 
    geom_bar(width = 0.7) + 
    theme_light() +
    coord_flip() +
    scale_fill_viridis(discrete = TRUE, option = "D") + 
    labs(x = "Mecanismo Causal", y = "Número de Homicidios") +
    theme(legend.position = "none") +
    scale_x_discrete(expand = expansion(add = 0.5)) +
    ggtitle("Homicidios por Mecanismo Causal")

conteo <- length(homicidios$mecanismo_causal_de_la_lesion_fatal)
sin_informacion <- sum(homicidios$mecanismo_causal_de_la_lesion_fatal == "Por determinar", na.rm = TRUE)
print(c("Conteo" = conteo, "Sin información" = sin_informacion))
porcentaje <- sin_informacion/conteo*100 
porcentaje <- round(porcentaje, 2)
print(glue("Los casos sin información representan el {porcentaje}%"))

# Grupo de Edad
table(homicidios$grupo_de_edad_quinquenal)

ggplot(homicidios %>% filter(grupo_de_edad_quinquenal != "Sin información"), 
    aes(x = grupo_de_edad_quinquenal, 
    fill = grupo_de_edad_quinquenal)) + 
    geom_bar(width = 0.7) + 
    theme_light() +
    scale_fill_viridis(discrete = TRUE, option = "D") + 
    labs(x = "Grupo de Edad Quinquenal", y = "Número de Homicidios",
         caption = "Excluye: casos sin información.") +
    theme(legend.position = "none") +
    scale_x_discrete(expand = expansion(add = 0.5)) +
    ggtitle("Homicidios por Grupo de Edad") +
    theme(axis.text.x = element_text(size = 8, angle = 45, hjust = 1, vjust = 1))

conteo <- length(homicidios$grupo_de_edad_quinquenal)
sin_informacion <- sum(homicidios$grupo_de_edad_quinquenal == "Sin información", na.rm = TRUE)
print(c("Conteo" = conteo, "Sin Información" = sin_informacion))
porcentaje <- sin_informacion/conteo*100 
porcentaje <- round(porcentaje, 2)
print(glue("Los casos sin información representan el {porcentaje}%"))

# Grupo Mayor/Menor de Edad
table(homicidios$grupo_mayor_menor_de_edad)

ggplot(homicidios %>% filter(grupo_mayor_menor_de_edad != "c) Sin información"), 
    aes(x = grupo_mayor_menor_de_edad, fill = grupo_mayor_menor_de_edad)) + 
    geom_bar(width = 0.5) + 
    theme_light() +
    scale_fill_viridis(discrete = TRUE, option = "D") + 
    labs(x = "Grupo de Edad", y = "Número de Homicidios",
         caption = "Excluye: casos sin información.") +
    theme(legend.position = "none") +
    scale_x_discrete(expand = expansion(add = 0.5)) +
    ggtitle("Homicidios por Grupo de Edad (Mayor/Menor de Edad)")

conteo <- length(homicidios$grupo_mayor_menor_de_edad)
sin_informacion <- sum(homicidios$grupo_mayor_menor_de_edad == "c) Sin información", na.rm = TRUE)
print(c("Conteo" = conteo, "Sin Información" = sin_informacion))
porcentaje <- sin_informacion/conteo*100 
porcentaje <- round(porcentaje, 2)
print(glue("Los casos sin información representan el {porcentaje}%"))

# Localidad (Bogotá)
ggplot(homicidios %>% filter(localidad_del_hecho != "Sin información"), 
    aes(x = fct_rev(fct_infreq(localidad_del_hecho)), fill = localidad_del_hecho)) + 
    geom_bar(width = 0.5) + 
    theme_light() +
    coord_flip() +
    scale_fill_viridis(discrete = TRUE, option = "D") + 
    labs(x = "Localidad", y = "Número de Homicidios",
         caption = "Excluye: casos sin información.") +
    theme(legend.position = "none") +
    scale_x_discrete(expand = expansion(add = 0.5)) +
    ggtitle("Homicidios por Localidad (Bogotá)")

homicidios_bogota <- homicidios %>% filter(departamento_del_hecho_dane == "Bogotá D.C.")
conteo_homicidios_bogota <- length(homicidios_bogota$localidad_del_hecho)
sin_informacion <- sum(homicidios_bogota$localidad_del_hecho == "Sin información", na.rm = TRUE)
print(c("Conteo" = conteo_homicidios_bogota, "Sin Información" = sin_informacion))
porcentaje <- (sin_informacion/conteo_homicidios_bogota)*100
print(glue("Los casos sin información representan el {round(porcentaje, 2)}%."))

# Departamento 
table(homicidios$departamento_del_hecho_dane)

ggplot(homicidios %>% filter(departamento_del_hecho_dane != "Sin información"),
    aes(x = fct_rev(fct_infreq(departamento_del_hecho_dane)), 
    fill = departamento_del_hecho_dane)) + 
    geom_bar(width = 0.5) + 
    theme_light() +
    coord_flip() +
    scale_fill_viridis(discrete = TRUE, option = "D") + 
    labs(x = "Departamento", y = "Número de Homicidios",
         caption = "Excluye: casos sin información.") +
    theme(legend.position = "none") +
    scale_x_discrete(expand = expansion(add = 0.5)) +
    ggtitle("Homicidios por Departamento")

conteo <- length(homicidios$departamento_del_hecho_dane)
sin_informacion <- sum(homicidios$departamento_del_hecho_dane == "Sin información", na.rm = TRUE)
print(c("Conteo" = conteo, "Sin Información" = sin_informacion))
porcentaje <- sin_informacion/conteo*100 
porcentaje <- round(porcentaje, 2)
print(glue("Los casos sin información representan el {porcentaje}%"))

# Sexo
table(homicidios$sexo_de_la_victima)

ggplot(homicidios %>% filter(sexo_de_la_victima != "Indeterminado"),
    aes(x = sexo_de_la_victima, fill = sexo_de_la_victima)) + 
    geom_bar(width = 0.5) + 
    theme_light() +
    coord_flip() +
    scale_fill_viridis(discrete = TRUE, option = "D") + 
    labs(x = "Sexo", y = "Número de Homicidios",
         caption = "Excluye: casos sin información.") +
    theme(legend.position = "none") +
    scale_x_discrete(expand = expansion(add = 0.5)) +
    ggtitle("Homicidios por Sexo")

conteo <- length(homicidios$sexo_de_la_victima)
sin_informacion <- sum(homicidios$sexo_de_la_victima == "Indeterminado", na.rm = TRUE)
print(c("Conteo" = conteo, "Sin Información" = sin_informacion))
porcentaje <- sin_informacion/conteo*100 
porcentaje <- round(porcentaje, 2)
print(glue("Los casos sin información representan el {porcentaje}%"))

# Actividad Durante el Hecho
ggplot(homicidios %>% filter(actividad_durante_el_hecho != "Sin información"),
    aes(x = fct_rev(fct_infreq(actividad_durante_el_hecho)), fill = actividad_durante_el_hecho)) + 
    geom_bar(width = 0.5) + 
    theme_light() +
    coord_flip() +
    scale_fill_viridis(discrete = TRUE, option = "D") + 
    labs(x = "Escenario", y = "Número de Homicidios",
         caption = "Excluye: casos sin información.") +
    theme(legend.position = "none") +
    scale_x_discrete(expand = expansion(add = 0.5)) 

conteo <- length(homicidios$actividad_durante_el_hecho)
sin_informacion <- sum(homicidios$actividad_durante_el_hecho == "Sin información", 
                       homicidios$actividad_durante_el_hecho == "Otra", na.rm = TRUE)
print(c("Conteo" = conteo, "Sin Información" = sin_informacion))
porcentaje <- sin_informacion/conteo*100 
porcentaje <- round(porcentaje, 2)
print(glue("Los casos sin información representan el {porcentaje}%"))

# Escenario del Hecho
ggplot(homicidios %>% filter(escenario_del_hecho != "sin información"),
    aes(x = fct_rev(fct_infreq(escenario_del_hecho)), fill = escenario_del_hecho)) + 
    geom_bar(width = 0.5) + 
    theme_light() +
    coord_flip() +
    scale_fill_viridis(discrete = TRUE, option = "D") + 
    labs(x = "Actividad", y = "Número de Homicidios",
         caption = "Excluye: casos sin información.") +
    theme(legend.position = "none") +
    scale_x_discrete(expand = expansion(add = 0.5)) 

conteo <- length(homicidios$escenario_del_hecho)
sin_informacion <- sum(homicidios$escenario_del_hecho == "sin información", na.rm = TRUE)
print(c("Conteo" = conteo, "Sin Información" = sin_informacion))
porcentaje <- sin_informacion/conteo*100 
porcentaje <- round(porcentaje, 2)
print(glue("Los casos sin información representan el {porcentaje}%"))

# Presunto Agresor
## Mujer
ggplot(homicidios %>% filter(sexo_de_la_victima == "Mujer") %>% 
    filter(presunto_agresor_detallado != "Sin información"),
    aes(x = fct_rev(fct_infreq(presunto_agresor_detallado)), fill = presunto_agresor_detallado)) + 
    geom_bar(width = 0.5) + 
    theme_light() +
    coord_flip() +
    scale_fill_viridis(discrete = TRUE, option = "D") + 
    labs(x = "Presunto Agresor", y = "Número de Homicidios",
         caption = "Excluye: casos sin información.") +
    theme(legend.position = "none") +
    scale_x_discrete(expand = expansion(add = 0.5)) +
    ggtitle("Homicidios por Sexo (Mujer)")

ggplot(homicidios %>% filter(sexo_de_la_victima == "Mujer") %>% 
    filter(presunto_agresor_detallado != "Sin información") %>%
    filter(presunto_agresor_detallado != "Agresor desconocido"),
    aes(x = fct_rev(fct_infreq(presunto_agresor_detallado)), fill = presunto_agresor_detallado)) + 
    geom_bar(width = 0.5) + 
    theme_light() +
    coord_flip() +
    scale_fill_viridis(discrete = TRUE, option = "D") + 
    labs(x = "Presunto Agresor", y = "Número de Homicidios", 
         caption = "Excluye: casos donde el agresor es desconocido, casos sin información.") +
    theme(legend.position = "none") +
    scale_x_discrete(expand = expansion(add = 0.5)) +
    ggtitle("Presunto Agresor por Sexo (Mujer)")

## Hombre
ggplot(homicidios %>% filter(sexo_de_la_victima == "Hombre") %>% 
    filter(presunto_agresor_detallado != "Sin información"),
    aes(x = fct_rev(fct_infreq(presunto_agresor_detallado)), fill = presunto_agresor_detallado)) + 
    geom_bar(width = 0.5) + 
    theme_light() +
    coord_flip() +
    scale_fill_viridis(discrete = TRUE, option = "D") + 
    labs(x = "Presunto Agresor", y = "Número de Homicidios",
         caption = "Excluye: casos sin información.") +
    theme(legend.position = "none") +
    scale_x_discrete(expand = expansion(add = 0.5)) +
    ggtitle("Homicidios por Sexo (Hombre)")

ggplot(homicidios %>% filter(sexo_de_la_victima == "Hombre") %>% 
    filter(presunto_agresor_detallado != "Sin información") %>%
    filter(presunto_agresor_detallado != "Agresor desconocido"),
    aes(x = fct_rev(fct_infreq(presunto_agresor_detallado)), fill = presunto_agresor_detallado)) + 
    geom_bar(width = 0.5) + 
    theme_light() +
    coord_flip() +
    scale_fill_viridis(discrete = TRUE, option = "D") + 
    labs(x = "Sexo", y = "Número de Homicidios",
         caption = "Excluye: casos donde el agresor es desconocido, casos sin información.") +
    theme(legend.position = "none") +
    scale_x_discrete(expand = expansion(add = 0.5)) +
    ggtitle("Homicidios por Sexo (Hombre)")

conteo <- length(homicidios$presunto_agresor_detallado)
sin_informacion <- sum(homicidios$presunto_agresor_detallado == "Sin información",
                       homicidios$presunto_agresor_detallado == "Agresor desconocido", na.rm = TRUE)
print(c("Conteo" = conteo, "Sin Información" = sin_informacion))
porcentaje <- sin_informacion/conteo*100 
porcentaje <- round(porcentaje, 2)
print(glue("Los casos sin información representan el {porcentaje}%"))

# Mecanismo Causal por Sexo 
homicidios %>%
  filter(sexo_de_la_victima %in% c("Hombre", "Mujer")) %>%
  filter(mecanismo_causal_de_la_lesion_fatal != "Por determinar") %>%
  ggplot(aes(x = fct_rev(fct_infreq(mecanismo_causal_de_la_lesion_fatal)), 
             fill = sexo_de_la_victima)) +
  geom_bar(position = "fill") + 
  coord_flip() +
  theme_light() +
  scale_fill_viridis(discrete = TRUE, option = "D") +
  labs(title = "Mecanismo Causal por Sexo de la Víctima",
       x = "Mecanismo Causal", 
       y = "Proporción (por Sexo)",
       fill = "Sexo")

# Departamentos por Año
departamentos_mas_homicidios <- homicidios %>%
  filter(departamento_del_hecho_dane != "Sin información") %>%
  count(departamento_del_hecho_dane) %>%
  arrange(desc(n)) %>%
  slice_head(n = 5) %>%
  pull(departamento_del_hecho_dane)


homicidios_anual <- homicidios %>%
  filter(departamento_del_hecho_dane %in% departamentos_mas_homicidios) %>%
  group_by(ano_del_hecho, departamento_del_hecho_dane) %>%
  summarise(conteo_homicidios = n(), .groups = 'drop') %>%
  mutate(ano_del_hecho = as.numeric(as.character(ano_del_hecho)))

ggplot(homicidios_anual, 
       aes(x = ano_del_hecho, 
           y = conteo_homicidios, 
           group = departamento_del_hecho_dane, 
           color = departamento_del_hecho_dane)) +
  geom_line(size = 1) + 
  geom_point(size = 2) +
  geom_text(aes(label = conteo_homicidios), vjust = -0.8, size = 3, check_overlap = TRUE) + 
  theme_light() +
  scale_color_viridis(discrete = TRUE, option = "D") +
  scale_x_continuous(breaks = unique(homicidios_anual$ano_del_hecho)) +
  labs(x = "Año (2015 - 2024)", 
       y = "Número de Homicidios",
       color = "Departamento",
       caption = "Incluye unicamente los 5 departamentos con mayor conteo de homicidios.") +
  ggtitle("Homicidios Anuales por Departamento") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
