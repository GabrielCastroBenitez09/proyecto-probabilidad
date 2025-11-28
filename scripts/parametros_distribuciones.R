# Paquetes
install.packages(tidyverse)
install.packages(glue)
install.packages("knitr")
install.packages("kableExtra")


# Librerias
library(tidyr)
library(dplyr)
library(forcats)
library(glue)
library(tibble)
library(knitr)
library(kableExtra)


# Base de Datos 
homicidios <- readRDS("datos/Homicidios_Limpios_2015_2024.rds")
names(homicidios)

options(scipen = 999)

# Promedio de Homicidios por año
## 2015 - 2024
homicidios_por_ano <- homicidios %>%
    group_by(ano_del_hecho) %>%
    summarise(homicidios_totales = n())

promedio_anual <- mean(homicidios_por_ano$homicidios_totales, na.rm = TRUE)

## Anterior al Covid-19
promedio_anual_pre_covid <- homicidios %>%
    filter(ano_del_hecho < 2020) %>%
    group_by(ano_del_hecho) %>%
    summarise(homicidios_totales = n()) %>%
    summarise(promedio_anual_15_20 = mean(homicidios_totales, na.rm = TRUE)) %>%
    pull(promedio_anual_15_20)

## Posterior al Covid-19
promedio_anual_post_covid <- homicidios %>%
    filter(ano_del_hecho >= 2021) %>%
    group_by(ano_del_hecho) %>%
    summarise(homicidios_totales = n()) %>%
    summarise(promedio_anual_21_24 = mean(homicidios_totales, na.rm = TRUE)) %>%
    pull(promedio_anual_21_24)

print(c("Promedio Anual (2015 - 2024)" = promedio_anual, 
        "Promedio Anual Anterior al Covid-19" = promedio_anual_pre_covid,
        "Promedio Anual Posterior al Covid-19" = promedio_anual_post_covid))

# Probabilidad Promedio Posterior Covid-19 <= Promedio Anterior Covid-19
ppois(q = promedio_anual_pre_covid, lambda = promedio_anual_post_covid)

# Homicidios Esperados Anuales
homicidios_post_covid_2_5 <- qpois(0.025, lambda = promedio_anual_post_covid)
homicidios_post_covid_97_5 <- qpois(0.975, lambda = promedio_anual_post_covid)
print(table("Límite Inferior" = homicidios_post_covid_2_5, "Límite Superior" = homicidios_post_covid_97_5))


# Mecanismo Causal
tabla_frecuencias <- table(homicidios$mecanismo_causal_de_la_lesion_fatal)
probabilidad_mecanismo <- prop.table(tabla_frecuencias)

probabilidad_df <- as.data.frame(probabilidad_mecanismo) %>%
  rename(Mecanismo = Var1, Probabilidad = Freq) %>%
  arrange(desc(Probabilidad))

probabilidades_mecanismo_5 <- head(probabilidad_df)

tabla_final <- kable(
  probabilidades_mecanismo_5,
  format = "markdown",
  caption = "Primeros 5 registros de la base de datos de homicidios")

print(tabla_final)

# Probabilidad Proyectil de Arma de Fuego
p <- probabilidad_mecanismo[["Proyectil de arma de fuego"]]

# Probabilidad 75% Homicidios Proyectild Arma de Fuego
homicidios_75_porciento <- promedio_anual*0.75
homicidios_75_porciento <- round(homicidios_75_porciento)

pbinom(q = homicidios_75_porciento, size = round(promedio_anual), prob = p)

# Probabilidad de Homicidios Arma de Fuego Bogotá
total_homicidios_bogota <- homicidios %>%
  filter(departamento_del_hecho_dane == "Bogotá D.C.") %>%
  nrow()

homicidios_af_bogota <- homicidios %>%
  filter(departamento_del_hecho_dane == "Bogotá D.C.", 
  mecanismo_causal_de_la_lesion_fatal == "Proyectil de arma de fuego") %>%
  nrow()

p_af_bogota <- homicidios_af_bogota / total_homicidios_bogota

print(paste("P(Arma Fuego | Bogotá) =", round(p_af_bogota,4)))

# Probabilidad de Homicidios Arma de Fuego Valle del Cauca
total_homicidios_valle_cauca <- homicidios %>%
  filter(departamento_del_hecho_dane == "Valle del Cauca") %>%
  nrow()

homicidios_af_valle_cauca <- homicidios %>%
  filter(departamento_del_hecho_dane == "Valle del Cauca", 
  mecanismo_causal_de_la_lesion_fatal == "Proyectil de arma de fuego") %>%
  nrow()

p_af_valle_cauca <- homicidios_af_valle_cauca / total_homicidios_valle_cauca

print(paste("P(Arma Fuego | Valle del Cauca) =", round(p_af_valle_cauca,4)))

# Probabilidad de Homicidios Arma de Fuego Antioquia
total_homicidios_antioquia <- homicidios %>%
  filter(departamento_del_hecho_dane == "Antioquia") %>%
  nrow()

homicidios_af_antioquia <- homicidios %>%
  filter(departamento_del_hecho_dane == "Antioquia", 
  mecanismo_causal_de_la_lesion_fatal == "Proyectil de arma de fuego") %>%
  nrow()

p_af_antioquia <- homicidios_af_antioquia / total_homicidios_antioquia

print(paste("P(Arma Fuego | Antioquia) =", round(p_af_antioquia,4)))

# 500 Homicidios Arma de Fuego en 1000
pbinom(q = 750, size = 1000, prob = p_af_antioquia, lower.tail = FALSE)
pbinom(q = 750, size = 1000, prob = p_af_bogota, lower.tail = FALSE)
pbinom(q = 750, size = 1000, prob = p_af_valle_cauca, lower.tail = FALSE)

# 5000 Homicidios Arma de Fuego en 10000
pbinom(q = 7500, size = 10000, prob = p_af_antioquia, lower.tail = FALSE)
pbinom(q = 7500, size = 10000, prob = p_af_bogota, lower.tail = FALSE)
pbinom(q = 7500, size = 10000, prob = p_af_valle_cauca, lower.tail = FALSE)


# Promedio de Homicidios por mes
conteo_mensual <- homicidios %>%
  group_by(mes_del_hecho, ano_del_hecho) %>%
  summarise(conteo = n(), .groups = 'drop')

promedio_homicidio_por_mes <- conteo_mensual %>%
  group_by(mes_del_hecho) %>%
  summarise(
    lambda = mean(conteo), 
    num_anios = n(),     
    total_homicidios = sum(conteo)
  ) %>%
  ungroup()

promedio_homicidio_por_mes

# Calcular la probabilidad P(X >= 1200), P(X >= 1100) y P(X >= 1000) usando ppois()
umbrales_a_probar <- c(1200, 1100, 1000)
resultados_por_umbral <- list()
for (umbral in umbrales_a_probar) {
  resultados_mensuales <- promedio_homicidio_por_mes_limpio %>%
    rowwise() %>% 
    mutate(
      umbral_k = umbral, 
      prob_exceso = ppois(q = umbral, lambda = lambda, lower.tail = FALSE)
    ) %>%
    ungroup() %>%
    mutate(
      prob_exceso_decimal = format(
        prob_exceso, 
        scientific = FALSE, 
        trim = TRUE
      )
    ) %>%
    select(mes_del_hecho, lambda, umbral_k, prob_exceso_decimal) %>%
    arrange(desc(lambda))
  resultados_por_umbral[[as.character(umbral)]] <- resultados_mensuales
}

tabla_final_comparativa <- bind_rows(resultados_por_umbral)
print(n = 36, tabla_final_comparativa)


# Ajueste de Distribución Normal Grupo de Edad Quinquenal
edades_validas <- na.omit(homicidios$edad_numerica)

mu_est <- mean(edades_validas)
sigma_est <- sd(edades_validas)
mu_est
sigma_est
prob_entre_30_y_40 <- pnorm(q = 40, mean = mu_est, sd = sigma_est) - pnorm(q = 30, mean = mu_est, sd = sigma_est)

hist(edades_validas, breaks = 30, freq = FALSE, 
     main = "Edades con Curva Normal Ajustada", 
     xlab = "Edad", ylab = "Densidad")
curve(dnorm(x, mean = mu_est, sd = sigma_est), 
      add = TRUE, col = "red", lwd = 2)


options(scipen = 0)
