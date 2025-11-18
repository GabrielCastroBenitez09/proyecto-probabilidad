homicidios <- readRDS("datos/Homicidios_Limpios_2015_2024.rds")


# Eliminar los casos con valores faltantes para el análisis de Normalidad
edades_validas <- na.omit(homicidios$edad_numerica)

mu_est <- mean(edades_validas)
sigma_est <- sd(edades_validas)

prob_entre_30_y_40 <- pnorm(q = 40, mean = mu_est, sd = sigma_est) - pnorm(q = 30, mean = mu_est, sd = sigma_est)

hist(edades_validas, breaks = 30, freq = FALSE, 
     main = "Histograma de Edades con Curva Normal Ajustada", 
     xlab = "Edad", ylab = "Densidad")
curve(dnorm(x, mean = mu_est, sd = sigma_est), 
      add = TRUE, col = "red", lwd = 2)